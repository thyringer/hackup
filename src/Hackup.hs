
module Hackup (
	(<:),
	(</),
	(</>),
	(<<),
	Attribute(..),
	Attributing,
	ElemConstruction,
	Marking,
	Markup(..),
	Nesting,
	Renderable,
	Rendering,
	VoidElemConstruction,
	fit,
	newAttr,
	newBooleanAttr,
	newElem,
	newVoidElem,
	put,
	render
) where

import qualified Data.Map.Strict as Map hiding (Map)
import Data.Map.Strict (Map)
import qualified Data.String
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import TextShow (TextShow, showt)


newtype Attribute lang = Attribute (Text, Text)

data Markup lang = Markup Text | Elem {
	symb :: Text,
	attr :: Map Text Text,
	cont :: Maybe [Markup lang]
} deriving (Show)

put = Markup


--

class TextShow a => Renderable a lang where
	fit :: a -> Markup lang
	fit = Markup . showt

instance Renderable Int lang

instance Renderable Integer lang

instance Renderable Float lang

instance Renderable Double lang

instance Renderable Bool lang where
	fit True = Markup "true"
	fit False = Markup "false"

instance Renderable String lang

instance Renderable Text lang


--

class Rendering input where
	render :: input -> Text

instance Rendering (Markup lang) where
	render markup = case markup of
		Markup txt -> txt
		elem@Elem {cont = Nothing} -> renderTag elem
		elem@Elem {cont = Just cont} -> renderTag elem <> foldr merge "" cont <>  "</" <> elem.symb <> ">"
		where
			renderAttr name val res = " " <> name <> if val == "" then "" else "=\"" <> val <>"\"" <> res
			renderTag elem = "<" <> elem.symb <> Map.foldrWithKey renderAttr "" elem.attr <> ">"
			merge markup res = render markup <> res


instance Rendering (Markup lang) => Rendering [Markup lang] where
	render = foldr (\markup res -> render markup <> res) ""


--

type ElemConstruction lang = [Attribute lang] -> [Markup lang] -> Markup lang

type VoidElemConstruction lang = [Attribute lang] -> Markup lang


newElem :: forall {k} {lang :: k}. Text -> ElemConstruction lang
newElem symb attr cont
	= Elem symb (Map.fromList [a | Attribute a <- attr]) (Just cont)

newVoidElem :: forall {k} {lang :: k}. Text -> VoidElemConstruction lang
newVoidElem symb attr
	= Elem symb (Map.fromList [a | Attribute a <- attr]) Nothing

newAttr :: forall {k} {lang :: k}. Text -> Text -> Attribute lang
newAttr symb val = Attribute (symb, val)

newBooleanAttr :: forall {k} {lang :: k}. Text -> Attribute lang
newBooleanAttr symb = Attribute (symb, "")


--

{-
class Identifying elem lang | elem -> lang where
	(#) :: elem -> Text -> Markup lang
-}

class Attributing elem attr lang | elem -> lang, attr -> lang where
	(<:) :: elem -> attr -> Markup lang


instance Attributing (Markup lang) (Attribute lang) lang where
	(<:) elem (Attribute (n, v)) = elem { attr = Map.insert n v elem.attr }

instance Attributing (Markup lang) [Attribute lang] lang where
	(<:) elem attr = elem { attr = Map.fromList [a | Attribute a <- attr] <> elem.attr }


instance Attributing ([Markup lang] -> Markup lang) (Attribute lang) lang where
	(<:) incompleted (Attribute (n, v)) = let elem = incompleted [] in elem {
		attr = Map.insert n v elem.attr
	}

instance Attributing ([Markup lang] -> Markup lang) [Attribute lang] lang where
	(<:) incompleted attr = let elem = incompleted [] in elem {
		attr = Map.fromList [a | Attribute a <- attr] <> elem.attr
	}


--

class Nesting elem cont lang | elem -> lang, cont -> lang where
	(</) :: elem -> cont -> Markup lang

infixl 8 </


_msg_void_elem = "Attempt to nest a void element"

instance Nesting (Markup lang) (Markup lang) lang where
	(</) elem additional_cont = case elem.cont of
		Just cont -> elem { cont = Just (cont ++ [additional_cont]) }
		Nothing -> error _msg_void_elem

instance Nesting (Markup lang) [Markup lang] lang where
	(</) elem additional_cont = case elem.cont of
		Just cont -> elem { cont = Just (cont ++ additional_cont) }
		Nothing -> error _msg_void_elem


instance Nesting ([Markup lang] -> Markup lang) (Markup lang) lang where
	(</) ctor additional_cont = ctor [additional_cont]

instance Nesting ([Markup lang] -> Markup lang) [Markup lang] lang where
	(</) ctor = ctor

instance Nesting (Markup lang) ([Markup lang] -> Markup lang) lang where
	(</) elem cont_ctor = let additional_cont = cont_ctor [] in case elem.cont of
		Just cont -> elem { cont = Just (cont ++ [additional_cont]) }
		Nothing -> error _msg_void_elem

instance Nesting ([Markup lang] -> Markup lang) ([Markup lang] -> Markup lang) lang where
	(</) ctor cont_ctor = ctor [cont_ctor []]


instance Nesting (ElemConstruction lang) (Markup lang) lang where
	(</) ctor additional_cont = ctor [] [additional_cont]

instance Nesting (ElemConstruction lang) [Markup lang] lang where
	(</) ctor = ctor []


instance Nesting (Markup lang) (ElemConstruction lang) lang where
	(</) elem cont_ctor = case elem.cont of
		Just cont -> elem { cont = Just (cont ++ [cont_ctor [] []]) }
		Nothing -> error _msg_void_elem

instance Nesting (ElemConstruction lang) (ElemConstruction lang) lang where
	(</) ctor cont_ctor = ctor [] [cont_ctor [] []]


instance Nesting (ElemConstruction lang) (VoidElemConstruction lang) lang where
	(</) ctor cont_ctor = ctor [] [cont_ctor []]


--

class Labeling elem lang | elem -> lang where
	(<<) :: elem -> Text -> Markup lang


instance Labeling (Markup lang) lang where
	(<<) elem additional_cont = case elem.cont of
		Just cont -> elem { cont = Just (cont ++ [Markup additional_cont]) }
		Nothing -> error _msg_void_elem

instance Labeling ([Markup lang] -> Markup lang) lang where
	(<<) ctor cont = ctor [Markup cont]

instance Labeling (ElemConstruction lang) lang where
	(<<) ctor cont = ctor [] [Markup cont]

--

class Marking elem1 elem2 lang | elem1 -> lang, elem2 -> lang where
	(</>) :: elem1 -> elem2 -> [Markup lang]

infixl 8 </>


instance Marking (Markup lang) (Markup lang) lang where
	(</>) elem1 elem2 = [elem1, elem2]

instance Marking ([Markup lang] -> Markup lang) (Markup lang) lang where
	(</>) elem1 elem2 = [elem1 [], elem2]

instance Marking (Markup lang) ([Markup lang] -> Markup lang) lang where
	(</>) elem1 elem2 = [elem1, elem2 []]

instance Marking ([Markup lang] -> Markup lang) ([Markup lang] -> Markup lang) lang where
	(</>) elem1 elem2 = [elem1 [], elem2 []]


instance Marking (ElemConstruction lang) (Markup lang) lang where
	(</>) ctor1 elem2 = [ctor1 [] [], elem2]

instance Marking (VoidElemConstruction lang) (Markup lang) lang where
	(</>) ctor1 elem2 = [ctor1 [], elem2]

instance Marking (Markup lang) (ElemConstruction lang) lang where
	(</>) elem1 ctor2 = [elem1, ctor2 [] []]

instance Marking (Markup lang) (VoidElemConstruction lang) lang where
	(</>) elem1 ctor2 = [elem1, ctor2 []]


instance Marking (ElemConstruction lang) ([Markup lang] -> Markup lang) lang where
	(</>) ctor1 ctor2 = [ctor1 [] [], ctor2 []]

instance Marking (VoidElemConstruction lang) ([Markup lang] -> Markup lang) lang where
	(</>) ctor1 ctor2 = [ctor1 [], ctor2 []]

instance Marking ([Markup lang] -> Markup lang) (ElemConstruction lang) lang where
	(</>) ctor1 ctor2 = [ctor1 [], ctor2 [] []]

instance Marking ([Markup lang] -> Markup lang) (VoidElemConstruction lang) lang where
	(</>) ctor1 ctor2 = [ctor1 [], ctor2 []]


instance Marking (ElemConstruction lang) (ElemConstruction lang) lang where
	(</>) ctor1 ctor2 = [ctor1 [] [], ctor2 [] []]

instance Marking (ElemConstruction lang) (VoidElemConstruction lang) lang where
	(</>) ctor1 ctor2 = [ctor1 [] [], ctor2 []]

instance Marking (VoidElemConstruction lang) (ElemConstruction lang) lang where
	(</>) ctor1 ctor2 = [ctor1 [], ctor2 [] []]

instance Marking (VoidElemConstruction lang) (VoidElemConstruction lang) lang where
	(</>) ctor1 ctor2 = [ctor1 [], ctor2 []]
