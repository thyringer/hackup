
module Hackup.Html where

import qualified Data.Map.Strict as Map hiding (Map)
import Data.Map.Strict ((!), Map)
import Data.Text (Text)
import TextShow

import Hackup


data Html

{-
instance {-# OVERLAPS #-} Nesting (Markup Html) Html where
	(<:) tag attr
		| Left ("class", new) <- attr, Map.member key tag.attr = let
				val = toDyn (fromDyn (tag.attr ! key) ([] :: [Text]) ++ fromDyn (new) ([] :: [Text]))
			in
				tag { attr = Map.insert key val tag.attr }
		| otherwise = tag { attr = Map.insert (fst attr) (snd attr) tag.attr }
		where
			key = "class"
-}


doctype :: Markup Html
doctype = Markup "<!DOCTYPE html>"


newHtmlElem :: Text -> ElemConstruction Html
newHtmlElem = newElem

newVoidHtmlElem :: Text -> VoidElemConstruction Html
newVoidHtmlElem = newVoidElem

a' = newHtmlElem "a"

abbr' = newHtmlElem "abbr"

address' = newHtmlElem "address"

area' = newHtmlElem "area"

article' = newHtmlElem "article"

aside' = newHtmlElem "aside"

audio' = newHtmlElem "audio"

b' = newHtmlElem "b"

base' = newVoidHtmlElem "base" 

bdi' = newHtmlElem "bdi"

bdo' = newHtmlElem "bdo"

blockquote' = newHtmlElem "blockquote"

body' = newHtmlElem "body"

br' = newVoidHtmlElem "br" 

button' = newHtmlElem "button"

canvas' = newHtmlElem "canvas"

caption' = newHtmlElem "caption"
capt' =caption'

cite' = newHtmlElem "cite"

code' = newHtmlElem "code"

col' = newVoidElem "col"

colgroup' = newHtmlElem "colgroup"

data' = newHtmlElem "data"

datalist' = newHtmlElem "datalist"

dd' = newHtmlElem "dd"

del' = newHtmlElem "del"

details' = newHtmlElem "details"

dfn' = newHtmlElem "dfn"

dialog' = newHtmlElem "dialog"

div' = newHtmlElem "div"

dl' = newHtmlElem "dl"

dt' = newHtmlElem "dt"

em' = newHtmlElem "em"

embed' = newVoidHtmlElem "embed"

fieldset' = newHtmlElem "fieldset"

figcaption' = newHtmlElem "figcaption"
figcapt' = figcaption'

figure' = newHtmlElem "figure"
fig' =figure'

footer' = newHtmlElem "footer"

form' = newHtmlElem "form"

h1' = newHtmlElem "h1"

h2' = newHtmlElem "h2"

h3' = newHtmlElem "h3"

h4' = newHtmlElem "h4"

h5' = newHtmlElem "h5"

h6' = newHtmlElem "h6"

head' = newHtmlElem "head"

header' = newHtmlElem "header"

hgroup' = newHtmlElem "hgroup"

hr' = newVoidHtmlElem "hr" 

html' = newHtmlElem "html"

i' = newHtmlElem "i"

iframe' = newHtmlElem "iframe"

img' = newVoidHtmlElem "img"

input' = newVoidHtmlElem "input"

ins' = newHtmlElem "ins"

kbd' = newHtmlElem "kbd"

label' = newHtmlElem "label"

legend' = newHtmlElem "legend"

li' = newHtmlElem "li"

link' = newVoidHtmlElem "link"

main' = newHtmlElem "main"

map' = newHtmlElem "map"

mark' = newHtmlElem "mark"

math' = newHtmlElem "math"

menu' = newHtmlElem "menu"

menuitem' = newHtmlElem "menuitem"

meta' = newVoidHtmlElem "meta" 

meter' = newHtmlElem "meter"

nav' = newHtmlElem "nav"

noscript' = newHtmlElem "noscript"

object' = newHtmlElem "object"

ol' = newHtmlElem "ol"

optgroup' = newHtmlElem "optgroup"

option' = newHtmlElem "option"

output' = newHtmlElem "output"

p' = newHtmlElem "p"

param' = newVoidHtmlElem "param"

picture' = newHtmlElem "picture"

pre' = newHtmlElem "pre"

progress' = newHtmlElem "progress"

q' = newHtmlElem "q"

rb' = newHtmlElem "rb"

rp' = newHtmlElem "rp"

rt' = newHtmlElem "rt"

rtc' = newHtmlElem "rtc"

ruby' = newHtmlElem "ruby"

s' = newHtmlElem "s"

samp' = newHtmlElem "samp"

script' = newHtmlElem "script"

section' = newHtmlElem "section"
sect' =section'

select' = newHtmlElem "select"

slot' = newHtmlElem "slot"

small' = newHtmlElem "small"

source' = newVoidHtmlElem "source"

span' = newHtmlElem "span"

strong' = newHtmlElem "strong"

style' = newHtmlElem "style"

sub' = newHtmlElem "sub"

summary' = newHtmlElem "summary"

sup' = newHtmlElem "sup"

svg' = newHtmlElem "svg"

table' = newHtmlElem "table"

tbody' = newHtmlElem "tbody"

td' = newHtmlElem "td"

template' = newHtmlElem "template"

textarea' = newHtmlElem "textarea"

tfoot' = newHtmlElem "tfoot"

th' = newHtmlElem "th"

thead' = newHtmlElem "thead"

time' = newHtmlElem "time"

title' = newHtmlElem "title"

tr' = newHtmlElem "tr"

track' = newVoidHtmlElem "track"

u' = newHtmlElem "u"

ul' = newHtmlElem "ul"

var' = newHtmlElem "var"

video' = newHtmlElem "video"

wbr' = newVoidHtmlElem "wbr"
