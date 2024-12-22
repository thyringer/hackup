
module Main (main) where

import qualified Data.Map.Strict as Map hiding (Map)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import TextShow (TextShow, showt)

import Hackup
import Hackup.Html
import qualified Hackup.Html.Attribute
import Hackup.Html.Attribute (
		charset',
		class',
		href',
		id',
		lang',
		name',
		rel',
		src'
	)


homepageHead = head' [lang' "de"] [
		meta' [charset' "utf-8"],
		title' [] [put "Hauptstädte der Welt"],
		link' [rel' "stylesheet", href' ".styles/main.css"],
		script' [src' ".scripts/controls.js"] []
	]

homepageNav = nav' 
	</ a' [href' "http://städte-der-welt.de/index.html"] << "Hauptseite" -- or: a' […] [put "Hauptseite"]
	</ a' [href' "http://städte-der-welt.de/contact.html"] << "Impressum"

content1 = main' </ [
		h1' << "Deutsche Millionenstädte",
		ul' [id' "cities"]
			</ li' << "Berlin"
			</ li' << "Hamburg"
			</ li' << "Köln"
			</ li' << "München"
	]

content2 = main' [] [
		h1' << "Deutsche Millionenstädte",
		ul' [id' "cities"] [
				li' << "Berlin",
				li' << "Hamburg",
				li' << "Köln",
				li' << "München"
			],
		p' << "Deutschland weist nur 4 Städte mit mehr als 4 Millionen Einwohnern vor."
	]


homepage = doctype </> html' [lang' "de"] [homepageHead, body' </ (div' </ homepageNav </ content2)]


main :: IO ()
main = do
	print $ render homepage
