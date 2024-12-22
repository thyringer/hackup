{-

The following attributes are equivalent to HTML tags, so they cannot be used directly:
- cite'
- code'
- data'
- form'
- label'
- slot'
- span'
- style'
- title'

-}

module Hackup.Html.Attribute where

import qualified Data.Map.Strict as Map hiding (Map)
import Data.Map.Strict ((!), Map)
import Data.Text (Text)
import TextShow

import Hackup
import Hackup.Html


{-
instance Identifying (Markup Html) Html where
	(#) elem val = elem { attr = Map.insert "id" val elem.attr }

instance Identifying ([Markup Html] -> Markup Html) Html where
	(#) ctor val = let elem = ctor [] in elem { attr = Map.insert "id" val elem.attr }

instance Identifying (ElemConstruction Html) Html where
	(#) ctor val = ctor [id' val] []

instance Identifying (VoidElemConstruction Html) Html where
	(#) ctor val = ctor [id' val]
-}


newHtmlAttr :: Text -> Text -> Attribute Html
newHtmlAttr = newAttr

newBooleanHtmlAttr :: Text -> Attribute Html
newBooleanHtmlAttr = newBooleanAttr


accept' = newHtmlAttr "accept"

accept_charset' = newHtmlAttr "accept-charset"

accesskey' = newHtmlAttr "accesskey"

action' = newHtmlAttr "action"

allow' = newHtmlAttr "allow"

allowfullscreen' = newBooleanHtmlAttr "allowfullscreen"

alt' = newHtmlAttr "alt"

async' = newBooleanHtmlAttr "async"

autocapitalize' = newHtmlAttr "autocapitalize"

autocomplete' = newHtmlAttr "autocomplete"

autofocus' = newBooleanHtmlAttr "autofocus"

autoplay' = newBooleanHtmlAttr "autoplay"

background' = newHtmlAttr "background"

buffered' = newHtmlAttr "buffered"

capture' = newHtmlAttr "capture"

challenge' = newHtmlAttr "challenge"

charset' = newHtmlAttr "charset"

checked' = newBooleanHtmlAttr "checked"

cite' = newHtmlAttr "cite"

class' = newHtmlAttr "class"

code' = newHtmlAttr "code"

codebase' = newHtmlAttr "codebase"

cols' = newHtmlAttr "cols"

colspan' = newHtmlAttr "colspan"

content' = newHtmlAttr "content"

contenteditable' = newHtmlAttr "contenteditable"

contextmenu' = newHtmlAttr "contextmenu"

controls' = newBooleanHtmlAttr "controls"

coords' = newHtmlAttr "coords"

crossorigin' = newHtmlAttr "crossorigin"

csp' = newHtmlAttr "csp"

data' = newHtmlAttr "data"

datetime' = newHtmlAttr "datetime"

decoding' = newHtmlAttr "decoding"

default' = newBooleanHtmlAttr "default"

defer' = newBooleanHtmlAttr "defer"

dir' = newHtmlAttr "dir"

dirname' = newHtmlAttr "dirname"

disabled' = newBooleanHtmlAttr "disabled"

download' = newHtmlAttr "download"

draggable' = newHtmlAttr "draggable"

enctype' = newHtmlAttr "enctype"

enterkeyhint' = newHtmlAttr "enterkeyhint"

for' = newHtmlAttr "for"

form' = newHtmlAttr "form"

formaction' = newHtmlAttr "formaction"

formenctype' = newHtmlAttr "formenctype"

formmethod' = newHtmlAttr "formmethod"

formnovalidate' = newBooleanHtmlAttr "formnovalidate"

formtarget' = newHtmlAttr "formtarget"

headers' = newHtmlAttr "headers"

height' = newHtmlAttr "height"

hidden' = newHtmlAttr ""

high' = newHtmlAttr "high"

href' = newHtmlAttr "href"

hreflang' = newHtmlAttr "hreflang"

http_equiv' = newHtmlAttr "http-equiv"

icon' = newHtmlAttr "icon"

id' = newHtmlAttr "id"

integrity' = newHtmlAttr "integrity"

inputmode' = newHtmlAttr "inputmode"

ismap' = newBooleanHtmlAttr "ismap"

itemprop' = newHtmlAttr "itemprop"

itemscope' = newBooleanHtmlAttr "itemscope"

keytype' = newHtmlAttr "keytype"

kind' = newHtmlAttr "kind"

label' = newHtmlAttr "label"

lang' = newHtmlAttr "lang"

loading' = newHtmlAttr "loading"

list' = newHtmlAttr "list"

loop' = newBooleanHtmlAttr "loop"

low' = newHtmlAttr "low"

max' = newHtmlAttr "max"

maxlength' = newHtmlAttr "maxlength"

minlength' = newHtmlAttr "minlength"

media' = newHtmlAttr "media"

method' = newHtmlAttr "method"

min' = newHtmlAttr "min"

multiple' = newBooleanHtmlAttr "multiple"

muted' = newBooleanHtmlAttr "muted"

name' = newHtmlAttr "name"

novalidate' = newBooleanHtmlAttr "novalidate"

open' = newBooleanHtmlAttr "open"

optimum' = newHtmlAttr "optimum"

pattern' = newHtmlAttr "pattern"

ping' = newHtmlAttr "ping"

placeholder' = newHtmlAttr "placeholder"

playsinline' = newBooleanHtmlAttr "playsinline"

poster' = newHtmlAttr "poster"

preload' = newHtmlAttr "preload"

radiogroup' = newHtmlAttr "radiogroup"

readonly' = newBooleanHtmlAttr "readonly"

referrerpolicy' = newHtmlAttr "referrerpolicy"

rel' = newHtmlAttr "rel"

required' = newBooleanHtmlAttr "required"

reversed' = newBooleanHtmlAttr "reversed"

role' = newHtmlAttr "role"

rows' = newHtmlAttr "rows"

rowspan' = newHtmlAttr "rowspan"

sandbox' = newHtmlAttr "sandbox"

scope' = newHtmlAttr "scope"

selected' = newBooleanHtmlAttr "selected"

shape' = newHtmlAttr "shape"

size' = newHtmlAttr "size"

sizes' = newHtmlAttr "sizes"

slot' = newHtmlAttr "slot"

span' = newHtmlAttr "span"

spellcheck' = newHtmlAttr "spellcheck"

src' = newHtmlAttr "src"

srcdoc' = newHtmlAttr "srcdoc"

srclang' = newHtmlAttr "srclang"

srcset' = newHtmlAttr "srcset"

start' = newHtmlAttr "start"

step' = newHtmlAttr "step"

style' = newHtmlAttr "style"

tabindex' = newHtmlAttr "tabindex"

target' = newHtmlAttr "target"

title' = newHtmlAttr "title"

translate' = newHtmlAttr "translate"

type' = newHtmlAttr "type"

usemap' = newHtmlAttr "usemap"

value' = newHtmlAttr "value"

width' = newHtmlAttr "width"

wrap' = newHtmlAttr "wrap"
