# Hackup

Hackup is a Haskell library designed to generate HTML markup in a declarative and compositional style. It provides a set of combinators to construct HTML elements with attributes and nestings, and render them as HTML text.

## Features

- **Declarative HTML generation**: Using combinators such as </>, <:, <<, you can build HTML tags, add attributes, and nest elements.
- **Type-safe**: The library leverages Haskell’s type system to ensure correctness when building HTML elements.

## Usage

Here is an example of how to use Hackup to generate HTML for a simple page:

```haskell
module Main (main) where

import Hackup
import Hackup.Html
import Hackup.Html.Attribute (charset', class', href', id', lang', name', rel', src')

homepageHead = head' [lang' "de"] [
    meta' [charset' "utf-8"],
    title' [] [put "Hauptstädte der Welt"],
    link' [rel' "stylesheet", href' ".styles/main.css"],
    script' [src' ".scripts/controls.js"] []
]

homepageNav = nav' [
    a' [href' "http://städte-der-welt.de/index.html"] << "Hauptseite",
    a' [href' "http://städte-der-welt.de/contact.html"] << "Impressum"
]

content1 = main' [] [
    h1' << "Deutsche Millionenstädte",
    ul' [id' "cities"] [
        li' << "Berlin",
        li' << "Hamburg",
        li' << "Köln",
        li' << "München"
    ]
]

homepage = doctype </> html' [lang' "de"] [homepageHead, body' </ (div' </ homepageNav </ content1)]

main :: IO ()
main = do
    print $ render homepage
```

The above example will generate the following HTML:

```html
<!DOCTYPE html>
<html lang="de">
    <head>
        <meta charset="utf-8">
        <title>Hauptstädte der Welt</title>
        <link rel="stylesheet" href=".styles/main.css">
        <script src=".scripts/controls.js"></script>
    </head>
    <body>
        <div>
            <nav>
                <a href="http://städte-der-welt.de/index.html">Hauptseite</a>
                <a href="http://städte-der-welt.de/contact.html">Impressum</a>
            </nav>
            <main>
                <h1>Deutsche Millionenstädte</h1>
                <ul id="cities">
                    <li>Berlin</li>
                    <li>Hamburg</li>
                    <li>Köln</li>
                    <li>München</li>
                </ul>
            </main>
        </div>
    </body>
</html>
```

## HTML Combinators

| Operator | Description                                       |
|----------|---------------------------------------------------|
| `<:`     | Add an attribute to an element.                  |
| `<<`     | Add text content to an element.                  |
| `</`     | Nest one element inside another.                 |
| `</>`    | Merge two elements together.                     |
| `render` | Render the generated markup as a `Text` object.  |

## License

This project is licensed under the [Unlicense](https://unlicense.org/).  

You are free to use, modify, and distribute this code without restriction.
