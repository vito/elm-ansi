# elm-ansi

ANSI text stream handling for Elm.

## Usage

### Parsing a string of ANSI console output

The `Ansi.parse` function returns a list of `Ansi.Action` types which can be
used for interpreting escape sequences:

```elm
> Ansi.parse "\u{001b}[1;32mhello\u{001b}[0m"
[SetBold True,SetForeground (Just Green),Print "hello",SetForeground Nothing,SetBackground Nothing,SetBold False,SetFaint False,SetItalic False,SetUnderline False,SetBlink False,SetInverted False,SetFraktur False,SetFramed False]
    : List Ansi.Action
```

The `Ansi.parseInto` function calls a helper function while parsing to avoid
the intermediate data structure.

### Rendering ANSI console output as HTML

The `Ansi.Log` module provides a component for incrementally parsing `String`
chunks and maintaining a `Model` that can be rendered as HTML.

It exports the usual `init`, `update`, `view` along with `Model` and other
types to assist with rendering, should you want to bring your own `view`
function. A `viewLine` function is also exported should you want to re-use that
aspect within your own view.
