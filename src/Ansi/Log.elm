module Ansi.Log exposing
    ( init, update, view, viewLine
    , Model, LineDiscipline(..), Line, Chunk, CursorPosition, Style
    )

{-| Log interprets a stream of text and ANSI escape codes.

@docs init, update, view, viewLine

@docs Model, LineDiscipline, Line, Chunk, CursorPosition, Style

-}

import Ansi
import Array exposing (Array)
import Html
import Html.Attributes
import Html.Lazy
import String
import UnicodeWidth exposing (stringWidth)


{-| Model is populated by parsing ANSI character sequences and escape codes
via `update`.

  - `lines` contains all of the output that's been parsed
  - `position` is the current position of the cursor
  - `style` is the style to be applied to any text that's printed
  - `remainder` is a partial ANSI escape sequence left around from an incomplete
    segment from the stream
  - `currentLinkParams` contains optional hyperlink parameters
  - `currentLinkUrl` is the URL of any active hyperlink

-}
type alias Model =
    { lineDiscipline : LineDiscipline
    , lines : Array Line
    , position : CursorPosition
    , savedPosition : Maybe CursorPosition
    , style : Style
    , remainder : String
    , currentLinkParams : List String
    , currentLinkUrl : Maybe String
    }


{-| A list of arbitrarily-sized chunks of output.
-}
type alias Line =
    ( List Chunk, Int )


{-| A blob of text paired with the style that was configured at the time.
Cached width stores the precomputed display width to avoid recalculation.
-}
type alias Chunk =
    { text : String
    , displayWidth : Int
    , style : Style
    , linkParams : List String
    , linkUrl : Maybe String
    }


{-| The current presentation state for any text that's printed.
-}
type alias Style =
    { foreground : Maybe Ansi.Color
    , background : Maybe Ansi.Color
    , bold : Bool
    , faint : Bool
    , italic : Bool
    , underline : Bool
    , blink : Bool
    , inverted : Bool
    , strikethrough : Bool
    , fraktur : Bool
    , framed : Bool
    }


{-| The coordinate in the window where text will be printed.
-}
type alias CursorPosition =
    { row : Int
    , column : Int
    }


{-| How to interpret linebreaks.

  - `Raw`: interpret `\n` as just `\n`, i.e. move down a line, retaining the
    cursor column
  - `Cooked`: interpret `\n` as `\r\n`, i.e. move down a line and go to the first
    column

-}
type LineDiscipline
    = Raw
    | Cooked


{-| Construct an empty model.
-}
init : LineDiscipline -> Model
init ldisc =
    { lineDiscipline = ldisc
    , lines = Array.empty
    , position = { row = 0, column = 0 }
    , savedPosition = Nothing
    , style =
        { foreground = Nothing
        , background = Nothing
        , bold = False
        , faint = False
        , italic = False
        , underline = False
        , blink = False
        , inverted = False
        , strikethrough = False
        , fraktur = False
        , framed = False
        }
    , remainder = ""
    , currentLinkParams = []
    , currentLinkUrl = Nothing
    }


{-| Parse and interpret a chunk of ANSI output.

Trailing partial ANSI escape codes will be prepended to the chunk in the next
call to `update`.

-}
update : String -> Model -> Model
update str model =
    Ansi.parseInto
        { model | remainder = "" }
        handleAction
        (model.remainder ++ str)


handleAction : Ansi.Action -> Model -> Model
handleAction action model =
    case action of
        Ansi.Print s width ->
            let
                chunk =
                    { text = s
                    , displayWidth = width
                    , style = model.style
                    , linkParams = model.currentLinkParams
                    , linkUrl = model.currentLinkUrl
                    }

                updatedChunk =
                    writeChunk model.position.column chunk
            in
            { model
                | lines = updateLine model.position.row updatedChunk model.lines
                , position = { row = model.position.row, column = model.position.column + width }
            }

        Ansi.CarriageReturn ->
            { model | position = CursorPosition model.position.row 0 }

        Ansi.Linebreak ->
            handleAction (Ansi.Print "" 0) <|
                case model.lineDiscipline of
                    Raw ->
                        { model | position = { row = model.position.row + 1, column = model.position.column } }

                    Cooked ->
                        { model | position = CursorPosition (model.position.row + 1) 0 }

        Ansi.Remainder s ->
            { model | remainder = s }

        Ansi.CursorUp num ->
            { model | position = { row = model.position.row - num, column = model.position.column } }

        Ansi.CursorDown num ->
            { model | position = { row = model.position.row + num, column = model.position.column } }

        Ansi.CursorForward num ->
            { model | position = { row = model.position.row, column = model.position.column + num } }

        Ansi.CursorBackward num ->
            { model | position = { row = model.position.row, column = model.position.column - num } }

        Ansi.CursorPosition row col ->
            { model | position = CursorPosition (row - 1) (col - 1) }

        Ansi.CursorColumn col ->
            { model | position = CursorPosition model.position.row col }

        Ansi.SaveCursorPosition ->
            { model | savedPosition = Just model.position }

        Ansi.RestoreCursorPosition ->
            { model | position = Maybe.withDefault model.position model.savedPosition }

        Ansi.EraseLine mode ->
            case mode of
                Ansi.EraseToBeginning ->
                    let
                        chunk =
                            { text = String.repeat model.position.column " "
                            , displayWidth = model.position.column
                            , style = model.style
                            , linkParams = []
                            , linkUrl = Nothing
                            }

                        updatedChunk =
                            writeChunk 0 chunk
                    in
                    { model | lines = updateLine model.position.row updatedChunk model.lines }

                Ansi.EraseToEnd ->
                    let
                        updater =
                            takeLeft model.position.column
                    in
                    { model | lines = updateLine model.position.row updater model.lines }

                Ansi.EraseAll ->
                    { model | lines = updateLine model.position.row (always blankLine) model.lines }

        Ansi.HyperlinkStart params url ->
            { model
              | currentLinkParams = params
              , currentLinkUrl = Just url
            }

        Ansi.HyperlinkEnd ->
            { model
              | currentLinkParams = []
              , currentLinkUrl = Nothing
            }

        _ ->
            { model | style = updateStyle action model.style }


updateLine : Int -> (Line -> Line) -> Array Line -> Array Line
updateLine row updater lines =
    let
        currentLines =
            Array.length lines

        line =
            updater <| Maybe.withDefault blankLine (Array.get row lines)
    in
    if row + 1 > currentLines then
        appendLine (row - currentLines) line lines

    else
        Array.set row line lines


appendLine : Int -> Line -> Array Line -> Array Line
appendLine after line lines =
    if after == 0 then
        Array.push line lines

    else
        appendLine (after - 1) line (Array.push blankLine lines)


updateStyle : Ansi.Action -> Style -> Style
updateStyle action style =
    case action of
        Ansi.SetForeground mc ->
            { style | foreground = mc }

        Ansi.SetBackground mc ->
            { style | background = mc }

        Ansi.SetInverted b ->
            { style | inverted = b }

        Ansi.SetBold b ->
            { style | bold = b }

        Ansi.SetFaint b ->
            { style | faint = b }

        Ansi.SetItalic b ->
            { style | italic = b }

        Ansi.SetUnderline b ->
            { style | underline = b }

        Ansi.SetBlink b ->
            { style | blink = b }

        Ansi.SetStrikethrough b ->
            { style | strikethrough = b }

        Ansi.SetFraktur b ->
            { style | fraktur = b }

        Ansi.SetFramed b ->
            { style | framed = b }

        _ ->
            style


lineLen : Line -> Int
lineLen =
    Tuple.second


stringDisplayWidth : String -> Int
stringDisplayWidth str =
    stringWidth str


writeChunk : Int -> Chunk -> Line -> Line
writeChunk pos chunk line =
    let
        len =
            lineLen line
    in
    if len == pos then
        addChunk chunk line

    else if pos > len then
        addChunk chunk <| addChunk (spacing chunk.style chunk.linkParams chunk.linkUrl (pos - len)) line

    else
        let
            appended =
                addChunk chunk (dropRight (len - pos) line)

            afterLen =
                len - lineLen appended
        in
        if afterLen > 0 then
            List.foldl addChunk appended (Tuple.first <| takeRight afterLen line)

        else
            appended


addChunk : Chunk -> Line -> Line
addChunk chunk line =
    if chunk.displayWidth == 0 then
        line

    else
        case line of
            ( [], _ ) ->
                ( [ chunk ], chunk.displayWidth )

            ( c :: cs, llen ) ->
                if c.style == chunk.style && c.linkUrl == chunk.linkUrl && c.linkParams == chunk.linkParams then
                    ( { c | text = c.text ++ chunk.text, displayWidth = c.displayWidth + chunk.displayWidth } :: cs, llen + chunk.displayWidth )

                else
                    ( chunk :: c :: cs, llen + chunk.displayWidth )


dropRight : Int -> Line -> Line
dropRight n line =
    case line of
        ( [], _ ) ->
            line

        ( c :: cs, llen ) ->
            if c.displayWidth <= n then
                dropRight (n - c.displayWidth) ( cs, llen - c.displayWidth )

            else
                let
                    result = dropRightFromText n c.text
                    actualDropped = c.displayWidth - result.width
                in
                ( { c | text = result.text, displayWidth = result.width } :: cs, llen - actualDropped )


{-| Drop n display-width units from the right of text, preserving character boundaries.
Returns the remaining text and its display width.
-}
dropRightFromText : Int -> String -> { text : String, width : Int }
dropRightFromText widthToDrop text =
    let
        chars = String.toList text
        totalWidth = stringDisplayWidth text
        targetWidth = totalWidth - widthToDrop
    in
    -- Build text from left until we reach target width
    takeLeftWidth targetWidth chars 0 []


{-| Take characters from left until we accumulate targetWidth display units.
Handles multi-width characters by never splitting them.
-}
takeLeftWidth : Int -> List Char -> Int -> List Char -> { text : String, width : Int }
takeLeftWidth targetWidth chars accWidth accChars =
    case chars of
        [] ->
            { text = String.fromList (List.reverse accChars)
            , width = accWidth
            }

        c :: rest ->
            let
                charWidth = UnicodeWidth.runeWidth c
                newWidth = accWidth + charWidth
            in
            if newWidth <= targetWidth then
                -- Character fits, include it
                takeLeftWidth targetWidth rest newWidth (c :: accChars)
            else
                -- Character would exceed target, stop here
                -- This means we undershoot rather than split characters
                { text = String.fromList (List.reverse accChars)
                , width = accWidth
                }


takeRight : Int -> Line -> Line
takeRight n line =
    case line of
        ( [], _ ) ->
            line

        ( c :: cs, llen ) ->
            if c.displayWidth < n then
                addChunk c (takeRight (n - c.displayWidth) ( cs, llen - c.displayWidth ))

            else if c.displayWidth == n then
                ( [ c ], c.displayWidth )

            else
                -- Need to take n width units from right of this chunk
                let
                    result = takeRightFromText n c.text
                in
                ( [ { c | text = result.text, displayWidth = result.width, style = c.style, linkParams = c.linkParams, linkUrl = c.linkUrl } ], result.width )


{-| Take n display-width units from the right of text, preserving character boundaries.
Returns the taken text and its display width.
-}
takeRightFromText : Int -> String -> { text : String, width : Int }
takeRightFromText widthToTake text =
    let
        chars = String.toList text
        totalWidth = stringDisplayWidth text
        widthToSkip = totalWidth - widthToTake
    in
    -- Skip widthToSkip units from left, then take the rest
    skipLeftWidth widthToSkip chars 0


{-| Skip characters from left until we've skipped targetWidth display units.
Returns remaining characters and their width.
When exact target can't be hit (would split emoji), overshoots the skip to undershoot the take.
-}
skipLeftWidth : Int -> List Char -> Int -> { text : String, width : Int }
skipLeftWidth targetWidth chars accWidth =
    case chars of
        [] ->
            { text = "", width = 0 }

        c :: rest ->
            let
                charWidth = UnicodeWidth.runeWidth c
                newWidth = accWidth + charWidth
            in
            if newWidth < targetWidth then
                -- Haven't skipped enough yet, continue
                skipLeftWidth targetWidth rest newWidth
            else if newWidth == targetWidth then
                -- Exactly at boundary, return rest
                { text = String.fromList rest
                , width = stringDisplayWidth (String.fromList rest)
                }
            else
                -- Would exceed target - skip this char too (overshoot skip to undershoot take)
                { text = String.fromList rest
                , width = stringDisplayWidth (String.fromList rest)
                }


spacing : Style -> List String -> Maybe String -> Int -> Chunk
spacing style params url width =
    { style = style
    , text = String.repeat width " "
    , linkParams = params
    , linkUrl = url
    , displayWidth = width
    }


takeLeft : Int -> Line -> Line
takeLeft n line =
    dropRight (lineLen line - n) line


blankLine : Line
blankLine =
    ( [], 0 )


{-| Render the model's logs as HTML.

Wraps a <pre> around the the result of calling `viewLine` for each line.

As a cheap optimization, each line is rendered lazily.

-}
view : Model -> Html.Html x
view model =
    Html.pre []
        (Array.toList (Array.map lazyLine model.lines))


lazyLine : Line -> Html.Html x
lazyLine =
    Html.Lazy.lazy viewLine


{-| Render an individual line as HTML.

The line is rendered as a <div> containing <span> elements with styling and
classes for each Chunk.

The `span` elements will have the following attributes:

  - `style="font-weight: bold|normal"`
  - `class="ansi-COLOR-fg ansi-COLOR-bg ansi-bold"`

...where each class is optional, and `COLOR` is one of:

  - `black`
  - `red`
  - `green`
  - `yellow`
  - `blue`
  - `magenta`
  - `cyan`
  - `white`
  - `bright-black`
  - `bright-red`
  - `bright-green`
  - `bright-yellow`
  - `bright-blue`
  - `bright-magenta`
  - `bright-cyan`
  - `bright-white`

If the chunk is inverted, the `-fg` and `-bg` classes will have their colors
swapped. If the chunk is bold, the `ansi-bold` class will be present.

-}
viewLine : Line -> Html.Html x
viewLine ( chunks, _ ) =
    Html.div [] (List.foldl (\c l -> viewChunk c :: l) [ Html.text "\n" ] chunks)


viewChunk : Chunk -> Html.Html x
viewChunk chunk =
    -- If the chunk has a URL, wrap it in an <a> tag
    case chunk.linkUrl of
        Just url ->
            Html.a
                ([ Html.Attributes.href url
                 , Html.Attributes.target "_blank"
                 , Html.Attributes.style "text-decoration" "underline"
                 , Html.Attributes.classList [("has-id", List.any (String.startsWith "id=") chunk.linkParams)]
                 ] ++ styleAttributes chunk.style)
                [ Html.text chunk.text ]

        Nothing ->
            Html.span (styleAttributes chunk.style)
                [ Html.text chunk.text ]


styleAttributes : Style -> List (Html.Attribute x)
styleAttributes style =
    let
        fgStyles =
            colorStyles True
                style.bold
                (if not style.inverted then
                    style.foreground

                 else
                    style.background
                )

        bgStyles =
            colorStyles False
                style.bold
                (if not style.inverted then
                    style.background

                 else
                    style.foreground
                )
    in
    [ Html.Attributes.style "font-weight"
        (if style.bold then
            "bold"

         else
            "normal"
        )
    , Html.Attributes.style "text-decoration"
        (if style.underline && style.strikethrough then
            "underline line-through"

         else if style.underline then
            "underline"

         else if style.strikethrough then
            "line-through"

         else
            "none"
        )
    , Html.Attributes.style "font-style"
        (if style.italic then
            "italic"

         else
            "normal"
        )
    , Html.Attributes.classList
        [ ( "ansi-blink", style.blink )
        , ( "ansi-faint", style.faint )
        , ( "ansi-Fraktur", style.fraktur )
        , ( "ansi-framed", style.framed )
        ]
    ]
        ++ fgStyles
        ++ bgStyles


colorStyles : Bool -> Bool -> Maybe Ansi.Color -> List (Html.Attribute x)
colorStyles fg bold mc =
    let
        suffix =
            if fg then
                "-fg"

            else
                "-bg"

        brightPrefix =
            "ansi-bright-"

        prefix =
            if bold then
                brightPrefix

            else
                "ansi-"

        class name =
            [ Html.Attributes.class (prefix ++ name ++ suffix) ]

        brightClass name =
            [ Html.Attributes.class (brightPrefix ++ name ++ suffix) ]
    in
    case mc of
        Nothing ->
            if bold then
                [ Html.Attributes.class "ansi-bold" ]

            else
                []

        Just Ansi.Black ->
            class "black"

        Just Ansi.Red ->
            class "red"

        Just Ansi.Green ->
            class "green"

        Just Ansi.Yellow ->
            class "yellow"

        Just Ansi.Blue ->
            class "blue"

        Just Ansi.Magenta ->
            class "magenta"

        Just Ansi.Cyan ->
            class "cyan"

        Just Ansi.White ->
            class "white"

        Just Ansi.BrightBlack ->
            brightClass "black"

        Just Ansi.BrightRed ->
            brightClass "red"

        Just Ansi.BrightGreen ->
            brightClass "green"

        Just Ansi.BrightYellow ->
            brightClass "yellow"

        Just Ansi.BrightBlue ->
            brightClass "blue"

        Just Ansi.BrightMagenta ->
            brightClass "magenta"

        Just Ansi.BrightCyan ->
            brightClass "cyan"

        Just Ansi.BrightWhite ->
            brightClass "white"

        Just (Ansi.Custom r g b) ->
            let
                attr =
                    if fg then
                        "color"

                    else
                        "background-color"
            in
            [ Html.Attributes.style attr <|
                "rgb("
                    ++ String.fromInt r
                    ++ ","
                    ++ String.fromInt g
                    ++ ","
                    ++ String.fromInt b
                    ++ ")"
            ]