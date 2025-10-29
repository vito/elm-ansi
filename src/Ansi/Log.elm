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
import Regex
import String


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
    , style : Style
    , linkParams : List String
    , linkUrl : Maybe String
    , cachedWidth : Int
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
        Ansi.Print s ->
            let
                chunk =
                    { text = s
                    , style = model.style
                    , linkParams = model.currentLinkParams
                    , linkUrl = model.currentLinkUrl
                    , cachedWidth = stringDisplayWidth s
                    }

                updatedChunk =
                    writeChunk model.position.column chunk
            in
            { model
                | lines = updateLine model.position.row updatedChunk model.lines
                , position = { row = model.position.row, column = model.position.column + chunk.cachedWidth }
            }

        Ansi.CarriageReturn ->
            { model | position = CursorPosition model.position.row 0 }

        Ansi.Linebreak ->
            handleAction (Ansi.Print "") <|
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
                            , style = model.style
                            , linkParams = []
                            , linkUrl = Nothing
                            , cachedWidth = model.position.column
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


{-| Combining character ranges (zero-width) from Go runewidth package.
-}
combiningRanges : Array ( Int, Int )
combiningRanges =
    Array.fromList
        [ ( 0x0300, 0x036F ), ( 0x0483, 0x0489 ), ( 0x07EB, 0x07F3 )
        , ( 0x0C00, 0x0C00 ), ( 0x0C04, 0x0C04 ), ( 0x0CF3, 0x0CF3 )
        , ( 0x0D00, 0x0D01 ), ( 0x135D, 0x135F ), ( 0x1A7F, 0x1A7F )
        , ( 0x1AB0, 0x1ACE ), ( 0x1B6B, 0x1B73 ), ( 0x1DC0, 0x1DFF )
        , ( 0x20D0, 0x20F0 ), ( 0x2CEF, 0x2CF1 ), ( 0x2DE0, 0x2DFF )
        , ( 0x3099, 0x309A ), ( 0xA66F, 0xA672 ), ( 0xA674, 0xA67D )
        , ( 0xA69E, 0xA69F ), ( 0xA6F0, 0xA6F1 ), ( 0xA8E0, 0xA8F1 )
        , ( 0xFE20, 0xFE2F ), ( 0x101FD, 0x101FD ), ( 0x10376, 0x1037A )
        , ( 0x10EAB, 0x10EAC ), ( 0x10F46, 0x10F50 ), ( 0x10F82, 0x10F85 )
        , ( 0x11300, 0x11301 ), ( 0x1133B, 0x1133C ), ( 0x11366, 0x1136C )
        , ( 0x11370, 0x11374 ), ( 0x16AF0, 0x16AF4 ), ( 0x1CF00, 0x1CF2D )
        , ( 0x1CF30, 0x1CF46 ), ( 0x1D165, 0x1D169 ), ( 0x1D16D, 0x1D172 )
        , ( 0x1D17B, 0x1D182 ), ( 0x1D185, 0x1D18B ), ( 0x1D1AA, 0x1D1AD )
        , ( 0x1D242, 0x1D244 ), ( 0x1E000, 0x1E006 ), ( 0x1E008, 0x1E018 )
        , ( 0x1E01B, 0x1E021 ), ( 0x1E023, 0x1E024 ), ( 0x1E026, 0x1E02A )
        , ( 0x1E08F, 0x1E08F ), ( 0x1E8D0, 0x1E8D6 )
        ]


-- Cached bounds for combining ranges
combiningBounds : { firstStart : Int, lastEnd : Int, maxIdx : Int }
combiningBounds =
    { firstStart = 0x0300
    , lastEnd = 0x1E8D6
    , maxIdx = 46  -- 47 ranges total (0-46)
    }


{-| Binary search to check if character is combining (zero-width).
-}
isCombining : Int -> Bool
isCombining ucs =
    if ucs < combiningBounds.firstStart || ucs > combiningBounds.lastEnd then
        False
    else
        bisearchCombining ucs 0 combiningBounds.maxIdx


bisearchCombining : Int -> Int -> Int -> Bool
bisearchCombining ucs min max =
    if max < min then
        False

    else
        let
            mid =
                (min + max) // 2
        in
        case Array.get mid combiningRanges of
            Nothing ->
                False

            Just ( rangeStart, rangeEnd ) ->
                if ucs > rangeEnd then
                    bisearchCombining ucs (mid + 1) max

                else if ucs < rangeStart then
                    bisearchCombining ucs min (mid - 1)

                else
                    True


{-| Doublewidth character ranges (2 columns) from Go runewidth package.
Includes CJK, emoji, and fullwidth characters.
-}
doublewidthRanges : Array ( Int, Int )
doublewidthRanges =
    Array.fromList
        [ ( 0x1100, 0x115F ), ( 0x231A, 0x231B ), ( 0x2329, 0x232A )
        , ( 0x23E9, 0x23EC ), ( 0x23F0, 0x23F0 ), ( 0x23F3, 0x23F3 )
        , ( 0x25FD, 0x25FE ), ( 0x2614, 0x2615 ), ( 0x2648, 0x2653 )
        , ( 0x267F, 0x267F ), ( 0x2693, 0x2693 ), ( 0x26A1, 0x26A1 )
        , ( 0x26AA, 0x26AB ), ( 0x26BD, 0x26BE ), ( 0x26C4, 0x26C5 )
        , ( 0x26CE, 0x26CE ), ( 0x26D4, 0x26D4 ), ( 0x26EA, 0x26EA )
        , ( 0x26F2, 0x26F3 ), ( 0x26F5, 0x26F5 ), ( 0x26FA, 0x26FA )
        , ( 0x26FD, 0x26FD ), ( 0x2705, 0x2705 ), ( 0x270A, 0x270B )
        , ( 0x2728, 0x2728 ), ( 0x274C, 0x274C ), ( 0x274E, 0x274E )
        , ( 0x2753, 0x2755 ), ( 0x2757, 0x2757 ), ( 0x2795, 0x2797 )
        , ( 0x27B0, 0x27B0 ), ( 0x27BF, 0x27BF ), ( 0x2B1B, 0x2B1C )
        , ( 0x2B50, 0x2B50 ), ( 0x2B55, 0x2B55 ), ( 0x2E80, 0x2E99 )
        , ( 0x2E9B, 0x2EF3 ), ( 0x2F00, 0x2FD5 ), ( 0x2FF0, 0x303E )
        , ( 0x3041, 0x3096 ), ( 0x3099, 0x30FF ), ( 0x3105, 0x312F )
        , ( 0x3131, 0x318E ), ( 0x3190, 0x31E3 ), ( 0x31EF, 0x321E )
        , ( 0x3220, 0x3247 ), ( 0x3250, 0x4DBF ), ( 0x4E00, 0xA48C )
        , ( 0xA490, 0xA4C6 ), ( 0xA960, 0xA97C ), ( 0xAC00, 0xD7A3 )
        , ( 0xF900, 0xFAFF ), ( 0xFE10, 0xFE19 ), ( 0xFE30, 0xFE52 )
        , ( 0xFE54, 0xFE66 ), ( 0xFE68, 0xFE6B ), ( 0xFF01, 0xFF60 )
        , ( 0xFFE0, 0xFFE6 ), ( 0x16FE0, 0x16FE4 ), ( 0x16FF0, 0x16FF1 )
        , ( 0x17000, 0x187F7 ), ( 0x18800, 0x18CD5 ), ( 0x18D00, 0x18D08 )
        , ( 0x1AFF0, 0x1AFF3 ), ( 0x1AFF5, 0x1AFFB ), ( 0x1AFFD, 0x1AFFE )
        , ( 0x1B000, 0x1B122 ), ( 0x1B132, 0x1B132 ), ( 0x1B150, 0x1B152 )
        , ( 0x1B155, 0x1B155 ), ( 0x1B164, 0x1B167 ), ( 0x1B170, 0x1B2FB )
        , ( 0x1F004, 0x1F004 ), ( 0x1F0CF, 0x1F0CF ), ( 0x1F18E, 0x1F18E )
        , ( 0x1F191, 0x1F19A ), ( 0x1F200, 0x1F202 ), ( 0x1F210, 0x1F23B )
        , ( 0x1F240, 0x1F248 ), ( 0x1F250, 0x1F251 ), ( 0x1F260, 0x1F265 )
        , ( 0x1F300, 0x1F320 ), ( 0x1F32D, 0x1F335 ), ( 0x1F337, 0x1F37C )
        , ( 0x1F37E, 0x1F393 ), ( 0x1F3A0, 0x1F3CA ), ( 0x1F3CF, 0x1F3D3 )
        , ( 0x1F3E0, 0x1F3F0 ), ( 0x1F3F4, 0x1F3F4 ), ( 0x1F3F8, 0x1F43E )
        , ( 0x1F440, 0x1F440 ), ( 0x1F442, 0x1F4FC ), ( 0x1F4FF, 0x1F53D )
        , ( 0x1F54B, 0x1F54E ), ( 0x1F550, 0x1F567 ), ( 0x1F57A, 0x1F57A )
        , ( 0x1F595, 0x1F596 ), ( 0x1F5A4, 0x1F5A4 ), ( 0x1F5FB, 0x1F64F )
        , ( 0x1F680, 0x1F6C5 ), ( 0x1F6CC, 0x1F6CC ), ( 0x1F6D0, 0x1F6D2 )
        , ( 0x1F6D5, 0x1F6D7 ), ( 0x1F6DC, 0x1F6DF ), ( 0x1F6EB, 0x1F6EC )
        , ( 0x1F6F4, 0x1F6FC ), ( 0x1F7E0, 0x1F7EB ), ( 0x1F7F0, 0x1F7F0 )
        , ( 0x1F90C, 0x1F93A ), ( 0x1F93C, 0x1F945 ), ( 0x1F947, 0x1F9FF )
        , ( 0x1FA70, 0x1FA7C ), ( 0x1FA80, 0x1FA88 ), ( 0x1FA90, 0x1FABD )
        , ( 0x1FABF, 0x1FAC5 ), ( 0x1FACE, 0x1FADB ), ( 0x1FAE0, 0x1FAE8 )
        , ( 0x1FAF0, 0x1FAF8 ), ( 0x20000, 0x2FFFD ), ( 0x30000, 0x3FFFD )
        ]


-- Cached bounds for doublewidth ranges
doublewidthBounds : { firstStart : Int, lastEnd : Int, maxIdx : Int }
doublewidthBounds =
    { firstStart = 0x1100
    , lastEnd = 0x3FFFD
    , maxIdx = 119  -- 120 ranges total (0-119)
    }


{-| Check if character is in doublewidth table (2 columns).
Uses binary search for O(log n) performance.
-}
isDoublewidth : Int -> Bool
isDoublewidth ucs =
    if ucs < doublewidthBounds.firstStart || ucs > doublewidthBounds.lastEnd then
        False
    else
        bisearchDoublewidth ucs 0 doublewidthBounds.maxIdx


bisearchDoublewidth : Int -> Int -> Int -> Bool
bisearchDoublewidth ucs min max =
    if max < min then
        False

    else
        let
            mid =
                (min + max) // 2
        in
        case Array.get mid doublewidthRanges of
            Nothing ->
                False

            Just ( rangeStart, rangeEnd ) ->
                if ucs > rangeEnd then
                    bisearchDoublewidth ucs (mid + 1) max

                else if ucs < rangeStart then
                    bisearchDoublewidth ucs min (mid - 1)

                else
                    True


{-| Check if character is a zero-width format/nonprint character.
Based on Go runewidth's nonprint table - these have no visual width.
Includes control chars, format chars (ZWSP, ZWJ, etc), surrogates, and special markers.
-}
isZeroWidthFormat : Int -> Bool
isZeroWidthFormat ucs =
    -- Soft hyphen (already handled in control char range but mentioned in Go nonprint)
    ucs == 0x00AD
    -- Arabic format character
    || ucs == 0x070F
    -- Mongolian vowel separator range
    || (ucs >= 0x180B && ucs <= 0x180E)
    -- Zero-width space, ZWNJ, ZWJ, LRM, RLM, and related format characters
    || (ucs >= 0x200B && ucs <= 0x200F)
    -- Line/paragraph separators and bidi formatting
    || (ucs >= 0x2028 && ucs <= 0x202E)
    -- Inhibit/activate controls
    || (ucs >= 0x206A && ucs <= 0x206F)
    -- Zero-width no-break space (BOM)
    || ucs == 0xFEFF
    -- Interlinear annotation chars
    || (ucs >= 0xFFF9 && ucs <= 0xFFFB)
    -- Noncharacters
    || ucs == 0xFFFE
    || ucs == 0xFFFF


unicodeCharWidth : Char -> Int
unicodeCharWidth char =
    let
        ucs =
            Char.toCode char
    in
    if ucs < doublewidthBounds.firstStart then
        if isCombining ucs then
            0
        else if isZeroWidthFormat ucs then
            0
        else
            1

    else if ucs >= 0x1F300 && ucs <= 0x1F9FF then
        2

    else if isDoublewidth ucs then
        2

    else if isCombining ucs then
        0

    else if isZeroWidthFormat ucs then
        0

    else
        1


normalWidthRegex : Regex.Regex
normalWidthRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^[\u{0020}-\u{007E}\u{00A0}-\u{00AC}\u{00AE}-\u{02FF}]+$"


stringDisplayWidth : String -> Int
stringDisplayWidth str =
    if String.isEmpty str then
        0
    -- Fast path: if all characters have normal width (width=1), use String.length
    else if Regex.contains normalWidthRegex str then
        String.length str
    -- General path: handles control chars, combining, and doublewidth
    else
        stringDisplayWidthHelper str 0


stringDisplayWidthHelper : String -> Int -> Int
stringDisplayWidthHelper str width =
    case String.uncons str of
        Nothing ->
            width

        Just ( char, rest ) ->
            let
                ucs =
                    Char.toCode char
            in
            -- Fast path for ASCII/Latin
            if ucs < 0x0300 then
                if ucs < 32 || (ucs >= 0x7F && ucs < 0xA0) || ucs == 0x00AD then
                    stringDisplayWidthHelper rest width
                else
                    stringDisplayWidthHelper rest (width + 1)

            else
                stringDisplayWidthHelper rest (width + unicodeCharWidth char)


writeChunk : Int -> Chunk -> Line -> Line
writeChunk pos chunk line =
    let
        len =
            lineLen line

        clen =
            chunk.cachedWidth

        afterLen =
            len - (clen + pos)
    in
    if len == pos then
        addChunk chunk line

    else if pos > len then
        addChunk chunk <| addChunk (spacing chunk.style chunk.linkParams chunk.linkUrl (pos - len)) line

    else
        let
            appended =
                addChunk chunk (dropRight (len - pos) line)
        in
        if afterLen > 0 then
            List.foldl addChunk appended (Tuple.first <| takeRight afterLen line)

        else
            appended


addChunk : Chunk -> Line -> Line
addChunk chunk line =
    let
        clen =
            chunk.cachedWidth
    in
    if clen == 0 then
        line

    else
        case line of
            ( [], _ ) ->
                ( [ chunk ], clen )

            ( c :: cs, llen ) ->
                if c.style == chunk.style && c.linkUrl == chunk.linkUrl && c.linkParams == chunk.linkParams then
                    let
                        mergedText =
                            c.text ++ chunk.text

                        mergedWidth =
                            c.cachedWidth + clen
                    in
                    ( { c | text = mergedText, cachedWidth = mergedWidth } :: cs, llen + clen )

                else
                    ( chunk :: c :: cs, llen + clen )


dropRight : Int -> Line -> Line
dropRight n line =
    case line of
        ( [], _ ) ->
            line

        ( c :: cs, llen ) ->
            let
                clen =
                    c.cachedWidth
            in
            if clen <= n then
                dropRight (n - clen) ( cs, llen - clen )

            else
                let
                    newText =
                        String.dropRight n c.text

                    newWidth =
                        stringDisplayWidth newText
                in
                ( { c | text = newText, cachedWidth = newWidth } :: cs, llen - n )


takeRight : Int -> Line -> Line
takeRight n line =
    case line of
        ( [], _ ) ->
            line

        ( c :: cs, llen ) ->
            let
                clen = c.cachedWidth
            in
            if clen < n then
                addChunk c (takeRight (n - clen) ( cs, llen - clen ))

            else if clen == n then
                ( [ c ], clen )

            else
                let
                    newText =
                        String.right n c.text

                    newWidth =
                        stringDisplayWidth newText
                in
                ( [ { c | text = newText, style = c.style, linkParams = c.linkParams, linkUrl = c.linkUrl, cachedWidth = newWidth } ], n )


spacing : Style -> List String -> Maybe String -> Int -> Chunk
spacing style params url len =
    { style = style
    , text = String.repeat len " "
    , linkParams = params
    , linkUrl = url
    , cachedWidth = len
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