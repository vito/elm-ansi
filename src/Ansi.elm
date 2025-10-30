module Ansi exposing
    ( parse, parseInto
    , Action(..), Color(..), EraseMode(..)
    )

{-| This library primarily exposes the `parse` function and the types that it
will yield.

@docs parse, parseInto

@docs Action, Color, EraseMode

-}

import Char
import String
import Url


{-| The events relevant to interpreting the stream.

  - `Print` is a chunk of text which should be interpreted with the style implied
    by the preceding actions (i.e. `[SetBold True, Print "foo"]`) should yield a
    bold `foo`
  - `Remainder` is a partial ANSI escape sequence, returned at the end of the
    actions if it was cut off. The next string passed to `parse` should have this
    prepended to it.
  - `HyperlinkStart` starts a hyperlink with the given parameters and URL
  - `HyperlinkEnd` ends a hyperlink
  - The rest are derived from their respective ANSI escape sequences.

-}
type Action
    = Print String
    | Remainder String
    | SetForeground (Maybe Color)
    | SetBackground (Maybe Color)
    | SetBold Bool
    | SetFaint Bool
    | SetItalic Bool
    | SetUnderline Bool
    | SetBlink Bool
    | SetInverted Bool
    | SetStrikethrough Bool
    | SetFraktur Bool
    | SetFramed Bool
    | Linebreak
    | CarriageReturn
    | CursorUp Int
    | CursorDown Int
    | CursorForward Int
    | CursorBackward Int
    | CursorPosition Int Int
    | CursorColumn Int
    | EraseDisplay EraseMode
    | EraseLine EraseMode
    | SaveCursorPosition
    | RestoreCursorPosition
    | HyperlinkStart (List String) String  -- (params, uri)
    | HyperlinkEnd


{-| The colors applied to the foreground/background.
-}
type Color
    = Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | BrightBlack
    | BrightRed
    | BrightGreen
    | BrightYellow
    | BrightBlue
    | BrightMagenta
    | BrightCyan
    | BrightWhite
    | Custom Int Int Int


{-| Method to erase the display or line.
-}
type EraseMode
    = EraseToBeginning
    | EraseToEnd
    | EraseAll


type Parser a
    = Parser ParserState Bool a (Action -> a -> a)


type alias CSIState =
    { marker : Maybe ParameterMarker
    , parsedCodes : List (Maybe Int)
    , currentCode : Maybe Int
    }


type ParserState
    = Escaped
    | CSI CSIState
    | OSC String
    | OSCAwaitingTerminator String
    | CharsetDesignation
    | Unescaped String


type ParameterMarker
    = LessThan      -- < (0x3C)
    | Equals        -- = (0x3D)
    | GreaterThan   -- > (0x3E)
    | Question      -- ? (0x3F)


emptyParser : a -> (Action -> a -> a) -> Parser a
emptyParser model update =
    Parser (Unescaped "") False model update


{-| Convert an arbitrary String of text into a sequence of actions.

If the input string ends with a partial ANSI escape sequence, it will be
yielded as a `Remainder` action, which should then be prepended to the next
call to `parse`.

-}
parse : String -> List Action
parse =
    List.reverse << parseInto [] (::)


{-| Update a structure with actions parsed out of the given string.
-}
parseInto : a -> (Action -> a -> a) -> String -> a
parseInto model update ansi =
    completeParsing <|
        List.foldl parseChar (emptyParser model update) <|
            String.toList ansi


completeParsing : Parser a -> a
completeParsing parser =
    case parser of
        Parser Escaped _ model update ->
            update (Remainder "\u{001B}") model

        Parser (CSI { marker, parsedCodes, currentCode }) _ model update ->
            let
                markerStr = case marker of
                    Just LessThan -> "<"
                    Just Equals -> "="
                    Just GreaterThan -> ">"
                    Just Question -> "?"
                    Nothing -> ""
            in
            update (Remainder <| "\u{001B}[" ++ markerStr ++ encodeCodes (parsedCodes ++ [ currentCode ])) model

        Parser (OSC chars) _ model update ->
            update (Remainder <| "\u{001B}]" ++ chars) model

        Parser (OSCAwaitingTerminator chars) _ model update ->
            update (Remainder <| "\u{001B}]" ++ chars ++ "\u{001B}") model

        Parser CharsetDesignation _ model update ->
            update (Remainder "\u{001B}(") model

        Parser (Unescaped "") _ model _ ->
            model

        Parser (Unescaped str) _ model update ->
            update (Print str) model


encodeCodes : List (Maybe Int) -> String
encodeCodes codes =
    String.join ";" (List.map encodeCode codes)


encodeCode : Maybe Int -> String
encodeCode code =
    case code of
        Nothing ->
            ""

        Just num ->
            String.fromInt num


{-| Converts a color code to an 8-bit color per
<https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit>
-}
colorCode : Int -> Maybe Color
colorCode code =
    case code of
        0 -> Just Black
        1 -> Just Red
        2 -> Just Green
        3 -> Just Yellow
        4 -> Just Blue
        5 -> Just Magenta
        6 -> Just Cyan
        7 -> Just White
        8 -> Just BrightBlack
        9 -> Just BrightRed
        10 -> Just BrightGreen
        11 -> Just BrightYellow
        12 -> Just BrightBlue
        13 -> Just BrightMagenta
        14 -> Just BrightCyan
        15 -> Just BrightWhite

        _ ->
            if code < 232 then
                -- 6x6x6 RGB cube (codes 16-231, 216 colors)
                let
                    c = code - 16
                    b = modBy 6 c
                    g = modBy 6 (c // 6)
                    r = c // 36  -- Division by 36 (6²) extracts red component
                in
                Just <| Custom (scale6 r) (scale6 g) (scale6 b)

            else if code < 256 then
                -- Grayscale ramp (codes 232-255, 24 shades)
                let
                    gray = (code - 232) * 10 + 8
                in
                Just <| Custom gray gray gray

            else
                Nothing


{-| Scales [0,5] -> [0,255] (non-uniformly)
0 -> 0, 1 -> 95, 2 -> 135, 3 -> 175, 4 -> 215, 5 -> 255
-}
scale6 : Int -> Int
scale6 n =
    if n == 0 then 0 else 55 + n * 40


{-| Capture SGR arguments in pattern match
-}
captureArguments : List Int -> List Action
captureArguments list =
    case list of
        -- Incomplete 256-color sequences - skip them entirely
        38 :: 5 :: [] ->
            []

        48 :: 5 :: [] ->
            []

        -- Incomplete 24-bit color sequences - skip them entirely
        38 :: 2 :: _ :: [] ->
            []

        38 :: 2 :: _ :: _ :: [] ->
            []

        48 :: 2 :: _ :: [] ->
            []

        48 :: 2 :: _ :: _ :: [] ->
            []

        -- Valid 256-color sequences - only emit action if colorCode succeeds
        38 :: 5 :: n :: xs ->
            case colorCode n of
                Just color ->
                    SetForeground (Just color) :: captureArguments xs
                Nothing ->
                    captureArguments xs

        48 :: 5 :: n :: xs ->
            case colorCode n of
                Just color ->
                    SetBackground (Just color) :: captureArguments xs
                Nothing ->
                    captureArguments xs

        -- Valid 24-bit color sequences
        38 :: 2 :: r :: g :: b :: xs ->
            let
                c =
                    clamp 0 255
            in
            SetForeground (Just <| Custom (c r) (c g) (c b)) :: captureArguments xs

        48 :: 2 :: r :: g :: b :: xs ->
            let
                c =
                    clamp 0 255
            in
            SetBackground (Just <| Custom (c r) (c g) (c b)) :: captureArguments xs

        n :: xs ->
            codeActions n ++ captureArguments xs

        [] ->
            []


-- Check if a character is an unreserved character according to RFC 3986
isUnreservedChar : Char -> Bool
isUnreservedChar c =
    Char.isAlphaNum c || c == '-' || c == '.' || c == '_' || c == '~'


-- Check if a character is a reserved character according to RFC 3986
isReservedChar : Char -> Bool
isReservedChar c =
    String.contains (String.fromChar c) ":/?#[]@!$&'()*+,;="


-- Validate that a parameter follows HTTP spec but doesn't contain the
-- hyperlink protocol delimiters
validateSingleParam : String -> Bool
validateSingleParam param =
    -- For hyperlink parameters, we need to exclude : and ; as they're delimiters
    -- in the hyperlink protocol, even though they're valid in HTTP URLs
    String.all (\c ->
        (isUnreservedChar c || (isReservedChar c && c /= ':' && c /= ';')) &&
        Char.toCode c >= 32 && Char.toCode c <= 126
    ) param


processHyperlinkParts : String -> String -> List Action
processHyperlinkParts params uri =
    let
        parsedParams = parseParams params
        validParams = List.all validateSingleParam parsedParams

        processedUri =
            if String.any (\c -> Char.toCode c > 127) uri then
                String.toList uri
                    |> List.map (\c ->
                        if Char.toCode c > 127 then
                            Url.percentEncode (String.fromChar c)
                        else
                            String.fromChar c
                    )
                    |> String.concat
            else
                uri
    in
    if not validParams then
        []
    else
        [HyperlinkStart parsedParams processedUri]


parseOSCWithState : Bool -> String -> (List Action, Bool)
parseOSCWithState hasActiveLink str =
    -- Specifically check for the hyperlink end sequence "8;;"
    if str == "8;;" then
        if hasActiveLink then
            ([HyperlinkEnd], False)
        else
            ([], False)
    -- Check for hyperlink start sequence "8;params;uri"
    else if String.startsWith "8;" str then
        -- Split only on first 2 semicolons to preserve semicolons in URI
        case String.split ";" str of
            "8" :: params :: rest ->
                -- URI might contain semicolons, so rejoin remaining parts
                let
                    uri = String.join ";" rest
                in
                if String.isEmpty uri then
                    -- Invalid format: no URI provided
                    ([], hasActiveLink)
                else
                    let
                        actions = processHyperlinkParts params uri
                    in
                    (actions, not (List.isEmpty actions))
            _ ->
                ([], hasActiveLink)
    else
        ([], hasActiveLink)


{-| Parse hyperlink parameters
-}
parseParams : String -> List String
parseParams params =
    if String.isEmpty params then
        []
    else
        String.split ":" params


parseChar : Char -> Parser a -> Parser a
parseChar c parser =
    case parser of
        Parser (Unescaped str) hasLink model update ->
            case c of
                '\u{000D}' ->
                    let
                        updatedModel = if str == "" then model else update (Print str) model
                    in
                    Parser (Unescaped "") hasLink (update CarriageReturn updatedModel) update

                '\n' ->
                    let
                        updatedModel = if str == "" then model else update (Print str) model
                    in
                    Parser (Unescaped "") hasLink (update Linebreak updatedModel) update

                '\u{001B}' ->
                    let
                        updatedModel = if str == "" then model else update (Print str) model
                    in
                    Parser Escaped hasLink updatedModel update

                _ ->
                    Parser (Unescaped (str ++ String.fromChar c)) hasLink model update

        Parser Escaped hasLink model update ->
            case c of
                '[' ->
                    Parser (CSI { marker = Nothing, parsedCodes = [], currentCode = Nothing }) hasLink model update

                ']' ->
                    Parser (OSC "") hasLink model update

                '(' ->
                    Parser CharsetDesignation hasLink model update

                ')' ->
                    Parser CharsetDesignation hasLink model update

                '*' ->
                    Parser CharsetDesignation hasLink model update

                '+' ->
                    Parser CharsetDesignation hasLink model update

                '7' ->
                    Parser (Unescaped "") hasLink (update SaveCursorPosition model) update

                '8' ->
                    Parser (Unescaped "") hasLink (update RestoreCursorPosition model) update

                'M' ->
                    Parser (Unescaped "") hasLink (update (CursorUp 1) model) update

                'D' ->
                    Parser (Unescaped "") hasLink (update (CursorDown 1) model) update

                'E' ->
                    Parser (Unescaped "") hasLink (update Linebreak (update CarriageReturn model)) update

                '=' ->
                    Parser (Unescaped "") hasLink model update

                '>' ->
                    Parser (Unescaped "") hasLink model update

                'c' ->
                    Parser (Unescaped "") hasLink model update

                'H' ->
                    Parser (Unescaped "") hasLink model update

                '\\' ->
                    Parser (Unescaped "\\") hasLink model update

                _ ->
                    Parser (Unescaped (String.fromChar c)) hasLink model update

        Parser (OSC chars) hasLink model update ->
            case c of
                '\u{0007}' ->
                    -- BEL character marks the end of an OSC sequence
                    let
                        (actions, newHasLink) = parseOSCWithState hasLink chars
                    in
                    finishEscapeSequence parser newHasLink actions

                '\u{001B}' ->
                    -- Possibly the start of an ESC+backslash terminator
                    Parser (OSCAwaitingTerminator chars) hasLink model update

                _ ->
                    -- Accumulate characters in the OSC sequence
                    Parser (OSC (chars ++ String.fromChar c)) hasLink model update

        Parser (OSCAwaitingTerminator chars) hasLink model update ->
            case c of
                '\\' ->
                    -- ESC+backslash terminator found
                    let
                        (actions, newHasLink) = parseOSCWithState hasLink chars
                    in
                    finishEscapeSequence parser newHasLink actions

                _ ->
                    -- Not a terminator, continue as a normal OSC sequence with ESC included
                    Parser (OSC (chars ++ "\u{001B}" ++ String.fromChar c)) hasLink model update

        Parser CharsetDesignation hasLink model update ->
            Parser (Unescaped "") hasLink model update

        Parser (CSI { marker, parsedCodes, currentCode }) hasLink model update ->
            if marker == Nothing && parsedCodes == [] && currentCode == Nothing then
                case c of
                    '<' ->
                        Parser (CSI { marker = Just LessThan, parsedCodes = [], currentCode = Nothing }) hasLink model update

                    '=' ->
                        Parser (CSI { marker = Just Equals, parsedCodes = [], currentCode = Nothing }) hasLink model update

                    '>' ->
                        Parser (CSI { marker = Just GreaterThan, parsedCodes = [], currentCode = Nothing }) hasLink model update

                    '?' ->
                        Parser (CSI { marker = Just Question, parsedCodes = [], currentCode = Nothing }) hasLink model update

                    _ ->
                        -- Not a parameter marker, process as normal CSI character
                        handleCSICommand c marker parsedCodes currentCode hasLink model update parser
            else
                -- We already have a marker or codes, process normally
                handleCSICommand c marker parsedCodes currentCode hasLink model update parser


-- Helper function to handle CSI command characters
handleCSICommand : Char -> Maybe ParameterMarker -> List (Maybe Int) -> Maybe Int -> Bool -> a -> (Action -> a -> a) -> Parser a -> Parser a
handleCSICommand c marker parsedCodes currentCode hasLink model update parser =
    -- If we have a parameter marker (<>=?), this is a private/experimental sequence
    -- We consume it but don't execute any actions (per ANSI/DEC VT100 standard)
    case marker of
        Just _ ->
            -- Any letter terminates the sequence, but we produce no actions
            if Char.isAlpha c then
                finishEscapeSequence parser hasLink []
            else if c == ';' then
                Parser (CSI { marker = marker, parsedCodes = parsedCodes ++ [ currentCode ], currentCode = Nothing }) hasLink model update
            else if Char.isDigit c then
                let
                    digitValue = Char.toCode c - 48
                    newCode = case currentCode of
                        Nothing -> digitValue
                        Just n -> n * 10 + digitValue
                in
                Parser (CSI { marker = marker, parsedCodes = parsedCodes, currentCode = Just newCode }) hasLink model update
            else
                -- Unknown character in private sequence, discard
                finishEscapeSequence parser hasLink []

        Nothing ->
            -- No marker means standard CSI sequence, process normally
            case c of
                'm' ->
                    finishEscapeSequence parser hasLink <|
                        captureArguments <|
                            List.map (Maybe.withDefault 0) (parsedCodes ++ [ currentCode ])

                'A' ->
                    finishEscapeSequence parser hasLink
                        [ CursorUp (max 1 (Maybe.withDefault 1 currentCode)) ]

                'B' ->
                    finishEscapeSequence parser hasLink
                        [ CursorDown (max 1 (Maybe.withDefault 1 currentCode)) ]

                'C' ->
                    finishEscapeSequence parser hasLink
                        [ CursorForward (max 1 (Maybe.withDefault 1 currentCode)) ]

                'D' ->
                    finishEscapeSequence parser hasLink
                        [ CursorBackward (max 1 (Maybe.withDefault 1 currentCode)) ]

                'E' ->
                    finishEscapeSequence parser hasLink
                        [ CursorDown (max 1 (Maybe.withDefault 1 currentCode)), CursorColumn 0 ]

                'F' ->
                    finishEscapeSequence parser hasLink
                        [ CursorUp (max 1 (Maybe.withDefault 1 currentCode)), CursorColumn 0 ]

                'G' ->
                    finishEscapeSequence parser hasLink
                        [ CursorColumn (max 0 (Maybe.withDefault 1 currentCode - 1)) ]

                'H' ->
                    finishEscapeSequence parser hasLink <|
                        cursorPosition (parsedCodes ++ [ currentCode ])

                'J' ->
                    finishEscapeSequence parser hasLink
                        [ EraseDisplay (eraseMode (Maybe.withDefault 0 currentCode)) ]

                'K' ->
                    finishEscapeSequence parser hasLink
                        [ EraseLine (eraseMode (Maybe.withDefault 0 currentCode)) ]

                'f' ->
                    finishEscapeSequence parser hasLink <|
                        cursorPosition (parsedCodes ++ [ currentCode ])

                's' ->
                    finishEscapeSequence parser hasLink [ SaveCursorPosition ]

                'u' ->
                    finishEscapeSequence parser hasLink [ RestoreCursorPosition ]

                ';' ->
                    Parser (CSI { marker = marker, parsedCodes = parsedCodes ++ [ currentCode ], currentCode = Nothing }) hasLink model update

                '<' ->
                    Parser (CSI { marker = Just LessThan, parsedCodes = parsedCodes, currentCode = currentCode }) hasLink model update

                '=' ->
                    Parser (CSI { marker = Just Equals, parsedCodes = parsedCodes, currentCode = currentCode }) hasLink model update

                '>' ->
                    Parser (CSI { marker = Just GreaterThan, parsedCodes = parsedCodes, currentCode = currentCode }) hasLink model update

                '?' ->
                    Parser (CSI { marker = Just Question, parsedCodes = parsedCodes, currentCode = currentCode }) hasLink model update

                _ ->
                    if Char.isDigit c then
                        let
                            digitValue = Char.toCode c - 48  -- '0' is ASCII 48
                            newCode = case currentCode of
                                Nothing -> digitValue
                                Just n -> n * 10 + digitValue
                        in
                        Parser (CSI { marker = marker, parsedCodes = parsedCodes, currentCode = Just newCode }) hasLink model update
                    else
                        -- Any non-digit, non-semicolon character (letter or otherwise) terminates CSI
                        -- Unknown/unrecognized commands are discarded per DEC/VT100 behavior
                        -- Private sequences (with markers) that we don't recognize are also discarded
                        finishEscapeSequence parser hasLink []


finishEscapeSequence : Parser a -> Bool -> List Action -> Parser a
finishEscapeSequence (Parser _ hasLink model update) newHasLink actions =
    Parser (Unescaped "") newHasLink (List.foldl update model actions) update


cursorPosition : List (Maybe Int) -> List Action
cursorPosition codes =
    case codes of
        [ Nothing, Nothing ] ->
            [ CursorPosition 1 1 ]

        [ Nothing ] ->
            [ CursorPosition 1 1 ]

        [ Just row ] ->
            [ CursorPosition (max 1 row) 1 ]

        [ Just row, Nothing ] ->
            [ CursorPosition (max 1 row) 1 ]

        [ Nothing, Just col ] ->
            [ CursorPosition 1 (max 1 col) ]

        [ Just row, Just col ] ->
            [ CursorPosition (max 1 row) (max 1 col) ]

        _ ->
            []


eraseMode : Int -> EraseMode
eraseMode code =
    case code of
        0 ->
            EraseToEnd

        1 ->
            EraseToBeginning

        _ ->
            EraseAll


codeActions : Int -> List Action
codeActions code =
    case code of
        0 ->
            reset

        1 ->
            [ SetBold True ]

        2 ->
            [ SetFaint True ]

        3 ->
            [ SetItalic True ]

        4 ->
            [ SetUnderline True ]

        5 ->
            [ SetBlink True ]

        7 ->
            [ SetInverted True ]

        9 ->
            [ SetStrikethrough True ]

        20 ->
            [ SetFraktur True ]

        21 ->
            [ SetBold False ]

        22 ->
            [ SetFaint False
            , SetBold False
            ]

        23 ->
            [ SetItalic False
            , SetFraktur False
            ]

        24 ->
            [ SetUnderline False ]

        25 ->
            [ SetBlink False ]

        27 ->
            [ SetInverted False ]

        29 ->
            [ SetStrikethrough False ]

        30 ->
            [ SetForeground (Just Black) ]

        31 ->
            [ SetForeground (Just Red) ]

        32 ->
            [ SetForeground (Just Green) ]

        33 ->
            [ SetForeground (Just Yellow) ]

        34 ->
            [ SetForeground (Just Blue) ]

        35 ->
            [ SetForeground (Just Magenta) ]

        36 ->
            [ SetForeground (Just Cyan) ]

        37 ->
            [ SetForeground (Just White) ]

        39 ->
            [ SetForeground Nothing ]

        40 ->
            [ SetBackground (Just Black) ]

        41 ->
            [ SetBackground (Just Red) ]

        42 ->
            [ SetBackground (Just Green) ]

        43 ->
            [ SetBackground (Just Yellow) ]

        44 ->
            [ SetBackground (Just Blue) ]

        45 ->
            [ SetBackground (Just Magenta) ]

        46 ->
            [ SetBackground (Just Cyan) ]

        47 ->
            [ SetBackground (Just White) ]

        49 ->
            [ SetBackground Nothing ]

        51 ->
            [ SetFramed True ]

        54 ->
            [ SetFramed False ]

        90 ->
            [ SetForeground (Just BrightBlack) ]

        91 ->
            [ SetForeground (Just BrightRed) ]

        92 ->
            [ SetForeground (Just BrightGreen) ]

        93 ->
            [ SetForeground (Just BrightYellow) ]

        94 ->
            [ SetForeground (Just BrightBlue) ]

        95 ->
            [ SetForeground (Just BrightMagenta) ]

        96 ->
            [ SetForeground (Just BrightCyan) ]

        97 ->
            [ SetForeground (Just BrightWhite) ]

        100 ->
            [ SetBackground (Just BrightBlack) ]

        101 ->
            [ SetBackground (Just BrightRed) ]

        102 ->
            [ SetBackground (Just BrightGreen) ]

        103 ->
            [ SetBackground (Just BrightYellow) ]

        104 ->
            [ SetBackground (Just BrightBlue) ]

        105 ->
            [ SetBackground (Just BrightMagenta) ]

        106 ->
            [ SetBackground (Just BrightCyan) ]

        107 ->
            [ SetBackground (Just BrightWhite) ]

        _ ->
            []


reset : List Action
reset =
    [ SetForeground Nothing
    , SetBackground Nothing
    , SetBold False
    , SetFaint False
    , SetItalic False
    , SetUnderline False
    , SetBlink False
    , SetInverted False
    , SetStrikethrough False
    , SetFraktur False
    , SetFramed False
    ]