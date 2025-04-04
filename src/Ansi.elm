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
    | SetFraktur Bool
    | SetFramed Bool
    | Linebreak
    | CarriageReturn
    | CursorUp Int
    | CursorDown Int
    | CursorForward Int
    | CursorBack Int
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


type ParserState
    = Escaped
    | CSI (List (Maybe Int)) (Maybe Int)
    | OSC String
    | OSCTerminating String
    | Unescaped String


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
            String.split "" ansi


completeParsing : Parser a -> a
completeParsing parser =
    case parser of
        Parser Escaped _ model update ->
            update (Remainder "\u{001B}") model

        Parser (CSI codes currentCode) _ model update ->
            update (Remainder <| "\u{001B}[" ++ encodeCodes (codes ++ [ currentCode ])) model

        Parser (OSC chars) _ model update ->
            update (Remainder <| "\u{001B}]" ++ chars) model

        Parser (OSCTerminating chars) _ model update ->
            update (Remainder <| "\u{001B}]" ++ chars ++ "\u{001B}") model

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
        0 ->
            Just Black

        1 ->
            Just Red

        2 ->
            Just Green

        3 ->
            Just Yellow

        4 ->
            Just Blue

        5 ->
            Just Magenta

        6 ->
            Just Cyan

        7 ->
            Just White

        8 ->
            Just BrightBlack

        9 ->
            Just BrightRed

        10 ->
            Just BrightGreen

        11 ->
            Just BrightYellow

        12 ->
            Just BrightBlue

        13 ->
            Just BrightMagenta

        14 ->
            Just BrightCyan

        15 ->
            Just BrightWhite

        _ ->
            if code >= 16 && code < 232 then
                let
                    c =
                        code - 16

                    b =
                        modBy 6 c

                    g =
                        modBy 6 (c // 6)

                    r =
                        modBy 6 ((c // 6) // 6)

                    -- Scales [0,5] -> [0,255] (not uniformly)
                    -- 0     1     2     3     4     5
                    -- 0    95   135   175   215   255
                    scale n =
                        if n == 0 then
                            0

                        else
                            55 + n * 40
                in
                Just <| Custom (scale r) (scale g) (scale b)

            else if code >= 232 && code < 256 then
                let
                    -- scales [232,255] -> [8,238]
                    c =
                        (code - 232) * 10 + 8
                in
                Just <| Custom c c c

            else
                Nothing


{-| Capture SGR arguments in pattern match
-}
captureArguments : List Int -> List Action
captureArguments list =
    case list of
        38 :: 5 :: n :: xs ->
            SetForeground (colorCode n) :: captureArguments xs

        48 :: 5 :: n :: xs ->
            SetBackground (colorCode n) :: captureArguments xs

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
                Url.percentEncode uri
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
        -- Parse a standard hyperlink format
        case String.split ";" str of
            -- Should be ["8", params, uri]
            "8" :: params :: uri :: [] ->
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


parseChar : String -> Parser a -> Parser a
parseChar char parser =
    case parser of
        Parser (Unescaped str) hasLink model update ->
            case char of
                "\u{000D}" ->
                    Parser (Unescaped "") hasLink (update CarriageReturn (completeUnescaped parser)) update

                "\n" ->
                    Parser (Unescaped "") hasLink (update Linebreak (completeUnescaped parser)) update

                "\u{001B}" ->
                    Parser Escaped hasLink (completeUnescaped parser) update

                _ ->
                    Parser (Unescaped (str ++ char)) hasLink model update

        Parser Escaped hasLink model update ->
            case char of
                "[" ->
                    Parser (CSI [] Nothing) hasLink model update

                "]" ->
                    Parser (OSC "") hasLink model update

                "\\" ->
                    case parser of
                        Parser (OSCTerminating chars) _ _ _ ->
                            -- Complete the OSC sequence (ESC+backslash terminator)
                            let
                                (actions, newHasLink) = parseOSCWithState hasLink chars
                            in
                            completeBracketed parser newHasLink actions

                        _ ->
                            -- Not an OSC terminator, just backslash
                            Parser (Unescaped "\\") hasLink model update

                _ ->
                    Parser (Unescaped char) hasLink model update

        Parser (OSC chars) hasLink model update ->
            case char of
                "\u{0007}" ->
                    -- BEL character marks the end of an OSC sequence
                    let
                        (actions, newHasLink) = parseOSCWithState hasLink chars
                    in
                    completeBracketed parser newHasLink actions

                "\u{001B}" ->
                    -- Possibly the start of an ESC+backslash terminator
                    Parser (OSCTerminating chars) hasLink model update

                _ ->
                    -- Accumulate characters in the OSC sequence
                    Parser (OSC (chars ++ char)) hasLink model update

        Parser (OSCTerminating chars) hasLink model update ->
            case char of
                "\\" ->
                    -- ESC+backslash terminator found
                    let
                        (actions, newHasLink) = parseOSCWithState hasLink chars
                    in
                    completeBracketed parser newHasLink actions

                _ ->
                    -- Not a terminator, continue as a normal OSC sequence with ESC included
                    Parser (OSC (chars ++ "\u{001B}" ++ char)) hasLink model update

        Parser (CSI codes currentCode) hasLink model update ->
            case char of
                "m" ->
                    completeBracketed parser hasLink <|
                        captureArguments <|
                            List.map (Maybe.withDefault 0) (codes ++ [ currentCode ])

                "A" ->
                    completeBracketed parser hasLink
                        [ CursorUp (Maybe.withDefault 1 currentCode) ]

                "B" ->
                    completeBracketed parser hasLink
                        [ CursorDown (Maybe.withDefault 1 currentCode) ]

                "C" ->
                    completeBracketed parser hasLink
                        [ CursorForward (Maybe.withDefault 1 currentCode) ]

                "D" ->
                    completeBracketed parser hasLink
                        [ CursorBack (Maybe.withDefault 1 currentCode) ]

                "E" ->
                    completeBracketed parser hasLink
                        [ CursorDown (Maybe.withDefault 1 currentCode), CursorColumn 0 ]

                "F" ->
                    completeBracketed parser hasLink
                        [ CursorUp (Maybe.withDefault 1 currentCode), CursorColumn 0 ]

                "G" ->
                    completeBracketed parser hasLink
                        [ CursorColumn (Maybe.withDefault 0 currentCode) ]

                "H" ->
                    completeBracketed parser hasLink <|
                        cursorPosition (codes ++ [ currentCode ])

                "J" ->
                    completeBracketed parser hasLink
                        [ EraseDisplay (eraseMode (Maybe.withDefault 0 currentCode)) ]

                "K" ->
                    completeBracketed parser hasLink
                        [ EraseLine (eraseMode (Maybe.withDefault 0 currentCode)) ]

                "f" ->
                    completeBracketed parser hasLink <|
                        cursorPosition (codes ++ [ currentCode ])

                "s" ->
                    completeBracketed parser hasLink [ SaveCursorPosition ]

                "u" ->
                    completeBracketed parser hasLink [ RestoreCursorPosition ]

                ";" ->
                    Parser (CSI (codes ++ [ currentCode ]) Nothing) hasLink model update

                c ->
                    case String.toInt c of
                        Just num ->
                            Parser (CSI codes (Just ((Maybe.withDefault 0 currentCode * 10) + num))) hasLink model update

                        Nothing ->
                            completeBracketed parser hasLink []


completeUnescaped : Parser a -> a
completeUnescaped parser =
    case parser of
        Parser (Unescaped "") _ model update ->
            model

        Parser (Unescaped str) _ model update ->
            update (Print str) model

        -- should be impossible
        Parser _ _ model _ ->
            model


completeBracketed : Parser a -> Bool -> List Action -> Parser a
completeBracketed (Parser _ hasLink model update) newHasLink actions =
    Parser (Unescaped "") newHasLink (List.foldl update model actions) update


cursorPosition : List (Maybe Int) -> List Action
cursorPosition codes =
    case codes of
        [ Nothing, Nothing ] ->
            [ CursorPosition 1 1 ]

        [ Nothing ] ->
            [ CursorPosition 1 1 ]

        [ Just row, Nothing ] ->
            [ CursorPosition row 1 ]

        [ Nothing, Just col ] ->
            [ CursorPosition 1 col ]

        [ Just row, Just col ] ->
            [ CursorPosition row col ]

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
    , SetFraktur False
    , SetFramed False
    ]
