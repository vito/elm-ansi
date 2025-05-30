module Tests exposing (all, assertWindowRendersAs, colorCode, esc, log, parsing, renderChunk, renderLine, renderWindow, styleFlags)

import Ansi
import Ansi.Log
import Array
import Expect
import Regex
import String
import Test exposing (..)


all : Test
all =
    describe "ANSI" [ parsing, log, hyperlinkTests ]


parsing : Test
parsing =
    describe "Parsing"
        [ test "basic foreground and background colors" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal"
                    , Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.Print "red fg"
                    , Ansi.SetBackground (Just Ansi.Green)
                    , Ansi.Print "green bg"
                    , Ansi.SetForeground (Just Ansi.BrightRed)
                    , Ansi.Print "bright red fg"
                    , Ansi.SetBackground (Just Ansi.BrightGreen)
                    , Ansi.Print "bright green bg"
                    ]
                    (Ansi.parse "normal\u{001B}[31mred fg\u{001B}[42mgreen bg\u{001B}[91mbright red fg\u{001B}[102mbright green bg")
        , test "single argument colors (38/48;5;n format)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal"
                    , Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.Print "red fg"
                    , Ansi.SetBackground (Just Ansi.Green)
                    , Ansi.Print "green bg"
                    , Ansi.SetForeground (Just Ansi.BrightRed)
                    , Ansi.Print "bright red fg"
                    , Ansi.SetBackground (Just Ansi.BrightGreen)
                    , Ansi.Print "bright green bg"
                    ]
                    (Ansi.parse "normal\u{001B}[38;5;1mred fg\u{001B}[48;5;2mgreen bg\u{001B}[38;5;9mbright red fg\u{001B}[48;5;10mbright green bg")
        , test "8-bit colors" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal"
                    , Ansi.SetForeground (Just <| Ansi.Custom 0 215 95)
                    , Ansi.Print "green fg"
                    , Ansi.SetBackground (Just <| Ansi.Custom 255 95 0)
                    , Ansi.Print "orange bg"
                    ]
                    (Ansi.parse "normal\u{001B}[38;5;41mgreen fg\u{001B}[48;5;202morange bg")
        , test "24-bit colors" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal"
                    , Ansi.SetForeground (Just <| Ansi.Custom 123 15 51)
                    , Ansi.Print "custom fg"
                    , Ansi.SetBackground (Just <| Ansi.Custom 55 66 77)
                    , Ansi.Print "custom bg"
                    , Ansi.SetBackground (Just <| Ansi.Custom 255 0 255)
                    , Ansi.Print "clamped"
                    ]
                    (Ansi.parse "normal\u{001B}[38;2;123;15;51mcustom fg\u{001B}[48;2;55;66;77mcustom bg\u{001B}[48;2;1000;0;255mclamped")
        , test "text styling" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal"
                    , Ansi.SetBold True
                    , Ansi.Print "bold"
                    , Ansi.SetFaint True
                    , Ansi.Print "faint"
                    , Ansi.SetItalic True
                    , Ansi.Print "italic"
                    , Ansi.SetUnderline True
                    , Ansi.Print "underline"
                    , Ansi.SetBlink True
                    , Ansi.Print "blink"
                    , Ansi.Print "fast blink"
                    , Ansi.SetInverted True
                    , Ansi.Print "inverted"
                    ]
                    (Ansi.parse "normal\u{001B}[1mbold\u{001B}[2mfaint\u{001B}[3mitalic\u{001B}[4munderline\u{001B}[5mblink\u{001B}[6mfast blink\u{001B}[7minverted")
        , test "resetting styles" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "some text"
                    , Ansi.SetForeground Nothing
                    , Ansi.SetBackground Nothing
                    , Ansi.SetBold False
                    , Ansi.SetFaint False
                    , Ansi.SetItalic False
                    , Ansi.SetUnderline False
                    , Ansi.SetBlink False
                    , Ansi.SetInverted False
                    , Ansi.SetFraktur False
                    , Ansi.SetFramed False
                    , Ansi.Print "reset"
                    ]
                    (Ansi.parse "some text\u{001B}[0mreset")
        , test "partial resetting" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "some text"
                    , Ansi.SetBold False
                    , Ansi.Print "not bold"
                    , Ansi.SetFaint False
                    , Ansi.SetBold False
                    , Ansi.Print "not intense"
                    , Ansi.SetItalic False
                    , Ansi.SetFraktur False
                    , Ansi.Print "not italic/fraktur"
                    ]
                    (Ansi.parse "some text\u{001B}[21mnot bold\u{001B}[22mnot intense\u{001B}[23mnot italic/fraktur")
        , test "carriage returns and linebreaks" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "some text"
                    , Ansi.CarriageReturn
                    , Ansi.Linebreak
                    , Ansi.Print "next line"
                    , Ansi.CarriageReturn
                    , Ansi.Print "overwriting"
                    , Ansi.Linebreak
                    , Ansi.Print "shifted down"
                    ]
                    (Ansi.parse "some text\u{000D}\nnext line\u{000D}overwriting\nshifted down")
        , test "cursor movement" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorUp 5
                    , Ansi.CursorDown 1
                    , Ansi.CursorForward 50
                    , Ansi.CursorBack 1
                    , Ansi.CursorPosition 1 50
                    , Ansi.CursorPosition 50 1
                    ]
                    (Ansi.parse "\u{001B}[5A\u{001B}[B\u{001B}[50C\u{001B}[D\u{001B}[;50H\u{001B}[50;f")
        , test "cursor movement (not ANSI.SYS)" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorDown 1
                    , Ansi.CursorColumn 0
                    , Ansi.CursorUp 5
                    , Ansi.CursorColumn 0
                    , Ansi.CursorColumn 1
                    , Ansi.CursorColumn 50
                    ]
                    (Ansi.parse "\u{001B}[E\u{001B}[5F\u{001B}[1G\u{001B}[50G")
        , test "cursor position save/restore" <|
            \() ->
                Expect.equal
                    [ Ansi.SaveCursorPosition
                    , Ansi.RestoreCursorPosition
                    ]
                    (Ansi.parse "\u{001B}[s\u{001B}[u")
        , test "erasure" <|
            \() ->
                Expect.equal
                    [ Ansi.EraseDisplay Ansi.EraseToEnd
                    , Ansi.EraseDisplay Ansi.EraseToBeginning
                    , Ansi.EraseDisplay Ansi.EraseAll
                    , Ansi.EraseLine Ansi.EraseToEnd
                    , Ansi.EraseLine Ansi.EraseToBeginning
                    , Ansi.EraseLine Ansi.EraseAll
                    ]
                    (Ansi.parse "\u{001B}[J\u{001B}[1J\u{001B}[2J\u{001B}[K\u{001B}[1K\u{001B}[2K")
        , test "handling partial escape sequences" <|
            \() ->
                let
                    partial1 = Ansi.parse "foo\u{001B}"
                    partial2 = Ansi.parse "foo\u{001B}["
                    partial3 = Ansi.parse "foo\u{001B}[31;32"
                in
                Expect.equal
                    [
                      [ Ansi.Print "foo", Ansi.Remainder "\u{001B}" ],
                      [ Ansi.Print "foo", Ansi.Remainder "\u{001B}[" ],
                      [ Ansi.Print "foo", Ansi.Remainder "\u{001B}[31;32" ]
                    ]
                    [ partial1, partial2, partial3 ]
        , test "handling invalid escape sequences" <|
            \() ->
                let
                    invalid1 = Ansi.parse "foo\u{001B}lol"
                    invalid2 = Ansi.parse "foo\u{001B}[[lol"
                    unknown = Ansi.parse "foo\u{001B}[1Zbar"
                in
                Expect.equal
                    [
                      [ Ansi.Print "foo", Ansi.Print "lol" ],
                      [ Ansi.Print "foo", Ansi.Print "lol" ],
                      [ Ansi.Print "foo", Ansi.Print "bar" ]
                    ]
                    [ invalid1, invalid2, unknown ]
        ]


hyperlinkTests : Test
hyperlinkTests =
    describe "Hyperlink Parsing and Rendering"
        [ describe "Hyperlink Parsing"
            [ test "hyperlink with BEL and ESC backslash terminators" <|
                \() ->
                    let
                        withBel = Ansi.parse "normal text \u{001B}]8;;https://example.com\u{0007}link text\u{001B}]8;;\u{0007} more text"
                        withEsc = Ansi.parse "normal text \u{001B}]8;;https://example.com\u{001B}\\link text\u{001B}]8;;\u{001B}\\ more text"
                    in
                    Expect.equal
                        [
                          [ Ansi.Print "normal text "
                          , Ansi.HyperlinkStart [] "https://example.com"
                          , Ansi.Print "link text"
                          , Ansi.HyperlinkEnd
                          , Ansi.Print " more text"
                          ],
                          [ Ansi.Print "normal text "
                          , Ansi.HyperlinkStart [] "https://example.com"
                          , Ansi.Print "link text"
                          , Ansi.HyperlinkEnd
                          , Ansi.Print " more text"
                          ]
                        ]
                        [ withBel, withEsc ]

            , test "hyperlinks with parameters" <|
                \() ->
                    let
                        withOneParam = Ansi.parse "\u{001B}]8;id=test;https://example.com\u{0007}link with id\u{001B}]8;;\u{0007}"
                        withMultiParams = Ansi.parse "\u{001B}]8;id=test:foo=bar;https://example.com\u{0007}link with multiple params\u{001B}]8;;\u{0007}"
                        withColonSeparated = Ansi.parse "\u{001B}]8;id=test:foo=bar:baz=quux;https://example.com\u{0007}link with multiple colon-separated params\u{001B}]8;;\u{0007}"
                    in
                    Expect.equal
                        [
                          [ Ansi.HyperlinkStart ["id=test"] "https://example.com"
                          , Ansi.Print "link with id"
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart ["id=test", "foo=bar"] "https://example.com"
                          , Ansi.Print "link with multiple params"
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart ["id=test", "foo=bar", "baz=quux"] "https://example.com"
                          , Ansi.Print "link with multiple colon-separated params"
                          , Ansi.HyperlinkEnd
                          ]
                        ]
                        [ withOneParam, withMultiParams, withColonSeparated ]

            , test "hyperlink with styling and complex interactions" <|
                \() ->
                    let
                        withStyling = Ansi.parse "normal \u{001B}[31m\u{001B}]8;;https://example.com\u{0007}red link\u{001B}]8;;\u{0007} text"
                        withMultipleLinks = Ansi.parse "\u{001B}]8;;https://example.com/1\u{0007}link1\u{001B}]8;;\u{0007} and \u{001B}]8;;https://example.com/2\u{0007}link2\u{001B}]8;;\u{0007}"
                        withNestedStyling = Ansi.parse "\u{001B}]8;;http://example.com/styled\u{0007}normal\u{001B}[1mbold\u{001B}[3mbold-italic\u{001B}]8;;\u{0007}"
                    in
                    Expect.equal
                        [
                          [ Ansi.Print "normal "
                          , Ansi.SetForeground (Just Ansi.Red)
                          , Ansi.HyperlinkStart [] "https://example.com"
                          , Ansi.Print "red link"
                          , Ansi.HyperlinkEnd
                          , Ansi.Print " text"
                          ],
                          [ Ansi.HyperlinkStart [] "https://example.com/1"
                          , Ansi.Print "link1"
                          , Ansi.HyperlinkEnd
                          , Ansi.Print " and "
                          , Ansi.HyperlinkStart [] "https://example.com/2"
                          , Ansi.Print "link2"
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart [] "http://example.com/styled"
                          , Ansi.Print "normal"
                          , Ansi.SetBold True
                          , Ansi.Print "bold"
                          , Ansi.SetItalic True
                          , Ansi.Print "bold-italic"
                          , Ansi.HyperlinkEnd
                          ]
                        ]
                        [ withStyling, withMultipleLinks, withNestedStyling ]

            , test "special cases and edge conditions" <|
                \() ->
                    let
                        partialOSC = Ansi.parse "\u{001B}]8;;https://example.com"
                        veryLongUrl = Ansi.parse "\u{001B}]8;;http://example.com/very/long/url/that/continues/for/quite/some/time/to/test/handling/of/lengthy/urls\u{0007}long url link\u{001B}]8;;\u{0007}"
                        specialSchemes = Ansi.parse "\u{001B}]8;;file:///usr/share/icons/Adwaita/256x256/apps/preferences-desktop-theme.png\u{0007}File link\u{001B}]8;;\u{0007}"
                        withMultiLine = Ansi.parse "\u{001B}]8;;http://example.com/multiline\u{0007}first line\nsecond line\u{001B}]8;;\u{0007}"
                        withCursorMove = Ansi.parse "\u{001B}]8;;http://example.com/cursor\u{0007}move\u{001B}[C\u{001B}[C\u{001B}[Cright\u{001B}]8;;\u{0007}"
                    in
                    Expect.equal
                        [
                          [ Ansi.Remainder "\u{001B}]8;;https://example.com" ],
                          [ Ansi.HyperlinkStart [] "http://example.com/very/long/url/that/continues/for/quite/some/time/to/test/handling/of/lengthy/urls"
                          , Ansi.Print "long url link"
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart [] "file:///usr/share/icons/Adwaita/256x256/apps/preferences-desktop-theme.png"
                          , Ansi.Print "File link"
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart [] "http://example.com/multiline"
                          , Ansi.Print "first line"
                          , Ansi.Linebreak
                          , Ansi.Print "second line"
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart [] "http://example.com/cursor"
                          , Ansi.Print "move"
                          , Ansi.CursorForward 1
                          , Ansi.CursorForward 1
                          , Ansi.CursorForward 1
                          , Ansi.Print "right"
                          , Ansi.HyperlinkEnd
                          ]
                        ]
                        [ partialOSC, veryLongUrl, specialSchemes, withMultiLine, withCursorMove ]
            ]

        , describe "Hyperlink Log Integration"
            [ test "hyperlink integration with log model" <|
                \() ->
                    let
                        model =
                            List.foldl Ansi.Log.update (Ansi.Log.init Ansi.Log.Raw)
                                ["\u{001B}]8;;https://example.com\u{0007}link text\u{001B}]8;;\u{0007}"]

                        firstLine =
                            Array.get 0 model.lines |> Maybe.withDefault ([], 0)

                        chunks =
                            Tuple.first firstLine

                        hasLink =
                            List.any (\chunk -> chunk.linkUrl == Just "https://example.com") chunks

                        styledModel =
                            List.foldl Ansi.Log.update (Ansi.Log.init Ansi.Log.Raw)
                                ["\u{001B}[31m\u{001B}]8;;https://example.com\u{0007}red link\u{001B}]8;;\u{0007}"]

                        styledFirstLine =
                            Array.get 0 styledModel.lines |> Maybe.withDefault ([], 0)

                        styledChunks =
                            Tuple.first styledFirstLine

                        hasStyledLink =
                            List.any (\chunk ->
                                chunk.linkUrl == Just "https://example.com" &&
                                chunk.style.foreground == Just Ansi.Red
                            ) styledChunks

                        idModel =
                            List.foldl Ansi.Log.update (Ansi.Log.init Ansi.Log.Raw)
                                ["\u{001B}]8;id=test;https://example.com\u{0007}link with id\u{001B}]8;;\u{0007}"]

                        idFirstLine =
                            Array.get 0 idModel.lines |> Maybe.withDefault ([], 0)

                        idChunks =
                            Tuple.first idFirstLine

                        hasIdParam =
                            List.any (\chunk -> chunk.linkParams == ["id=test"]) idChunks
                    in
                    Expect.all
                        [ \_ -> Expect.true "Should contain a chunk with the link URL" hasLink
                        , \_ -> Expect.true "Should contain a red-styled link" hasStyledLink
                        , \_ -> Expect.true "Should contain a chunk with id parameter" hasIdParam
                        ]
                        ()

            , test "multiple hyperlinks in one line" <|
                \() ->
                    let
                        model =
                            List.foldl Ansi.Log.update (Ansi.Log.init Ansi.Log.Raw)
                                ["\u{001B}]8;;link1\u{0007}first\u{001B}]8;;\u{0007} and \u{001B}]8;;link2\u{0007}second\u{001B}]8;;\u{0007}"]

                        firstLine =
                            Array.get 0 model.lines |> Maybe.withDefault ([], 0)

                        chunks =
                            Tuple.first firstLine

                        linkChunks =
                            List.filter (\chunk -> chunk.linkUrl /= Nothing) chunks
                    in
                    Expect.equal 2 (List.length linkChunks)
            ]
        ]


assertWindowRendersAs : Ansi.Log.LineDiscipline -> String -> List String -> Expect.Expectation
assertWindowRendersAs ldisc rendered updates =
    let
        window =
            List.foldl Ansi.Log.update (Ansi.Log.init ldisc) updates
    in
    Expect.equal (esc rendered) (esc <| renderWindow window)


esc : String -> String
esc =
    Regex.replace (Maybe.withDefault Regex.never <| Regex.fromString "\u{001B}") (always "\\e")


renderWindow : Ansi.Log.Model -> String
renderWindow window =
    String.join "\u{000D}\n" (Array.toList (Array.map renderLine window.lines))


renderLine : Ansi.Log.Line -> String
renderLine ( chunks, _ ) =
    String.join "" (List.foldl (\c l -> renderChunk c :: l) [] chunks)


renderChunk : Ansi.Log.Chunk -> String
renderChunk chunk =
    "\u{001B}[0m" ++ styleFlags chunk.style ++ chunk.text


styleFlags : Ansi.Log.Style -> String
styleFlags style =
    String.join ""
        [ case style.foreground of
            Nothing ->
                ""

            Just fg ->
                "\u{001B}[" ++ String.fromInt (30 + colorCode fg) ++ "m"
        , case style.background of
            Nothing ->
                ""

            Just bg ->
                "\u{001B}[" ++ String.fromInt (40 + colorCode bg) ++ "m"
        , if style.bold then
            "\u{001B}[1m"

          else
            ""
        , if style.faint then
            "\u{001B}[2m"

          else
            ""
        , if style.italic then
            "\u{001B}[3m"

          else
            ""
        , if style.underline then
            "\u{001B}[4m"

          else
            ""
        , if style.blink then
            "\u{001B}[5m"

          else
            ""
        , if style.inverted then
            "\u{001B}[7m"

          else
            ""
        ]


colorCode : Ansi.Color -> Int
colorCode color =
    case color of
        Ansi.Black ->
            0

        Ansi.Red ->
            1

        Ansi.Green ->
            2

        Ansi.Yellow ->
            3

        Ansi.Blue ->
            4

        Ansi.Magenta ->
            5

        Ansi.Cyan ->
            6

        Ansi.White ->
            7

        Ansi.BrightBlack ->
            60

        Ansi.BrightRed ->
            61

        Ansi.BrightGreen ->
            62

        Ansi.BrightYellow ->
            63

        Ansi.BrightBlue ->
            64

        Ansi.BrightMagenta ->
            65

        Ansi.BrightCyan ->
            66

        Ansi.BrightWhite ->
            67

        Ansi.Custom _ _ _ ->
            -- not supported in this format
            -1


log : Test
log =
    describe "Log"
        [ test "basic printing" <|
            \() -> assertWindowRendersAs Ansi.Log.Raw "\u{001B}[0mx" [ "x" ]
        , test "basic printing of multiple chunks" <|
            \() -> assertWindowRendersAs Ansi.Log.Raw "\u{001B}[0mxyz" [ "x", "y", "z" ]
        , test "colors" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m\u{001B}[31mred\u{001B}[0m\u{001B}[31m\u{001B}[41mred bg"
                    [ "\u{001B}[31mred\u{001B}[41mred bg" ]
        , test "resetting foreground" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m\u{001B}[31mred\u{001B}[0mwhite"
                    [ "\u{001B}[31mred\u{001B}[39mwhite" ]
        , test "resetting background" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m\u{001B}[41mred\u{001B}[0mwhite"
                    [ "\u{001B}[41mred\u{001B}[49mwhite" ]
        , test "text styling" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mnormal\u{001B}[0m\u{001B}[1mbold\u{001B}[0m\u{001B}[1m\u{001B}[2mfaint\u{001B}[0m\u{001B}[1m\u{001B}[2m\u{001B}[3mitalic\u{001B}[0m\u{001B}[1m\u{001B}[2m\u{001B}[3m\u{001B}[4munderline\u{001B}[0m\u{001B}[1m\u{001B}[2m\u{001B}[3m\u{001B}[4m\u{001B}[5mblink\u{001B}[0m\u{001B}[1m\u{001B}[2m\u{001B}[3m\u{001B}[4m\u{001B}[5m\u{001B}[7minverted"
                    [ "normal\u{001B}[1mbold\u{001B}[2mfaint\u{001B}[3mitalic\u{001B}[4munderline\u{001B}[5mblink\u{001B}[7minverted" ]
        , test "line overwriting" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m\u{001B}[31mred\u{001B}[0m baz"
                    [ "foo\u{000D}" ++ "bar baz\u{000D}\u{001B}[31mred" ]
        , test "new lines in raw mode" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m\u{001B}[41mfoo\u{000D}\n\u{001B}[0m\u{001B}[41m   bar baz\u{000D}\n\u{001B}[0m\u{001B}[41m          "
                    [ "\u{001B}[41mfoo\nbar baz\n" ]
        , test "new lines in cooked mode" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Cooked
                    "\u{001B}[0m\u{001B}[41mfoo\u{000D}\n\u{001B}[0m\u{001B}[41mbar baz\u{000D}\n"
                    [ "\u{001B}[41mfoo\nbar baz\n" ]
        , test "ansi escapes on boundaries" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m\u{001B}[41mfoo\u{001B}[0m\u{001B}[31m\u{001B}[41mbar baz"
                    [ "\u{001B}[4", "1mfoo", "\u{001B}", "[31mbar baz" ]
        , test "cursor movement" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mONE\u{000D}\n\u{001B}[0mtwo\u{000D}\n\u{001B}[0mthrxeE\u{000D}\n\u{001B}[0mf!!r\u{000D}\n\u{001B}[0mxyz"
                    [ "one\u{000D}\ntwo\u{000D}\nthree\u{000D}\nfour\u{000D}\nxyz\u{000D}"
                    , "\u{001B}[4AONE"
                    , "\u{001B}[2Bx"
                    , "\u{001B}[CE"
                    , "\u{001B}[B"
                    , "\u{001B}[5D!!"
                    ]
        , test "cursor movement (not ANSI.SYS)" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mONE\u{000D}\n\u{001B}[0mtwo\u{000D}\n\u{001B}[0mxyree\u{000D}\n\u{001B}[0mfour\u{000D}\n\u{001B}[0mxyz"
                    [ "one\u{000D}\ntwo\u{000D}\nthree\u{000D}\nfour\u{000D}\nxyz\u{000D}"
                    , "\u{001B}[4FONE"
                    , "\u{001B}[2Ex"
                    , "\u{001B}[1Gy"
                    ]
        , test "setting the cursor position" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mone\u{000D}\n\u{001B}[0mtwo\u{000D}\n\u{001B}[0mtHRee\u{000D}\n\u{001B}[0mfOUr\u{000D}\n\u{001B}[0mxyz"
                    [ "one\u{000D}\ntwo\u{000D}\nthree\u{000D}\nfour\u{000D}\nxyz\u{000D}"
                    , "\u{001B}[3;2HHR"
                    , "\u{001B}[4;2fOU"
                    ]
        , test "saving and restoring the cursor position" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mone\u{000D}\n\u{001B}[0mtwo\u{000D}\n\u{001B}[0mtHRee\u{000D}\n\u{001B}[0mfour\u{000D}\n\u{001B}[0mxyz"
                    [ "one\u{000D}\ntwo\u{000D}\nt\u{001B}[shree\u{000D}\nfour\u{000D}\nxyz\u{000D}"
                    , "\u{001B}[uHR"
                    ]
        , test "erasing line contents" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0moneTWO\u{000D}\n\u{001B}[0m   TWO\u{000D}\n\u{001B}[0m      THREEFOUR\u{000D}\n"
                    [ "onetwenty\u{001B}[6D\u{001B}[KTWO\u{000D}\n"
                    , "onetwo\u{001B}[3D\u{001B}[1KTWO\u{000D}\n"
                    , "onetwo\u{001B}[2KTHREEFOUR\u{000D}\n"
                    ]
        ]