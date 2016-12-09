module Tests exposing (..)

import Test exposing (..)
import Expect
import Array
import Regex
import String
import Ansi
import Ansi.Log


all : Test
all =
    describe "ANSI" [ parsing, log ]


parsing : Test
parsing =
    describe "Parsing"
        [ test "colors" <|
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
                    (Ansi.parse "normal\x1B[31mred fg\x1B[42mgreen bg\x1B[91mbright red fg\x1B[102mbright green bg")
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
                    (Ansi.parse "normal\x1B[1mbold\x1B[2mfaint\x1B[3mitalic\x1B[4munderline\x1B[5mblink\x1B[6mfast blink\x1B[7minverted")
        , test "resetting" <|
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
                    , Ansi.Print "reset"
                    , Ansi.SetForeground Nothing
                    , Ansi.SetBackground Nothing
                    , Ansi.SetBold False
                    , Ansi.SetFaint False
                    , Ansi.SetItalic False
                    , Ansi.SetUnderline False
                    , Ansi.SetBlink False
                    , Ansi.SetInverted False
                    , Ansi.Print "reset again"
                    , Ansi.SetForeground Nothing
                    , Ansi.SetBackground Nothing
                    , Ansi.SetBold False
                    , Ansi.SetFaint False
                    , Ansi.SetItalic False
                    , Ansi.SetUnderline False
                    , Ansi.SetBlink False
                    , Ansi.SetInverted False
                    , Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.Print "reset to red"
                    ]
                    (Ansi.parse "some text\x1B[0mreset\x1B[mreset again\x1B[;31mreset to red")
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
                    (Ansi.parse "some text\x0D\nnext line\x0Doverwriting\nshifted down")
        , test "cursor movement" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorUp 5
                    , Ansi.CursorUp 50
                    , Ansi.CursorUp 1
                    , Ansi.CursorDown 5
                    , Ansi.CursorDown 50
                    , Ansi.CursorDown 1
                    , Ansi.CursorForward 5
                    , Ansi.CursorForward 50
                    , Ansi.CursorForward 1
                    , Ansi.CursorBack 5
                    , Ansi.CursorBack 50
                    , Ansi.CursorBack 1
                    , Ansi.CursorPosition 1 50
                    , Ansi.CursorPosition 50 1
                    , Ansi.CursorPosition 1 1
                    , Ansi.CursorPosition 1 1
                    , Ansi.CursorPosition 50 50
                    , Ansi.CursorPosition 1 50
                    , Ansi.CursorPosition 50 1
                    , Ansi.CursorPosition 1 1
                    , Ansi.CursorPosition 1 1
                    , Ansi.CursorPosition 50 50
                    ]
                    (Ansi.parse "\x1B[5A\x1B[50A\x1B[A\x1B[5B\x1B[50B\x1B[B\x1B[5C\x1B[50C\x1B[C\x1B[5D\x1B[50D\x1B[D\x1B[;50H\x1B[50;H\x1B[H\x1B[;H\x1B[50;50H\x1B[;50f\x1B[50;f\x1B[f\x1B[;f\x1B[50;50f")
        , test "cursor movement (not ANSI.SYS)" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorDown 1
                    , Ansi.CursorColumn 0
                    , Ansi.CursorDown 5
                    , Ansi.CursorColumn 0
                    , Ansi.CursorDown 50
                    , Ansi.CursorColumn 0
                    , Ansi.CursorUp 1
                    , Ansi.CursorColumn 0
                    , Ansi.CursorUp 5
                    , Ansi.CursorColumn 0
                    , Ansi.CursorUp 50
                    , Ansi.CursorColumn 0
                    , Ansi.CursorColumn 0
                    , Ansi.CursorColumn 0
                    , Ansi.CursorColumn 1
                    , Ansi.CursorColumn 5
                    , Ansi.CursorColumn 50
                    ]
                    (Ansi.parse "\x1B[E\x1B[5E\x1B[50E\x1B[F\x1B[5F\x1B[50F\x1B[G\x1B[0G\x1B[1G\x1B[5G\x1B[50G")
        , test "cursor position save/restore" <|
            \() ->
                Expect.equal
                    [ Ansi.SaveCursorPosition
                    , Ansi.RestoreCursorPosition
                    ]
                    (Ansi.parse "\x1B[s\x1B[u")
        , test "erasure" <|
            \() ->
                Expect.equal
                    [ Ansi.EraseDisplay Ansi.EraseToEnd
                    , Ansi.EraseDisplay Ansi.EraseToEnd
                    , Ansi.EraseDisplay Ansi.EraseToBeginning
                    , Ansi.EraseDisplay Ansi.EraseAll
                    , Ansi.EraseLine Ansi.EraseToEnd
                    , Ansi.EraseLine Ansi.EraseToEnd
                    , Ansi.EraseLine Ansi.EraseToBeginning
                    , Ansi.EraseLine Ansi.EraseAll
                    ]
                    (Ansi.parse "\x1B[J\x1B[0J\x1B[1J\x1B[2J\x1B[K\x1B[0K\x1B[1K\x1B[2K")
        , test "partial escape sequence" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "foo", Ansi.Remainder "\x1B" ]
                    (Ansi.parse "foo\x1B")
        , test "partial escape sequence with bracket" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "foo", Ansi.Remainder "\x1B[" ]
                    (Ansi.parse "foo\x1B[")
        , test "partial escape sequence with bracket and codes" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "foo", Ansi.Remainder "\x1B[31;32" ]
                    (Ansi.parse "foo\x1B[31;32")
        , test "invalid escape sequences (no bracket)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "foo", Ansi.Print "lol" ]
                    (Ansi.parse "foo\x1Blol")
        , test "invalid escape sequences (double bracket)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "foo", Ansi.Print "lol" ]
                    (Ansi.parse "foo\x1B[[lol")
        , test "unknown escape sequences" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "foo", Ansi.Print "bar" ]
                    (Ansi.parse "foo\x1B[1Zbar")
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
    Regex.replace Regex.All (Regex.regex "\x1B") (always "\\e")


renderWindow : Ansi.Log.Model -> String
renderWindow window =
    String.join "\x0D\n" (Array.toList (Array.map renderLine window.lines))


renderLine : Ansi.Log.Line -> String
renderLine ( chunks, _ ) =
    String.join "" (List.foldl (\c l -> renderChunk c :: l) [] chunks)


renderChunk : Ansi.Log.Chunk -> String
renderChunk chunk =
    "\x1B[0m" ++ styleFlags chunk.style ++ chunk.text


styleFlags : Ansi.Log.Style -> String
styleFlags style =
    String.join ""
        [ case style.foreground of
            Nothing ->
                ""

            Just fg ->
                "\x1B[" ++ toString (30 + colorCode fg) ++ "m"
        , case style.background of
            Nothing ->
                ""

            Just bg ->
                "\x1B[" ++ toString (40 + colorCode bg) ++ "m"
        , if style.bold then
            "\x1B[1m"
          else
            ""
        , if style.faint then
            "\x1B[2m"
          else
            ""
        , if style.italic then
            "\x1B[3m"
          else
            ""
        , if style.underline then
            "\x1B[4m"
          else
            ""
        , if style.blink then
            "\x1B[5m"
          else
            ""
        , if style.inverted then
            "\x1B[7m"
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


log : Test
log =
    describe "Log"
        [ test "basic printing" <|
            \() -> assertWindowRendersAs Ansi.Log.Raw "\x1B[0mx" [ "x" ]
        , test "basic printing of multiple chunks" <|
            \() -> assertWindowRendersAs Ansi.Log.Raw "\x1B[0mxyz" [ "x", "y", "z" ]
        , test "colors" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0m\x1B[31mred\x1B[0m\x1B[31m\x1B[41mred bg"
                    [ "\x1B[31mred\x1B[41mred bg" ]
        , test "resetting foreground" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0m\x1B[31mred\x1B[0mwhite"
                    [ "\x1B[31mred\x1B[39mwhite" ]
        , test "resetting background" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0m\x1B[41mred\x1B[0mwhite"
                    [ "\x1B[41mred\x1B[49mwhite" ]
        , test "text styling" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0mnormal\x1B[0m\x1B[1mbold\x1B[0m\x1B[1m\x1B[2mfaint\x1B[0m\x1B[1m\x1B[2m\x1B[3mitalic\x1B[0m\x1B[1m\x1B[2m\x1B[3m\x1B[4munderline\x1B[0m\x1B[1m\x1B[2m\x1B[3m\x1B[4m\x1B[5mblink\x1B[0m\x1B[1m\x1B[2m\x1B[3m\x1B[4m\x1B[5m\x1B[7minverted"
                    [ "normal\x1B[1mbold\x1B[2mfaint\x1B[3mitalic\x1B[4munderline\x1B[5mblink\x1B[7minverted" ]
        , test "line overwriting" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0m\x1B[31mred\x1B[0m baz"
                    [ "foo\x0D" ++ "bar baz\x0D\x1B[31mred" ]
        , test "new lines in raw mode" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0m\x1B[41mfoo\x0D\n\x1B[0m\x1B[41m   bar baz\x0D\n\x1B[0m\x1B[41m          "
                    [ "\x1B[41mfoo\nbar baz\n" ]
        , test "new lines in cooked mode" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Cooked
                    "\x1B[0m\x1B[41mfoo\x0D\n\x1B[0m\x1B[41mbar baz\x0D\n"
                    [ "\x1B[41mfoo\nbar baz\n" ]
        , test "ansi escapes on boundaries" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0m\x1B[41mfoo\x1B[0m\x1B[31m\x1B[41mbar baz"
                    [ "\x1B[4", "1mfoo", "\x1B", "[31mbar baz" ]
        , test "cursor movement" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0mONE\x0D\n\x1B[0mtwo\x0D\n\x1B[0mthrxeE\x0D\n\x1B[0mf!!r\x0D\n\x1B[0mxyz"
                    [ "one\x0D\ntwo\x0D\nthree\x0D\nfour\x0D\nxyz\x0D"
                    , "\x1B[4AONE"
                    , "\x1B[2Bx"
                    , "\x1B[CE"
                    , "\x1B[B"
                    , "\x1B[5D!!"
                    ]
        , test "cursor movement (not ANSI.SYS)" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0mONE\x0D\n\x1B[0mtwo\x0D\n\x1B[0mxyree\x0D\n\x1B[0mfour\x0D\n\x1B[0mxyz"
                    [ "one\x0D\ntwo\x0D\nthree\x0D\nfour\x0D\nxyz\x0D"
                    , "\x1B[4FONE"
                    , "\x1B[2Ex"
                    , "\x1B[1Gy"
                    ]
        , test "setting the cursor position" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0mone\x0D\n\x1B[0mtwo\x0D\n\x1B[0mtHRee\x0D\n\x1B[0mfOUr\x0D\n\x1B[0mxyz"
                    [ "one\x0D\ntwo\x0D\nthree\x0D\nfour\x0D\nxyz\x0D"
                    , "\x1B[3;2HHR"
                    , "\x1B[4;2fOU"
                    ]
        , test "saving and restoring the cursor position" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0mone\x0D\n\x1B[0mtwo\x0D\n\x1B[0mtHRee\x0D\n\x1B[0mfour\x0D\n\x1B[0mxyz"
                    [ "one\x0D\ntwo\x0D\nt\x1B[shree\x0D\nfour\x0D\nxyz\x0D"
                    , "\x1B[uHR"
                    ]
        , test "erasing line contents" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\x1B[0moneTWO\x0D\n\x1B[0m   TWO\x0D\n\x1B[0m      THREEFOUR\x0D\n"
                    [ "onetwenty\x1B[6D\x1B[KTWO\x0D\n"
                    , "onetwo\x1B[3D\x1B[1KTWO\x0D\n"
                    , "onetwo\x1B[2KTHREEFOUR\x0D\n"
                    ]
        ]
