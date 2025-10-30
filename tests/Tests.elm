module Tests exposing (all, assertWindowRendersAs, colorCode, displayWidthTests, esc, log, parsing, renderChunk, renderLine, renderWindow, styleFlags)

import Ansi
import Ansi.Log
import Array
import Expect
import Regex
import String
import Test exposing (..)


all : Test
all =
    describe "ANSI" [ parsing, log, hyperlinkTests, displayWidthTests ]


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
                    , Ansi.SetStrikethrough True
                    , Ansi.Print "strikethrough"
                    ]
                    (Ansi.parse "normal\u{001B}[1mbold\u{001B}[2mfaint\u{001B}[3mitalic\u{001B}[4munderline\u{001B}[5mblink\u{001B}[6mfast blink\u{001B}[7minverted\u{001B}[9mstrikethrough")
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
                    , Ansi.SetStrikethrough False
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
        , test "fraktur style" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal"
                    , Ansi.SetFraktur True
                    , Ansi.Print "fraktur"
                    , Ansi.SetItalic False
                    , Ansi.SetFraktur False
                    , Ansi.Print "not fraktur"
                    ]
                    (Ansi.parse "normal\u{001B}[20mfraktur\u{001B}[23mnot fraktur")
        , test "framed style" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal"
                    , Ansi.SetFramed True
                    , Ansi.Print "framed"
                    , Ansi.SetFramed False
                    , Ansi.Print "not framed"
                    ]
                    (Ansi.parse "normal\u{001B}[51mframed\u{001B}[54mnot framed")
        , test "fast blink (code 6) - not implemented, no action" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal"
                    , Ansi.Print "fast blink"
                    ]
                    (Ansi.parse "normal\u{001B}[6mfast blink")
        , test "all standard foreground colors" <|
            \() ->
                Expect.equal
                    [ Ansi.SetForeground (Just Ansi.Black)
                    , Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.SetForeground (Just Ansi.Green)
                    , Ansi.SetForeground (Just Ansi.Yellow)
                    , Ansi.SetForeground (Just Ansi.Blue)
                    , Ansi.SetForeground (Just Ansi.Magenta)
                    , Ansi.SetForeground (Just Ansi.Cyan)
                    , Ansi.SetForeground (Just Ansi.White)
                    ]
                    (Ansi.parse "\u{001B}[30m\u{001B}[31m\u{001B}[32m\u{001B}[33m\u{001B}[34m\u{001B}[35m\u{001B}[36m\u{001B}[37m")
        , test "all standard background colors" <|
            \() ->
                Expect.equal
                    [ Ansi.SetBackground (Just Ansi.Black)
                    , Ansi.SetBackground (Just Ansi.Red)
                    , Ansi.SetBackground (Just Ansi.Green)
                    , Ansi.SetBackground (Just Ansi.Yellow)
                    , Ansi.SetBackground (Just Ansi.Blue)
                    , Ansi.SetBackground (Just Ansi.Magenta)
                    , Ansi.SetBackground (Just Ansi.Cyan)
                    , Ansi.SetBackground (Just Ansi.White)
                    ]
                    (Ansi.parse "\u{001B}[40m\u{001B}[41m\u{001B}[42m\u{001B}[43m\u{001B}[44m\u{001B}[45m\u{001B}[46m\u{001B}[47m")
        , test "all bright foreground colors" <|
            \() ->
                Expect.equal
                    [ Ansi.SetForeground (Just Ansi.BrightBlack)
                    , Ansi.SetForeground (Just Ansi.BrightRed)
                    , Ansi.SetForeground (Just Ansi.BrightGreen)
                    , Ansi.SetForeground (Just Ansi.BrightYellow)
                    , Ansi.SetForeground (Just Ansi.BrightBlue)
                    , Ansi.SetForeground (Just Ansi.BrightMagenta)
                    , Ansi.SetForeground (Just Ansi.BrightCyan)
                    , Ansi.SetForeground (Just Ansi.BrightWhite)
                    ]
                    (Ansi.parse "\u{001B}[90m\u{001B}[91m\u{001B}[92m\u{001B}[93m\u{001B}[94m\u{001B}[95m\u{001B}[96m\u{001B}[97m")
        , test "all bright background colors" <|
            \() ->
                Expect.equal
                    [ Ansi.SetBackground (Just Ansi.BrightBlack)
                    , Ansi.SetBackground (Just Ansi.BrightRed)
                    , Ansi.SetBackground (Just Ansi.BrightGreen)
                    , Ansi.SetBackground (Just Ansi.BrightYellow)
                    , Ansi.SetBackground (Just Ansi.BrightBlue)
                    , Ansi.SetBackground (Just Ansi.BrightMagenta)
                    , Ansi.SetBackground (Just Ansi.BrightCyan)
                    , Ansi.SetBackground (Just Ansi.BrightWhite)
                    ]
                    (Ansi.parse "\u{001B}[100m\u{001B}[101m\u{001B}[102m\u{001B}[103m\u{001B}[104m\u{001B}[105m\u{001B}[106m\u{001B}[107m")
        , test "color reset codes" <|
            \() ->
                Expect.equal
                    [ Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.Print "red"
                    , Ansi.SetForeground Nothing
                    , Ansi.Print "default fg"
                    , Ansi.SetBackground (Just Ansi.Blue)
                    , Ansi.Print "blue bg"
                    , Ansi.SetBackground Nothing
                    , Ansi.Print "default bg"
                    ]
                    (Ansi.parse "\u{001B}[31mred\u{001B}[39mdefault fg\u{001B}[44mblue bg\u{001B}[49mdefault bg")
        , test "individual style reset codes" <|
            \() ->
                Expect.equal
                    [ Ansi.SetUnderline True
                    , Ansi.Print "underlined"
                    , Ansi.SetUnderline False
                    , Ansi.Print "not underlined"
                    , Ansi.SetBlink True
                    , Ansi.Print "blinking"
                    , Ansi.SetBlink False
                    , Ansi.Print "not blinking"
                    , Ansi.SetInverted True
                    , Ansi.Print "inverted"
                    , Ansi.SetInverted False
                    , Ansi.Print "not inverted"
                    , Ansi.SetStrikethrough True
                    , Ansi.Print "struck"
                    , Ansi.SetStrikethrough False
                    , Ansi.Print "not struck"
                    ]
                    (Ansi.parse "\u{001B}[4munderlined\u{001B}[24mnot underlined\u{001B}[5mblinking\u{001B}[25mnot blinking\u{001B}[7minverted\u{001B}[27mnot inverted\u{001B}[9mstruck\u{001B}[29mnot struck")
        , test "cursor movement with default parameters" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorUp 1
                    , Ansi.CursorDown 1
                    , Ansi.CursorForward 1
                    , Ansi.CursorBackward 1
                    ]
                    (Ansi.parse "\u{001B}[A\u{001B}[B\u{001B}[C\u{001B}[D")
        , test "cursor movement with explicit zero defaults to 1" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorUp 1
                    , Ansi.CursorDown 1
                    , Ansi.CursorForward 1
                    , Ansi.CursorBackward 1
                    ]
                    (Ansi.parse "\u{001B}[0A\u{001B}[0B\u{001B}[0C\u{001B}[0D")
        , test "ESC[C is equivalent to ESC[1C and ESC[0C" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorForward 1
                    , Ansi.CursorForward 1
                    , Ansi.CursorForward 1
                    ]
                    (Ansi.parse "\u{001B}[C\u{001B}[1C\u{001B}[0C")
        , test "cursor position variations" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorPosition 1 1
                    , Ansi.CursorPosition 10 1
                    , Ansi.CursorPosition 1 20
                    , Ansi.CursorPosition 15 25
                    ]
                    (Ansi.parse "\u{001B}[H\u{001B}[10H\u{001B}[;20H\u{001B}[15;25H")
        , test "cursor position with single row parameter" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorPosition 10 1
                    , Ansi.CursorPosition 5 1
                    , Ansi.CursorPosition 1 1
                    ]
                    (Ansi.parse "\u{001B}[10H\u{001B}[5H\u{001B}[1H")
        , test "cursor position with only column parameter" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorPosition 1 10
                    , Ansi.CursorPosition 1 5
                    , Ansi.CursorPosition 1 1
                    ]
                    (Ansi.parse "\u{001B}[;10H\u{001B}[;5H\u{001B}[;1H")
        , test "cursor position with 0 values" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorPosition 1 1
                    , Ansi.CursorPosition 10 1
                    , Ansi.CursorPosition 1 10
                    ]
                    (Ansi.parse "\u{001B}[0;0H\u{001B}[10;0H\u{001B}[0;10H")
        , test "cursor position with f command" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorPosition 1 1
                    , Ansi.CursorPosition 5 10
                    ]
                    (Ansi.parse "\u{001B}[f\u{001B}[5;10f")
        , test "cursor column movement" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorColumn 0
                    , Ansi.CursorColumn 9
                    , Ansi.CursorColumn 49
                    ]
                    (Ansi.parse "\u{001B}[G\u{001B}[10G\u{001B}[50G")
        , test "cursor down with column reset (E)" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorDown 1
                    , Ansi.CursorColumn 0
                    , Ansi.CursorDown 3
                    , Ansi.CursorColumn 0
                    ]
                    (Ansi.parse "\u{001B}[E\u{001B}[3E")
        , test "cursor up with column reset (F)" <|
            \() ->
                Expect.equal
                    [ Ansi.CursorUp 1
                    , Ansi.CursorColumn 0
                    , Ansi.CursorUp 2
                    , Ansi.CursorColumn 0
                    ]
                    (Ansi.parse "\u{001B}[F\u{001B}[2F")
        , test "erase display modes" <|
            \() ->
                Expect.equal
                    [ Ansi.EraseDisplay Ansi.EraseToEnd
                    , Ansi.EraseDisplay Ansi.EraseToBeginning
                    , Ansi.EraseDisplay Ansi.EraseAll
                    , Ansi.EraseDisplay Ansi.EraseAll
                    ]
                    (Ansi.parse "\u{001B}[J\u{001B}[1J\u{001B}[2J\u{001B}[3J")
        , test "erase line modes" <|
            \() ->
                Expect.equal
                    [ Ansi.EraseLine Ansi.EraseToEnd
                    , Ansi.EraseLine Ansi.EraseToBeginning
                    , Ansi.EraseLine Ansi.EraseAll
                    , Ansi.EraseLine Ansi.EraseAll
                    ]
                    (Ansi.parse "\u{001B}[K\u{001B}[1K\u{001B}[2K\u{001B}[3K")
        , test "combined style codes" <|
            \() ->
                Expect.equal
                    [ Ansi.SetBold True
                    , Ansi.SetItalic True
                    , Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.Print "styled"
                    ]
                    (Ansi.parse "\u{001B}[1;3;31mstyled")
        , test "all styles combined in single sequence" <|
            \() ->
                Expect.equal
                    [ Ansi.SetBold True
                    , Ansi.SetFaint True
                    , Ansi.SetItalic True
                    , Ansi.SetUnderline True
                    , Ansi.SetBlink True
                    , Ansi.SetInverted True
                    , Ansi.SetStrikethrough True
                    , Ansi.SetFraktur True
                    , Ansi.SetFramed True
                    , Ansi.SetForeground (Just Ansi.Cyan)
                    , Ansi.SetBackground (Just Ansi.Magenta)
                    ]
                    (Ansi.parse "\u{001B}[1;2;3;4;5;7;9;20;51;36;45m")
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
                    , Ansi.CursorBackward 1
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
                    , Ansi.CursorColumn 0
                    , Ansi.CursorColumn 49
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
        , test "malformed 256-color sequences" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before"
                    , Ansi.Print "incomplete"
                    , Ansi.Print "ext"
                    ]
                    (Ansi.parse "before\u{001B}[38;5mincomplete\u{001B}[48;5text")
        , test "malformed 24-bit color sequences" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before"
                    , Ansi.Print "missing g and b"
                    , Ansi.Print "missing b"
                    ]
                    (Ansi.parse "before\u{001B}[38;2;255mmissing g and b\u{001B}[48;2;100;200mmissing b")
        , test "Concourse CI log sequence with unknown commands" <|
            \() ->
                -- Real-world case: ESC[r ESC[m ESC(B ESC[?1003l ESC[?1006l ESC[?2004l ESC[1;1H ESC[1;24r ESC[1;1H
                -- Unknown commands (r, ?...l) should be discarded, only recognized commands output
                -- ESC(B is a charset designation sequence and should be consumed/ignored
                Expect.equal
                    ([ Ansi.SetForeground Nothing
                    , Ansi.SetBackground Nothing
                    , Ansi.SetBold False
                    , Ansi.SetFaint False
                    , Ansi.SetItalic False
                    , Ansi.SetUnderline False
                    , Ansi.SetBlink False
                    , Ansi.SetInverted False
                    , Ansi.SetStrikethrough False
                    , Ansi.SetFraktur False
                    , Ansi.SetFramed False
                    , Ansi.CursorPosition 1 1
                    , Ansi.CursorPosition 1 1
                    ] )
                    (Ansi.parse "\u{001B}[r\u{001B}[m\u{001B}(B\u{001B}[?1003l\u{001B}[?1006l\u{001B}[?2004l\u{001B}[1;1H\u{001B}[1;24r\u{001B}[1;1H")
        , test "multiple SGR parameters in one sequence" <|
            \() ->
                Expect.equal
                    [ Ansi.SetBold True
                    , Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.SetBackground (Just Ansi.Blue)
                    , Ansi.Print "styled"
                    , Ansi.SetBold True
                    , Ansi.SetItalic True
                    , Ansi.SetUnderline True
                    , Ansi.Print "multi-style"
                    ]
                    (Ansi.parse "\u{001B}[1;31;44mstyled\u{001B}[1;3;4mmulti-style")
        , test "out-of-range SGR parameters are ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal"
                    , Ansi.Print "unchanged"
                    ]
                    (Ansi.parse "normal\u{001B}[999munchanged")
        , test "out-of-range 256-color values" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before"
                    , Ansi.Print "after"
                    ]
                    (Ansi.parse "before\u{001B}[38;5;256mafter")
        , test "CSI with < parameter marker is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before"
                    , Ansi.Print "after"
                    ]
                    (Ansi.parse "before\u{001B}[<1;2;3mafter")
        , test "CSI with = parameter marker is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before"
                    , Ansi.Print "after"
                    ]
                    (Ansi.parse "before\u{001B}[=5cafter")
        , test "CSI with > parameter marker is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before"
                    , Ansi.Print "after"
                    ]
                    (Ansi.parse "before\u{001B}[>cafter")
        , test "CSI with ? parameter marker is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before"
                    , Ansi.Print "after"
                    ]
                    (Ansi.parse "before\u{001B}[?1003lafter")
        , test "CSI < marker does not affect normal sequences" <|
            \() ->
                Expect.equal
                    [ Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.Print "red"
                    , Ansi.Print "ignored"
                    , Ansi.SetForeground (Just Ansi.Blue)
                    , Ansi.Print "blue"
                    ]
                    (Ansi.parse "\u{001B}[31mred\u{001B}[<10;20Mignored\u{001B}[34mblue")
        , test "CSI > marker with parameters is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text"
                    , Ansi.Print "more"
                    ]
                    (Ansi.parse "text\u{001B}[>1;2;3;4cmore")
        , test "CSI = marker with parameters is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text"
                    , Ansi.Print "more"
                    ]
                    (Ansi.parse "text\u{001B}[=7;8;9cmore")
        , test "CSI ? marker in middle of sequence is ignored" <|
            \() ->
                -- '?' should only be valid at start; if it appears after digits, ignore the whole sequence
                Expect.equal
                    [ Ansi.Print "text"
                    , Ansi.Print "more"
                    ]
                    (Ansi.parse "text\u{001B}[1?2mmore")
        , test "Multiple CSI sequences with different markers" <|
            \() ->
                Expect.equal
                    [ Ansi.SetBold True
                    , Ansi.Print "bold"
                    , Ansi.Print "ignored1"
                    , Ansi.SetForeground (Just Ansi.Green)
                    , Ansi.Print "green"
                    , Ansi.Print "ignored2"
                    , Ansi.SetBlink True
                    , Ansi.Print "blink"
                    ]
                    (Ansi.parse "\u{001B}[1mbold\u{001B}[<5Mignored1\u{001B}[32mgreen\u{001B}[>1cignored2\u{001B}[5mblink")
        , test "CSI marker in remainder (incomplete sequence)" <|
            \() ->
                -- Incomplete sequence with marker should be preserved in Remainder
                Expect.equal
                    [ Ansi.Print "text"
                    , Ansi.Remainder "\u{001B}[<12"
                    ]
                    (Ansi.parse "text\u{001B}[<12")
        , test "CSI marker followed by semicolon" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before"
                    , Ansi.Print "after"
                    ]
                    (Ansi.parse "before\u{001B}[>;1;2mafter")
        , test "charset designation ESC(B is consumed and ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before"
                    , Ansi.Print "after"
                    ]
                    (Ansi.parse "before\u{001B}(Bafter")
        , test "charset designation ESC)0 is consumed and ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text"
                    , Ansi.Print "more"
                    ]
                    (Ansi.parse "text\u{001B})0more")
        , test "charset designation ESC*A is consumed and ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "start"
                    , Ansi.Print "end"
                    ]
                    (Ansi.parse "start\u{001B}*Aend")
        , test "charset designation ESC+B is consumed and ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "foo"
                    , Ansi.Print "bar"
                    ]
                    (Ansi.parse "foo\u{001B}+Bbar")
        , test "multiple charset designations in sequence" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "a"
                    , Ansi.Print "b"
                    , Ansi.Print "c"
                    ]
                    (Ansi.parse "a\u{001B}(B\u{001B})0b\u{001B}*Ac")
        , test "charset designation mixed with CSI sequences" <|
            \() ->
                Expect.equal
                    [ Ansi.SetBold True
                    , Ansi.Print "bold"
                    , Ansi.Print "text"
                    ]
                    (Ansi.parse "\u{001B}[1mbold\u{001B}(Btext")
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
                        [ \_ ->
                            hasLink
                                |> Expect.equal True
                                |> Expect.onFail "Should contain a chunk with the link URL"
                        , \_ ->
                            hasStyledLink
                                |> Expect.equal True
                                |> Expect.onFail "Should contain a red-styled link"
                        , \_ ->
                            hasIdParam
                                |> Expect.equal True
                                |> Expect.onFail "Should contain a chunk with id parameter"
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

        , describe "URL Handling"
            [ test "URL with semicolons in query string" <|
                \() ->
                    let
                        result = Ansi.parse "\u{001B}]8;;http://example.com?a=1;b=2;c=3\u{0007}link\u{001B}]8;;\u{0007}"
                    in
                    Expect.equal
                        [ Ansi.HyperlinkStart [] "http://example.com?a=1;b=2;c=3"
                        , Ansi.Print "link"
                        , Ansi.HyperlinkEnd
                        ]
                        result

            , test "URL with semicolons and parameters" <|
                \() ->
                    let
                        result = Ansi.parse "\u{001B}]8;id=test;http://example.com?foo=bar;baz=qux\u{0007}link\u{001B}]8;;\u{0007}"
                    in
                    Expect.equal
                        [ Ansi.HyperlinkStart ["id=test"] "http://example.com?foo=bar;baz=qux"
                        , Ansi.Print "link"
                        , Ansi.HyperlinkEnd
                        ]
                        result

            , test "URL with multiple semicolons in path and query" <|
                \() ->
                    let
                        result = Ansi.parse "\u{001B}]8;;http://example.com/path;with;semicolons?q=a;b;c\u{0007}link\u{001B}]8;;\u{0007}"
                    in
                    Expect.equal
                        [ Ansi.HyperlinkStart [] "http://example.com/path;with;semicolons?q=a;b;c"
                        , Ansi.Print "link"
                        , Ansi.HyperlinkEnd
                        ]
                        result

            , test "URL with non-ASCII characters" <|
                \() ->
                    let
                        result = Ansi.parse "\u{001B}]8;;http://example.com/café\u{0007}link\u{001B}]8;;\u{0007}"
                    in
                    Expect.equal
                        [ Ansi.HyperlinkStart [] "http://example.com/caf%C3%A9"
                        , Ansi.Print "link"
                        , Ansi.HyperlinkEnd
                        ]
                        result

            , test "URL with already percent-encoded content should not double-encode" <|
                \() ->
                    let
                        -- URL already has %20, should not become %2520
                        result = Ansi.parse "\u{001B}]8;;http://example.com/file%20name/café\u{0007}link\u{001B}]8;;\u{0007}"
                    in
                    Expect.equal
                        [ Ansi.HyperlinkStart [] "http://example.com/file%20name/caf%C3%A9"
                        , Ansi.Print "link"
                        , Ansi.HyperlinkEnd
                        ]
                        result

            , test "URL with mixed ASCII, percent-encoded, and non-ASCII" <|
                \() ->
                    let
                        result = Ansi.parse "\u{001B}]8;;http://example.com/path%20one/中文/file%20two\u{0007}link\u{001B}]8;;\u{0007}"
                    in
                    Expect.equal
                        [ Ansi.HyperlinkStart [] "http://example.com/path%20one/%E4%B8%AD%E6%96%87/file%20two"
                        , Ansi.Print "link"
                        , Ansi.HyperlinkEnd
                        ]
                        result
            ]

        , describe "Hyperlink State Management"
            [ test "erased content should not inherit hyperlink state" <|
                \() ->
                    let
                        model =
                            List.foldl Ansi.Log.update (Ansi.Log.init Ansi.Log.Raw)
                                [ "\u{001B}]8;;http://example.com\u{0007}link text here"
                                , "\u{001B}[1K"  -- Erase to beginning
                                ]

                        firstLine =
                            Array.get 0 model.lines |> Maybe.withDefault ([], 0)

                        chunks =
                            Tuple.first firstLine

                        -- Find chunks that are just spaces (erased content)
                        spaceChunks =
                            List.filter (\chunk -> String.all (\c -> c == ' ') chunk.text) chunks

                        -- Erased spaces should NOT have link URLs
                        spacesHaveNoLinks =
                            List.all (\chunk -> chunk.linkUrl == Nothing) spaceChunks
                    in

                        spacesHaveNoLinks
                            |> Expect.equal True
                            |> Expect.onFail "Erased spaces should not be hyperlinks"


            , test "line erase with active link should clear link from erased area" <|
                \() ->
                    let
                        model =
                            List.foldl Ansi.Log.update (Ansi.Log.init Ansi.Log.Raw)
                                [ "prefix\u{001B}]8;;http://example.com\u{0007}link"
                                , "\u{001B}[6D"  -- Move cursor back 6
                                , "\u{001B}[1K"  -- Erase to beginning
                                ]

                        firstLine =
                            Array.get 0 model.lines |> Maybe.withDefault ([], 0)

                        chunks =
                            Tuple.first firstLine

                        -- All space chunks should have no links
                        allSpacesNoLinks =
                            chunks
                                |> List.filter (\chunk -> String.all (\c -> c == ' ') chunk.text)
                                |> List.all (\chunk -> chunk.linkUrl == Nothing)
                    in

                    allSpacesNoLinks
                        |> Expect.equal True
                        |> Expect.onFail "Erased content should not be hyperlinked"

            , test "multiple sequential links work correctly" <|
                \() ->
                    let
                        result = Ansi.parse "\u{001B}]8;;http://link1.com\u{0007}first\u{001B}]8;;\u{0007} text \u{001B}]8;;http://link2.com\u{0007}second\u{001B}]8;;\u{0007}"
                    in
                    Expect.equal
                        [ Ansi.HyperlinkStart [] "http://link1.com"
                        , Ansi.Print "first"
                        , Ansi.HyperlinkEnd
                        , Ansi.Print " text "
                        , Ansi.HyperlinkStart [] "http://link2.com"
                        , Ansi.Print "second"
                        , Ansi.HyperlinkEnd
                        ]
                        result

            , test "hyperlink end without active link is ignored" <|
                \() ->
                    let
                        result = Ansi.parse "text\u{001B}]8;;\u{0007} more text"
                    in
                    Expect.equal
                        [ Ansi.Print "text"
                        -- HyperlinkEnd should be silently ignored
                        , Ansi.Print " more text"
                        ]
                        result
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
                    "\u{001B}[0mONE\u{000D}\n\u{001B}[0mtwo\u{000D}\n\u{001B}[0myhree\u{000D}\n\u{001B}[0mfour\u{000D}\n\u{001B}[0mxyz"
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


displayWidthTests : Test
displayWidthTests =
    describe "Display Width Calculation"
        [ describe "Basic ASCII"
            [ test "ASCII characters are 1 column" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "Hello" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 5 width
                        Nothing ->
                            Expect.fail "No line created"
            , test "Numbers are 1 column each" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "12345" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 5 width
                        Nothing ->
                            Expect.fail "No line created"
            ]
        , describe "Emoji (2 columns)"
            [ test "Coffee emoji ☕ is 2 columns" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "☕" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 2 width
                        Nothing ->
                            Expect.fail "No line created"
            , test "Rocket emoji 🚀 is 2 columns" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "🚀" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 2 width
                        Nothing ->
                            Expect.fail "No line created"
            , test "Multiple emoji" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "☕🚀🎉" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 6 width  -- 2 + 2 + 2
                        Nothing ->
                            Expect.fail "No line created"
            , test "Emoji mixed with ASCII" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "Hi ☕ there" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 11 width  -- 3 + 2 + 6
                        Nothing ->
                            Expect.fail "No line created"
            ]
        , describe "CJK Characters (2 columns)"
            [ test "Chinese characters are 2 columns each" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "你好" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 4 width  -- 2 + 2
                        Nothing ->
                            Expect.fail "No line created"
            , test "Japanese Hiragana are 2 columns each" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "こんにちは" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 10 width  -- 5 chars × 2 columns
                        Nothing ->
                            Expect.fail "No line created"
            , test "Korean Hangul syllables are 2 columns each" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "한글" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 4 width  -- 2 + 2
                        Nothing ->
                            Expect.fail "No line created"
            ]
        , describe "Fullwidth Forms (2 columns)"
            [ test "Fullwidth ASCII letter is 2 columns" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "Ａ" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 2 width
                        Nothing ->
                            Expect.fail "No line created"
            , test "Fullwidth number is 2 columns" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "１２３" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 6 width  -- 3 chars × 2 columns
                        Nothing ->
                            Expect.fail "No line created"
            ]
        , describe "Combining Characters (0 columns)"
            [ test "Combining accent e + ́ displays as 1 column" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        -- é as combining: e (U+0065) + combining acute (U+0301)
                        updated = Ansi.Log.update "e\u{0301}" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 1 width  -- e=1, combining=0
                        Nothing ->
                            Expect.fail "No line created"
            , test "café with combining accent" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        -- café: c + a + f + e + combining-acute
                        updated = Ansi.Log.update "cafe\u{0301}" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 4 width  -- c=1, a=1, f=1, e=1, combining=0
                        Nothing ->
                            Expect.fail "No line created"
            ]
        , describe "Zero-Width Characters"
            [ test "Zero-width space is 0 columns" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "a\u{200B}b" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 2 width  -- a=1, zero-width=0, b=1
                        Nothing ->
                            Expect.fail "No line created"
            , test "Zero-width joiner is 0 columns" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "a\u{200D}b" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 2 width  -- a=1, ZWJ=0, b=1
                        Nothing ->
                            Expect.fail "No line created"
            ]
        , describe "Control Characters (0 columns)"
            [ test "NULL character is 0 columns" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "a\u{0000}b" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 2 width  -- a=1, NULL=0, b=1
                        Nothing ->
                            Expect.fail "No line created"
            ]
        , describe "Mixed Content"
            [ test "ASCII + emoji + CJK" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "Hi ☕ 你好" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 10 width  -- H=1, i=1, space=1, ☕=2, space=1, 你=2, 好=2
                        Nothing ->
                            Expect.fail "No line created"
            , test "Real-world example: Rich box message" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "Even the price of a ☕ can brighten my day!" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 43 width  -- 41 ASCII + 2 for emoji
                        Nothing ->
                            Expect.fail "No line created"
            , test "Complex mix with combining chars" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        -- "café ☕ 你好" with combining é
                        updated = Ansi.Log.update "cafe\u{0301} ☕ 你好" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 12 width  -- cafe=4, combining=0, space=1, ☕=2, space=1, 你好=4
                        Nothing ->
                            Expect.fail "No line created"
            ]
        , describe "Edge Cases"
            [ test "Empty string" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "" model
                    in
                    Expect.equal 0 (Array.length updated.lines)
            , test "Only spaces" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "     " model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 5 width
                        Nothing ->
                            Expect.fail "No line created"
            , test "Only emoji" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "🎉🎉🎉" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (_, width) ->
                            Expect.equal 6 width  -- 3 emoji × 2 columns
                        Nothing ->
                            Expect.fail "No line created"
            ]
        ]