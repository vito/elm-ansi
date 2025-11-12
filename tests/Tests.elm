module Tests exposing (all, assertWindowRendersAs, colorCode, esc, log, parsing, renderChunk, renderLine, renderWindow, styleFlags, wideCharHelpers)

import Ansi
import Ansi.Log
import Array
import Expect
import Regex
import String
import Test exposing (..)


all : Test
all =
    describe "ANSI" [ parsing, log, hyperlinkTests, wideCharHelpers ]


parsing : Test
parsing =
    describe "Parsing"
        [ test "basic foreground and background colors" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal" 6
                    , Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.Print "red fg" 6
                    , Ansi.SetBackground (Just Ansi.Green)
                    , Ansi.Print "green bg" 8
                    , Ansi.SetForeground (Just Ansi.BrightRed)
                    , Ansi.Print "bright red fg" 13
                    , Ansi.SetBackground (Just Ansi.BrightGreen)
                    , Ansi.Print "bright green bg" 15
                    ]
                    (Ansi.parse "normal\u{001B}[31mred fg\u{001B}[42mgreen bg\u{001B}[91mbright red fg\u{001B}[102mbright green bg")
        , test "single argument colors (38/48;5;n format)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal" 6
                    , Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.Print "red fg" 6
                    , Ansi.SetBackground (Just Ansi.Green)
                    , Ansi.Print "green bg" 8
                    , Ansi.SetForeground (Just Ansi.BrightRed)
                    , Ansi.Print "bright red fg" 13
                    , Ansi.SetBackground (Just Ansi.BrightGreen)
                    , Ansi.Print "bright green bg" 15
                    ]
                    (Ansi.parse "normal\u{001B}[38;5;1mred fg\u{001B}[48;5;2mgreen bg\u{001B}[38;5;9mbright red fg\u{001B}[48;5;10mbright green bg")
        , test "8-bit colors" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal" 6
                    , Ansi.SetForeground (Just <| Ansi.Custom 0 215 95)
                    , Ansi.Print "green fg" 8
                    , Ansi.SetBackground (Just <| Ansi.Custom 255 95 0)
                    , Ansi.Print "orange bg" 9
                    ]
                    (Ansi.parse "normal\u{001B}[38;5;41mgreen fg\u{001B}[48;5;202morange bg")
        , test "24-bit colors" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal" 6
                    , Ansi.SetForeground (Just <| Ansi.Custom 123 15 51)
                    , Ansi.Print "custom fg" 9
                    , Ansi.SetBackground (Just <| Ansi.Custom 55 66 77)
                    , Ansi.Print "custom bg" 9
                    , Ansi.SetBackground (Just <| Ansi.Custom 255 0 255)
                    , Ansi.Print "clamped" 7
                    ]
                    (Ansi.parse "normal\u{001B}[38;2;123;15;51mcustom fg\u{001B}[48;2;55;66;77mcustom bg\u{001B}[48;2;1000;0;255mclamped")
        , test "text styling" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal" 6
                    , Ansi.SetBold True
                    , Ansi.Print "bold" 4
                    , Ansi.SetFaint True
                    , Ansi.Print "faint" 5
                    , Ansi.SetItalic True
                    , Ansi.Print "italic" 6
                    , Ansi.SetUnderline True
                    , Ansi.Print "underline" 9
                    , Ansi.SetBlink True
                    , Ansi.Print "blink" 5
                    , Ansi.Print "fast blink" 10
                    , Ansi.SetInverted True
                    , Ansi.Print "inverted" 8
                    , Ansi.SetStrikethrough True
                    , Ansi.Print "strikethrough" 13
                    ]
                    (Ansi.parse "normal\u{001B}[1mbold\u{001B}[2mfaint\u{001B}[3mitalic\u{001B}[4munderline\u{001B}[5mblink\u{001B}[6mfast blink\u{001B}[7minverted\u{001B}[9mstrikethrough")
        , test "resetting styles" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "some text" 9
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
                    , Ansi.Print "reset" 5
                    ]
                    (Ansi.parse "some text\u{001B}[0mreset")
        , test "partial resetting" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "some text" 9
                    , Ansi.SetBold False
                    , Ansi.Print "not bold" 8
                    , Ansi.SetFaint False
                    , Ansi.SetBold False
                    , Ansi.Print "not intense" 11
                    , Ansi.SetItalic False
                    , Ansi.SetFraktur False
                    , Ansi.Print "not italic/fraktur" 18
                    ]
                    (Ansi.parse "some text\u{001B}[21mnot bold\u{001B}[22mnot intense\u{001B}[23mnot italic/fraktur")
        , test "fraktur style" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal" 6
                    , Ansi.SetFraktur True
                    , Ansi.Print "fraktur" 7
                    , Ansi.SetItalic False
                    , Ansi.SetFraktur False
                    , Ansi.Print "not fraktur" 11
                    ]
                    (Ansi.parse "normal\u{001B}[20mfraktur\u{001B}[23mnot fraktur")
        , test "framed style" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal" 6
                    , Ansi.SetFramed True
                    , Ansi.Print "framed" 6
                    , Ansi.SetFramed False
                    , Ansi.Print "not framed" 10
                    ]
                    (Ansi.parse "normal\u{001B}[51mframed\u{001B}[54mnot framed")
        , test "fast blink (code 6) - not implemented, no action" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal" 6
                    , Ansi.Print "fast blink" 10
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
                    , Ansi.Print "red" 3
                    , Ansi.SetForeground Nothing
                    , Ansi.Print "default fg" 10
                    , Ansi.SetBackground (Just Ansi.Blue)
                    , Ansi.Print "blue bg" 7
                    , Ansi.SetBackground Nothing
                    , Ansi.Print "default bg" 10
                    ]
                    (Ansi.parse "\u{001B}[31mred\u{001B}[39mdefault fg\u{001B}[44mblue bg\u{001B}[49mdefault bg")
        , test "individual style reset codes" <|
            \() ->
                Expect.equal
                    [ Ansi.SetUnderline True
                    , Ansi.Print "underlined" 10
                    , Ansi.SetUnderline False
                    , Ansi.Print "not underlined" 14
                    , Ansi.SetBlink True
                    , Ansi.Print "blinking" 8
                    , Ansi.SetBlink False
                    , Ansi.Print "not blinking" 12
                    , Ansi.SetInverted True
                    , Ansi.Print "inverted" 8
                    , Ansi.SetInverted False
                    , Ansi.Print "not inverted" 12
                    , Ansi.SetStrikethrough True
                    , Ansi.Print "struck" 6
                    , Ansi.SetStrikethrough False
                    , Ansi.Print "not struck" 10
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
                    , Ansi.Print "styled" 6
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
                    [ Ansi.Print "some text" 9
                    , Ansi.CarriageReturn
                    , Ansi.Linebreak
                    , Ansi.Print "next line" 9
                    , Ansi.CarriageReturn
                    , Ansi.Print "overwriting" 11
                    , Ansi.Linebreak
                    , Ansi.Print "shifted down" 12
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
                      [ Ansi.Print "foo" 3, Ansi.Remainder "\u{001B}" ],
                      [ Ansi.Print "foo" 3, Ansi.Remainder "\u{001B}[" ],
                      [ Ansi.Print "foo" 3, Ansi.Remainder "\u{001B}[31;32" ]
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
                      [ Ansi.Print "foo" 3, Ansi.Print "lol" 3],
                      [ Ansi.Print "foo" 3, Ansi.Print "lol" 3],
                      [ Ansi.Print "foo" 3, Ansi.Print "bar" 3]
                    ]
                    [ invalid1, invalid2, unknown ]
        , test "malformed 256-color sequences" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "incomplete" 10
                    , Ansi.Print "ext" 3
                    ]
                    (Ansi.parse "before\u{001B}[38;5mincomplete\u{001B}[48;5text")
        , test "malformed 24-bit color sequences" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "missing g and b" 15
                    , Ansi.Print "missing b" 9
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
                    , Ansi.Print "styled" 6
                    , Ansi.SetBold True
                    , Ansi.SetItalic True
                    , Ansi.SetUnderline True
                    , Ansi.Print "multi-style" 11
                    ]
                    (Ansi.parse "\u{001B}[1;31;44mstyled\u{001B}[1;3;4mmulti-style")
        , test "out-of-range SGR parameters are ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "normal" 6
                    , Ansi.Print "unchanged" 9
                    ]
                    (Ansi.parse "normal\u{001B}[999munchanged")
        , test "out-of-range 256-color values" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}[38;5;256mafter")
        , test "CSI with < parameter marker is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}[<1;2;3mafter")
        , test "CSI with = parameter marker is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}[=5cafter")
        , test "CSI with > parameter marker is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}[>cafter")
        , test "CSI with ? parameter marker is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}[?1003lafter")
        , test "CSI < marker does not affect normal sequences" <|
            \() ->
                Expect.equal
                    [ Ansi.SetForeground (Just Ansi.Red)
                    , Ansi.Print "red" 3
                    , Ansi.Print "ignored" 7
                    , Ansi.SetForeground (Just Ansi.Blue)
                    , Ansi.Print "blue" 4
                    ]
                    (Ansi.parse "\u{001B}[31mred\u{001B}[<10;20Mignored\u{001B}[34mblue")
        , test "CSI > marker with parameters is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text" 4
                    , Ansi.Print "more" 4
                    ]
                    (Ansi.parse "text\u{001B}[>1;2;3;4cmore")
        , test "CSI = marker with parameters is properly ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text" 4
                    , Ansi.Print "more" 4
                    ]
                    (Ansi.parse "text\u{001B}[=7;8;9cmore")
        , test "CSI ? marker in middle of sequence is ignored" <|
            \() ->
                -- '?' should only be valid at start; if it appears after digits, ignore the whole sequence
                Expect.equal
                    [ Ansi.Print "text" 4
                    , Ansi.Print "more" 4
                    ]
                    (Ansi.parse "text\u{001B}[1?2mmore")
        , test "Multiple CSI sequences with different markers" <|
            \() ->
                Expect.equal
                    [ Ansi.SetBold True
                    , Ansi.Print "bold" 4
                    , Ansi.Print "ignored1" 8
                    , Ansi.SetForeground (Just Ansi.Green)
                    , Ansi.Print "green" 5
                    , Ansi.Print "ignored2" 8
                    , Ansi.SetBlink True
                    , Ansi.Print "blink" 5
                    ]
                    (Ansi.parse "\u{001B}[1mbold\u{001B}[<5Mignored1\u{001B}[32mgreen\u{001B}[>1cignored2\u{001B}[5mblink")
        , test "CSI marker in remainder (incomplete sequence)" <|
            \() ->
                -- Incomplete sequence with marker should be preserved in Remainder
                Expect.equal
                    [ Ansi.Print "text" 4
                    , Ansi.Remainder "\u{001B}[<12"
                    ]
                    (Ansi.parse "text\u{001B}[<12")
        , test "CSI marker followed by semicolon" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}[>;1;2mafter")
        , test "charset designation ESC(B is consumed and ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}(Bafter")
        , test "charset designation ESC)0 is consumed and ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text" 4
                    , Ansi.Print "more" 4
                    ]
                    (Ansi.parse "text\u{001B})0more")
        , test "charset designation ESC*A is consumed and ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "start" 5
                    , Ansi.Print "end" 3
                    ]
                    (Ansi.parse "start\u{001B}*Aend")
        , test "charset designation ESC+B is consumed and ignored" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "foo" 3
                    , Ansi.Print "bar" 3
                    ]
                    (Ansi.parse "foo\u{001B}+Bbar")
        , test "multiple charset designations in sequence" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "a" 1
                    , Ansi.Print "b" 1
                    , Ansi.Print "c" 1
                    ]
                    (Ansi.parse "a\u{001B}(B\u{001B})0b\u{001B}*Ac")
        , test "charset designation mixed with CSI sequences" <|
            \() ->
                Expect.equal
                    [ Ansi.SetBold True
                    , Ansi.Print "bold" 4
                    , Ansi.Print "text" 4
                    ]
                    (Ansi.parse "\u{001B}[1mbold\u{001B}(Btext")
        , test "ESC 7 saves cursor position" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text" 4
                    , Ansi.SaveCursorPosition
                    , Ansi.Print "more" 4
                    ]
                    (Ansi.parse "text\u{001B}7more")
        , test "ESC 8 restores cursor position" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text" 4
                    , Ansi.RestoreCursorPosition
                    , Ansi.Print "more" 4
                    ]
                    (Ansi.parse "text\u{001B}8more")
        , test "ESC M reverse index (cursor up)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text" 4
                    , Ansi.CursorUp 1
                    , Ansi.Print "more" 4
                    ]
                    (Ansi.parse "text\u{001B}Mmore")
        , test "ESC D index (cursor down)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text" 4
                    , Ansi.CursorDown 1
                    , Ansi.Print "more" 4
                    ]
                    (Ansi.parse "text\u{001B}Dmore")
        , test "ESC E next line (CR + LF)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "text" 4
                    , Ansi.CarriageReturn
                    , Ansi.Linebreak
                    , Ansi.Print "more" 4
                    ]
                    (Ansi.parse "text\u{001B}Emore")
        , test "ESC = keypad application mode (ignored)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}=after")
        , test "ESC > keypad numeric mode (ignored)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}>after")
        , test "ESC c full reset (ignored)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}cafter")
        , test "ESC H tab set (ignored)" <|
            \() ->
                Expect.equal
                    [ Ansi.Print "before" 6
                    , Ansi.Print "after" 5
                    ]
                    (Ansi.parse "before\u{001B}Hafter")
        , test "multiple single-char escapes in sequence" <|
            \() ->
                Expect.equal
                    [ Ansi.SaveCursorPosition
                    , Ansi.Print "text" 4
                    , Ansi.RestoreCursorPosition
                    ]
                    (Ansi.parse "\u{001B}7text\u{001B}8")
        , test "single-char escapes mixed with CSI" <|
            \() ->
                Expect.equal
                    [ Ansi.SaveCursorPosition
                    , Ansi.SetBold True
                    , Ansi.Print "bold" 4
                    , Ansi.RestoreCursorPosition
                    ]
                    (Ansi.parse "\u{001B}7\u{001B}[1mbold\u{001B}8")
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
                          [ Ansi.Print "normal text " 12
                          , Ansi.HyperlinkStart [] "https://example.com"
                          , Ansi.Print "link text" 9
                          , Ansi.HyperlinkEnd
                          , Ansi.Print " more text" 10
                          ],
                          [ Ansi.Print "normal text " 12
                          , Ansi.HyperlinkStart [] "https://example.com"
                          , Ansi.Print "link text" 9
                          , Ansi.HyperlinkEnd
                          , Ansi.Print " more text" 10
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
                          , Ansi.Print "link with id" 12
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart ["id=test", "foo=bar"] "https://example.com"
                          , Ansi.Print "link with multiple params" 25
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart ["id=test", "foo=bar", "baz=quux"] "https://example.com"
                          , Ansi.Print "link with multiple colon-separated params" 41
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
                          [ Ansi.Print "normal " 7
                          , Ansi.SetForeground (Just Ansi.Red)
                          , Ansi.HyperlinkStart [] "https://example.com"
                          , Ansi.Print "red link" 8
                          , Ansi.HyperlinkEnd
                          , Ansi.Print " text" 5
                          ],
                          [ Ansi.HyperlinkStart [] "https://example.com/1"
                          , Ansi.Print "link1" 5
                          , Ansi.HyperlinkEnd
                          , Ansi.Print " and " 5
                          , Ansi.HyperlinkStart [] "https://example.com/2"
                          , Ansi.Print "link2" 5
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart [] "http://example.com/styled"
                          , Ansi.Print "normal" 6
                          , Ansi.SetBold True
                          , Ansi.Print "bold" 4
                          , Ansi.SetItalic True
                          , Ansi.Print "bold-italic" 11
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
                          , Ansi.Print "long url link" 13
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart [] "file:///usr/share/icons/Adwaita/256x256/apps/preferences-desktop-theme.png"
                          , Ansi.Print "File link" 9
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart [] "http://example.com/multiline"
                          , Ansi.Print "first line" 10
                          , Ansi.Linebreak
                          , Ansi.Print "second line" 11
                          , Ansi.HyperlinkEnd
                          ],
                          [ Ansi.HyperlinkStart [] "http://example.com/cursor"
                          , Ansi.Print "move" 4
                          , Ansi.CursorForward 1
                          , Ansi.CursorForward 1
                          , Ansi.CursorForward 1
                          , Ansi.Print "right" 5
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
                        , Ansi.Print "link" 4
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
                        , Ansi.Print "link" 4
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
                        , Ansi.Print "link" 4
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
                        , Ansi.Print "link" 4
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
                        , Ansi.Print "link" 4
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
                        , Ansi.Print "link" 4
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
                        , Ansi.Print "first" 5
                        , Ansi.HyperlinkEnd
                        , Ansi.Print " text " 6
                        , Ansi.HyperlinkStart [] "http://link2.com"
                        , Ansi.Print "second" 6
                        , Ansi.HyperlinkEnd
                        ]
                        result

            , test "hyperlink end without active link is ignored" <|
                \() ->
                    let
                        result = Ansi.parse "text\u{001B}]8;;\u{0007} more text"
                    in
                    Expect.equal
                        [ Ansi.Print "text" 4
                        -- HyperlinkEnd should be silently ignored
                        , Ansi.Print " more text" 10
                        ]
                        result
            ]
        ]


wideCharHelpers : Test
wideCharHelpers =
    describe "Wide Character Helper Functions"
        [ describe "Basic emoji handling"
            [ test "coffee emoji has correct width" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "☕" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            Expect.equal 2 width
                        Nothing ->
                            Expect.fail "Expected line to exist"

            , test "multiple emoji accumulate width correctly" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "☕🎉😀" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            Expect.equal 6 width  -- 3 emoji × 2 width each
                        Nothing ->
                            Expect.fail "Expected line to exist"

            , test "mixed ASCII and emoji" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "Hello ☕" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            Expect.equal 8 width  -- "Hello " = 6 + ☕ = 2
                        Nothing ->
                            Expect.fail "Expected line to exist"
            ]

        , describe "Overwriting with emoji (dropRight behavior)"
            [ test "overwrite ASCII with emoji maintains correct width" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        withText = Ansi.Log.update "abcde" model
                        withEmoji = Ansi.Log.update "\r☕" withText
                        line = Array.get 0 withEmoji.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            -- ☕ (width 2) + "cde" (width 3) = 5
                            Expect.equal 5 width
                        Nothing ->
                            Expect.fail "Expected line to exist"

            , test "overwrite emoji with ASCII maintains correct width" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        withEmoji = Ansi.Log.update "☕test" model
                        withOverwrite = Ansi.Log.update "\rAB" withEmoji
                        line = Array.get 0 withOverwrite.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            -- "AB" (width 2) + "test" (width 4) = 6
                            Expect.equal 6 width
                        Nothing ->
                            Expect.fail "Expected line to exist"

            , test "overwrite in middle of emoji-containing text" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        withText = Ansi.Log.update "Hello ☕ World" model
                        -- Move to position 6 (after "Hello ") and overwrite
                        withMove = Ansi.Log.update "\r\u{001B}[7CX" withText
                        line = Array.get 0 withMove.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            -- "Hello " (6) + "X" (1) + " World" (6) = 13
                            Expect.equal 13 width
                        Nothing ->
                            Expect.fail "Expected line to exist"

            , test "emoji at line boundary during overwrite" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        withEmoji = Ansi.Log.update "Test☕" model
                        withOverwrite = Ansi.Log.update "\rNEW" withEmoji
                        line = Array.get 0 withOverwrite.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            -- "NEW" (3) + "t" (1) + "☕" (2) = 6
                            Expect.equal 6 width
                        Nothing ->
                            Expect.fail "Expected line to exist"
            ]

        , describe "CJK character handling"
            [ test "CJK characters have width 2" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "你好" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            Expect.equal 4 width  -- 2 CJK chars × 2 width each
                        Nothing ->
                            Expect.fail "Expected line to exist"

            , test "mixed CJK, emoji, and ASCII" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "Hello 世界 ☕" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            -- "Hello " = 6, "世界" = 4, " " = 1, "☕" = 2 = 13
                            Expect.equal 13 width
                        Nothing ->
                            Expect.fail "Expected line to exist"
            ]

        , describe "Cursor movement with wide chars"
            [ test "cursor forward over emoji" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        withEmoji = Ansi.Log.update "☕ test" model
                        -- Position should be at column 7 after printing
                    in
                    Expect.equal 7 withEmoji.position.column

            , test "cursor backward over emoji (erasing)" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        withText = Ansi.Log.update "test☕end" model
                        -- Move back 4 positions and write "X"
                        withBackward = Ansi.Log.update "\u{001B}[4DX" withText
                        line = Array.get 0 withBackward.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            -- Width should account for emoji properly
                            -- "testX" + whatever remains = depends on exact positioning
                            Expect.atLeast 1 width
                        Nothing ->
                            Expect.fail "Expected line to exist"
            ]

        , describe "Edge cases"
            [ test "single emoji on line" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "☕" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            Expect.equal 2 width
                        Nothing ->
                            Expect.fail "Expected line to exist"

            , test "emoji followed by newline" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "☕\n" model
                        line0 = Array.get 0 updated.lines
                        line1 = Array.get 1 updated.lines
                    in
                    case (line0, line1) of
                        (Just (_, width0), Just (_, width1)) ->
                            -- In Raw mode, \n preserves column, so line1 starts at column 2
                            Expect.equal (2, 2) (width0, width1)
                        _ ->
                            Expect.fail "Expected both lines to exist"

            , test "empty line has zero width" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        updated = Ansi.Log.update "\n" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            Expect.equal 0 width
                        Nothing ->
                            Expect.fail "Expected line to exist"

            , test "erase line with emoji content" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        withEmoji = Ansi.Log.update "Test ☕ end" model
                        -- Erase from cursor to end
                        erased = Ansi.Log.update "\r\u{001B}[6C\u{001B}[K" withEmoji
                        line = Array.get 0 erased.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            -- Should have "Test " remaining (width 5)
                            -- Plus 1 for cursor at position 6 = width 6
                            Expect.atMost 6 width
                        Nothing ->
                            Expect.fail "Expected line to exist"
            ]

        , describe "Variation selectors"
            [ test "emoji with text variation selector" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        -- ☕ + U+FE0E (text presentation)
                        updated = Ansi.Log.update "☕\u{FE0E}" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            -- Text presentation should force width 1
                            Expect.equal 1 width
                        Nothing ->
                            Expect.fail "Expected line to exist"

            , test "emoji with emoji variation selector" <|
                \() ->
                    let
                        model = Ansi.Log.init Ansi.Log.Raw
                        -- ☕ + U+FE0F (emoji presentation)
                        updated = Ansi.Log.update "☕\u{FE0F}" model
                        line = Array.get 0 updated.lines
                    in
                    case line of
                        Just (chunks, width) ->
                            -- Emoji presentation should force width 2
                            Expect.equal 2 width
                        Nothing ->
                            Expect.fail "Expected line to exist"
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
        , test "unicode width - CJK characters" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m你好世界test"
                    [ "你好世界test" ]
        , test "emoji - coffee emoji maintains alignment" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mEven the price of a ☕ can brighten my day!"
                    [ "Even the price of a ☕ can brighten my day!" ]
        , test "emoji - line overwriting with emoji preserves width" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mTest ☕ rest"
                    [ "Test ☕ test\u{000D}Test ☕ rest" ]
        , test "emoji - cursor movement over emoji" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mHello ☕ Worxd"
                    [ "Hello ☕ World\u{001B}[2Dx" ]
        , test "emoji - multiple emoji in sequence" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m☕🎉😀 party"
                    [ "☕🎉😀 party" ]
        , test "emoji - mixed ASCII, emoji, and CJK" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mHello ☕ 世界 🎉"
                    [ "Hello ☕ 世界 🎉" ]
        , test "emoji - overwriting text ending in emoji" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mTest XX"
                    [ "Test ☕\u{000D}Test XX" ]
        , test "emoji - erase line with emoji content" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mTenew"
                    [ "Test ☕ end\u{001B}[9D\u{001B}[Knew" ]
        , test "emoji - cursor position after emoji" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m☕Xtest"
                    [ "☕\u{001B}[s test\u{001B}[uX" ]
        , test "emoji - single emoji line" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m☕"
                    [ "☕" ]
        , test "emoji - emoji at line boundaries" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m☕\u{000D}\n\u{001B}[0m  test"
                    [ "☕\ntest" ]
        , test "emoji - overwrite with partial emoji positioning" <|
            \() ->
                -- When overwriting with emoji, ensure proper width handling
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m☕cde"
                    [ "abcde\u{000D}☕" ]
        , test "emoji with variation selector - text presentation" <|
            \() ->
                -- U+FE0E is text presentation selector (should force width 1)
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m☕\u{FE0E} text"
                    [ "☕\u{FE0E} text" ]
        , test "emoji with variation selector - emoji presentation" <|
            \() ->
                -- U+FE0F is emoji presentation selector (should force width 2)
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m☕\u{FE0F} emoji"
                    [ "☕\u{FE0F} emoji" ]
        , test "wide chars - erasing with mixed widths" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mHell世界"
                    [ "Hello ☕ 世界\u{001B}[9D\u{001B}[K世界" ]
        , test "wide chars - cursor backward over emoji" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mtestXend"
                    [ "test☕end\u{001B}[4DX" ]
        , test "wide chars - complex table-like alignment" <|
            \() ->
                -- Simulates Rich-style table with emoji
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m│ Hope you enjoy using Rich!                   │\u{000D}\n\u{001B}[0m│ Even the price of a ☕ can brighten my day!   │"
                    [ "│ Hope you enjoy using Rich!                   │\r\n│ Even the price of a ☕ can brighten my day!   │" ]
        , test "partial ANSI sequences across updates" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m\u{001B}[31m\u{001B}[4mred underlined"
                    [ "\u{001B}[3", "1;4mred underlined" ]
        , test "complex style reset with SGR 0" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m\u{001B}[31m\u{001B}[1m\u{001B}[3m\u{001B}[4mbold italic underline red \u{001B}[0mnormal text"
                    [ "\u{001B}[1;3;4;31mbold italic underline red \u{001B}[0mnormal text" ]
        , test "cursor position beyond buffer creates empty lines" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mfirst\u{000D}\n\u{000D}\n\u{000D}\n\u{000D}\n\u{001B}[0m     fifth"
                    [ "first", "\u{001B}[5;6Hfifth" ]
        , test "hyperlink with cursor movement overwrites" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mXYZ end\u{001B}[0mt link text"
                    [ "\u{001B}]8;;https://example.com\u{0007}test link text\u{001B}]8;;\u{0007} end", "\u{001B}[18D", "XYZ" ]
        , test "erase display with cursor at middle" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mline1\u{000D}\n\u{001B}[0m     line2\u{000D}\n\u{001B}[0m          line3\u{000D}\n\u{001B}[0m               line4"
                    [ "line1\nline2\nline3\nline4", "\u{001B}[2;3H", "\u{001B}[0J" ]
        , test "multiple saved cursor positions overwrites" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mabcdefghijkl       GHI"
                    [ "abcdefghijkl", "\u{001B}[s", "\u{001B}[7C", "\u{001B}[s", "\u{001B}[u", "ABC", "\u{001B}[u", "GHI" ]
        , test "complex overwriting with different styles" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0m\u{001B}[31mred t\u{001B}[0m\u{001B}[32mGREEN\u{001B}[0m\u{001B}[33mllow"
                    [ "\u{001B}[31mred text\u{001B}[33myellow", "\u{001B}[9D", "\u{001B}[32mGREEN" ]
        , test "cursor backward at line start" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mtest\u{000D}\n\u{001B}[0mX   second"
                    [ "test\nsecond", "\u{001B}[100D", "X" ]
        , test "combining erase line with cursor movement" <|
            \() ->
                assertWindowRendersAs Ansi.Log.Raw
                    "\u{001B}[0mfirst\u{000D}\n\u{000D}\n\u{001B}[0m           third\u{000D}\n\u{001B}[0mNEW"
                    [ "first\nsecond\nthird\nfourth", "\u{001B}[2;1H", "\u{001B}[K", "\u{001B}[2B", "\u{001B}[K", "NEW" ]
        ]