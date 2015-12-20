module Tests where

import Array
import ElmTest exposing (..)
import Regex
import String

import Ansi
import Ansi.Log

all : Test
all = suite "ANSI" [parsing, log]

parsing : Test
parsing =
  suite "Parsing"
    [ test "colors" <|
        assertEqual
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
          (Ansi.parse "normal\x1b[31mred fg\x1b[42mgreen bg\x1b[91mbright red fg\x1b[102mbright green bg")
    , test "text styling" <|
        assertEqual
          [ Ansi.Print "normal"
          , Ansi.SetBold True
          , Ansi.Print "bold"
          , Ansi.SetFaint True
          , Ansi.Print "faint"
          , Ansi.SetItalic True
          , Ansi.Print "italic"
          , Ansi.SetUnderline True
          , Ansi.Print "underline"
          , Ansi.SetInverted True
          , Ansi.Print "inverted"
          ]
          (Ansi.parse "normal\x1b[1mbold\x1b[2mfaint\x1b[3mitalic\x1b[4munderline\x1b[7minverted")
    , test "resetting" <|
        assertEqual
          [ Ansi.Print "some text"
          , Ansi.SetForeground Nothing
          , Ansi.SetBackground Nothing
          , Ansi.SetBold False
          , Ansi.SetFaint False
          , Ansi.SetItalic False
          , Ansi.SetUnderline False
          , Ansi.SetInverted False
          , Ansi.Print "reset"
          , Ansi.SetForeground Nothing
          , Ansi.SetBackground Nothing
          , Ansi.SetBold False
          , Ansi.SetFaint False
          , Ansi.SetItalic False
          , Ansi.SetUnderline False
          , Ansi.SetInverted False
          , Ansi.Print "reset again"
          , Ansi.SetForeground Nothing
          , Ansi.SetBackground Nothing
          , Ansi.SetBold False
          , Ansi.SetFaint False
          , Ansi.SetItalic False
          , Ansi.SetUnderline False
          , Ansi.SetInverted False
          , Ansi.SetForeground (Just Ansi.Red)
          , Ansi.Print "reset to red"
          ]
          (Ansi.parse "some text\x1b[0mreset\x1b[mreset again\x1b[;31mreset to red")
    , test "carriage returns and linebreaks" <|
        assertEqual
          [ Ansi.Print "some text"
          , Ansi.CarriageReturn
          , Ansi.Linebreak
          , Ansi.Print "next line"
          , Ansi.CarriageReturn
          , Ansi.Print "overwriting"
          , Ansi.Linebreak
          , Ansi.Print "shifted down"
          ]
          (Ansi.parse "some text\r\nnext line\roverwriting\nshifted down")
    , test "cursor movement" <|
        assertEqual
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
          (Ansi.parse "\x1b[5A\x1b[50A\x1b[A\x1b[5B\x1b[50B\x1b[B\x1b[5C\x1b[50C\x1b[C\x1b[5D\x1b[50D\x1b[D\x1b[;50H\x1b[50;H\x1b[H\x1b[;H\x1b[50;50H\x1b[;50f\x1b[50;f\x1b[f\x1b[;f\x1b[50;50f")
    , test "cursor movement (not ANSI.SYS)" <|
        assertEqual
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
          (Ansi.parse "\x1b[E\x1b[5E\x1b[50E\x1b[F\x1b[5F\x1b[50F\x1b[G\x1b[0G\x1b[1G\x1b[5G\x1b[50G")
    , test "cursor position save/restore" <|
        assertEqual
          [ Ansi.SaveCursorPosition
          , Ansi.RestoreCursorPosition
          ]
          (Ansi.parse "\x1b[s\x1b[u")
    , test "erasure" <|
        assertEqual
          [ Ansi.EraseDisplay Ansi.EraseToEnd
          , Ansi.EraseDisplay Ansi.EraseToEnd
          , Ansi.EraseDisplay Ansi.EraseToBeginning
          , Ansi.EraseDisplay Ansi.EraseAll
          , Ansi.EraseLine Ansi.EraseToEnd
          , Ansi.EraseLine Ansi.EraseToEnd
          , Ansi.EraseLine Ansi.EraseToBeginning
          , Ansi.EraseLine Ansi.EraseAll
          ]
          (Ansi.parse "\x1b[J\x1b[0J\x1b[1J\x1b[2J\x1b[K\x1b[0K\x1b[1K\x1b[2K")
    , test "partial escape sequence" <|
        assertEqual
          [Ansi.Print "foo", Ansi.Remainder "\x1b"]
          (Ansi.parse "foo\x1b")
    , test "partial escape sequence with bracket" <|
        assertEqual
          [Ansi.Print "foo", Ansi.Remainder "\x1b["]
          (Ansi.parse "foo\x1b[")
    , test "partial escape sequence with bracket and codes" <|
        assertEqual
          [Ansi.Print "foo", Ansi.Remainder "\x1b[31;32"]
          (Ansi.parse "foo\x1b[31;32")
    , test "invalid escape sequences (no bracket)" <|
        assertEqual
          [Ansi.Print "foo", Ansi.Print "lol"]
          (Ansi.parse "foo\x1blol")
    , test "invalid escape sequences (double bracket)" <|
        assertEqual
          [Ansi.Print "foo", Ansi.Print "lol"]
          (Ansi.parse "foo\x1b[[lol")
    , test "unknown escape sequences" <|
        assertEqual
          [Ansi.Print "foo", Ansi.Print "bar"]
          (Ansi.parse "foo\x1b[1Zbar")
    ]

assertWindowRendersAs : Ansi.Log.LineDiscipline -> String -> List String -> Assertion
assertWindowRendersAs ldisc rendered updates =
  let
    window = List.foldl Ansi.Log.update (Ansi.Log.init ldisc) updates
  in
    assertEqual (esc rendered) (esc <| renderWindow window)

esc : String -> String
esc = Regex.replace Regex.All (Regex.regex "\x1b") (always "\\e")

renderWindow : Ansi.Log.Model -> String
renderWindow window =
  String.join "\r\n" (Array.toList (Array.map renderLine window.lines))

renderLine : Ansi.Log.Line -> String
renderLine (chunks, _) =
  String.join "" (List.foldl (\c l -> renderChunk c :: l) [] chunks)

renderChunk : Ansi.Log.Chunk -> String
renderChunk chunk =
  "\x1b[0m" ++ styleFlags chunk.style ++ chunk.text

styleFlags : Ansi.Log.Style -> String
styleFlags style =
  String.join ""
    [ case style.foreground of
        Nothing -> ""
        Just fg -> "\x1b[" ++ toString (30 + colorCode fg) ++ "m"
    , case style.background of
        Nothing -> ""
        Just bg -> "\x1b[" ++ toString (40 + colorCode bg) ++ "m"
    , if style.bold then "\x1b[1m" else ""
    , if style.faint then "\x1b[2m" else ""
    , if style.italic then "\x1b[3m" else ""
    , if style.underline then "\x1b[4m" else ""
    , if style.inverted then "\x1b[7m" else ""
    ]

colorCode : Ansi.Color -> Int
colorCode color =
  case color of
    Ansi.Black -> 0
    Ansi.Red -> 1
    Ansi.Green -> 2
    Ansi.Yellow -> 3
    Ansi.Blue -> 4
    Ansi.Magenta -> 5
    Ansi.Cyan -> 6
    Ansi.White -> 7
    Ansi.BrightBlack -> 60
    Ansi.BrightRed -> 61
    Ansi.BrightGreen -> 62
    Ansi.BrightYellow -> 63
    Ansi.BrightBlue -> 64
    Ansi.BrightMagenta -> 65
    Ansi.BrightCyan -> 66
    Ansi.BrightWhite -> 67


log : Test
log =
  suite "Log"
    [ test "basic printing" <|
        assertWindowRendersAs Ansi.Log.Raw "\x1b[0mx" ["x"]
    , test "basic printing of multiple chunks" <|
        assertWindowRendersAs Ansi.Log.Raw "\x1b[0mxyz" ["x", "y", "z"]
    , test "colors" <|
        assertWindowRendersAs Ansi.Log.Raw
          "\x1b[0m\x1b[31mred\x1b[0m\x1b[31m\x1b[41mred bg"
          ["\x1b[31mred\x1b[41mred bg"]
    , test "text styling" <|
        assertWindowRendersAs Ansi.Log.Raw
          "\x1b[0mnormal\x1b[0m\x1b[1mbold\x1b[0m\x1b[1m\x1b[2mfaint\x1b[0m\x1b[1m\x1b[2m\x1b[3mitalic\x1b[0m\x1b[1m\x1b[2m\x1b[3m\x1b[4munderline\x1b[0m\x1b[1m\x1b[2m\x1b[3m\x1b[4m\x1b[7minverted"
          ["normal\x1b[1mbold\x1b[2mfaint\x1b[3mitalic\x1b[4munderline\x1b[7minverted"]
    , test "line overwriting" <|
        assertWindowRendersAs Ansi.Log.Raw
          "\x1b[0m\x1b[31mred\x1b[0m baz"
          ["foo\rbar baz\r\x1b[31mred"]
    , test "new lines in raw mode" <|
        assertWindowRendersAs Ansi.Log.Raw
          "\x1b[0m\x1b[41mfoo\r\n\x1b[0m\x1b[41m   bar baz\r\n\x1b[0m\x1b[41m          "
          ["\x1b[41mfoo\nbar baz\n"]
    , test "new lines in cooked mode" <|
        assertWindowRendersAs Ansi.Log.Cooked
          "\x1b[0m\x1b[41mfoo\r\n\x1b[0m\x1b[41mbar baz\r\n"
          ["\x1b[41mfoo\nbar baz\n"]
    , test "ansi escapes on boundaries" <|
        assertWindowRendersAs Ansi.Log.Raw
          "\x1b[0m\x1b[41mfoo\x1b[0m\x1b[31m\x1b[41mbar baz"
          ["\x1b[4", "1mfoo", "\x1b", "[31mbar baz"]
    , test "cursor movement" <|
        assertWindowRendersAs Ansi.Log.Raw
          "\x1b[0mONE\r\n\x1b[0mtwo\r\n\x1b[0mthrxeE\r\n\x1b[0mf!!r\r\n\x1b[0mxyz"
          [ "one\r\ntwo\r\nthree\r\nfour\r\nxyz\r"
          , "\x1b[4AONE"
          , "\x1b[2Bx"
          , "\x1b[CE"
          , "\x1b[B"
          , "\x1b[5D!!"
          ]
    , test "cursor movement (not ANSI.SYS)" <|
        assertWindowRendersAs Ansi.Log.Raw
          "\x1b[0mONE\r\n\x1b[0mtwo\r\n\x1b[0mxyree\r\n\x1b[0mfour\r\n\x1b[0mxyz"
          [ "one\r\ntwo\r\nthree\r\nfour\r\nxyz\r"
          , "\x1b[4FONE"
          , "\x1b[2Ex"
          , "\x1b[1Gy"
          ]
    , test "setting the cursor position" <|
        assertWindowRendersAs Ansi.Log.Raw
          "\x1b[0mone\r\n\x1b[0mtwo\r\n\x1b[0mtHRee\r\n\x1b[0mfOUr\r\n\x1b[0mxyz"
          [ "one\r\ntwo\r\nthree\r\nfour\r\nxyz\r"
          , "\x1b[3;2HHR"
          , "\x1b[4;2fOU"
          ]
    , test "saving and restoring the cursor position" <|
        assertWindowRendersAs Ansi.Log.Raw
          "\x1b[0mone\r\n\x1b[0mtwo\r\n\x1b[0mtHRee\r\n\x1b[0mfour\r\n\x1b[0mxyz"
          [ "one\r\ntwo\r\nt\x1b[shree\r\nfour\r\nxyz\r"
          , "\x1b[uHR"
          ]
    , test "erasing line contents" <|
        assertWindowRendersAs Ansi.Log.Raw
          "\x1b[0moneTWO\r\n\x1b[0m   TWO\r\n\x1b[0m      THREEFOUR\r\n"
          [ "onetwenty\x1b[6D\x1b[KTWO\r\n"
          , "onetwo\x1b[3D\x1b[1KTWO\r\n"
          , "onetwo\x1b[2KTHREEFOUR\r\n"
          ]
    ]
