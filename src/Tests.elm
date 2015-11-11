module Tests where

import ElmTest.Test exposing (..)
import ElmTest.Assertion exposing (..)

import Ansi

all : Test
all =
  suite "ANSI Parsing"
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
    , test "cursor position save/restore" <|
        assertEqual
          [ Ansi.SaveCursorPosition
          , Ansi.RestoreCursorPosition
          ]
          (Ansi.parse "\x1b[s\x1b[u")
    , test "erasure" <|
        assertEqual
          [ Ansi.EraseDisplay Ansi.EraseToBeginning
          , Ansi.EraseDisplay Ansi.EraseToBeginning
          , Ansi.EraseDisplay Ansi.EraseToEnd
          , Ansi.EraseDisplay Ansi.EraseAll
          , Ansi.EraseLine Ansi.EraseToBeginning
          , Ansi.EraseLine Ansi.EraseToBeginning
          , Ansi.EraseLine Ansi.EraseToEnd
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
    , test "invalid escape sequences" <|
        assertEqual
          [Ansi.Print "foo\x1blol"]
          (Ansi.parse "foo\x1blol")
    , test "unknown escape sequences" <|
        assertEqual
          [Ansi.Print "foobar"]
          (Ansi.parse "foo\x1b[1Zbar")
    ]
