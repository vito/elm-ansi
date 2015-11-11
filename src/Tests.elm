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
          ]
          (Ansi.parse "some text\x1b[0mreset")
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
    ]
