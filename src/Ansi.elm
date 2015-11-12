module Ansi (Color(..), Action(..), EraseMode(..), parse) where

{-| This library primarily exposes the `parse` function and the types that it
will yield.

@docs parse

@docs Action, Color, EraseMode
-}

import Char
import String

{-| The events relevant to interpreting the stream.

* `Print` is a chunk of text which should be interpreted with the style implied
  by the preceding actions (i.e. `[SetBold True, Print "foo"]`) should yield a
  bold `foo`
* `Remainder` is a partial ANSI escape sequence, returned at the end of the
  actions if it was cut off. The next string passed to `parse` should have this
  prepended to it.
* The rest are derived from their respective ANSI escape sequences.
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
  | SetInverted Bool
  | Linebreak
  | CarriageReturn
  | CursorUp Int
  | CursorDown Int
  | CursorForward Int
  | CursorBack Int
  | CursorPosition Int Int
  | EraseDisplay EraseMode
  | EraseLine EraseMode
  | SaveCursorPosition
  | RestoreCursorPosition

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

{-| Method to erase the display or line.
-}
type EraseMode
  = EraseToBeginning
  | EraseToEnd
  | EraseAll

type alias Parser =
  { actions : List Action
  , state : ParserState
  }

type ParserState
  = Escaped
  | CSI (List (Maybe Int)) (Maybe Int)
  | Unescaped String

emptyParser : Parser
emptyParser =
  { actions = []
  , state = Unescaped ""
  }

{-| Convert an arbitrary String of text into a sequence of actions.

If the input string ends with a partial ANSI escape sequence, it will be
yielded as a `Remainder` action, which should then be prepended to the next
call to `parse`.
-}
parse : String -> List Action
parse str =
  completeParsing <|
    String.foldl parseChar emptyParser str

completeParsing : Parser -> List Action
completeParsing parser =
  case parser.state of
    Escaped ->
      parser.actions ++ [Remainder "\x1b"]

    CSI codes currentCode ->
      parser.actions ++ [Remainder <| "\x1b[" ++ encodeCodes (codes ++ [currentCode])]

    Unescaped "" ->
      parser.actions

    Unescaped str ->
      parser.actions ++ [Print str]

encodeCodes : List (Maybe Int) -> String
encodeCodes codes =
  String.join ";" (List.map encodeCode codes)

encodeCode : Maybe Int -> String
encodeCode code =
  case code of
    Nothing -> ""
    Just num -> toString num

parseChar : Char -> Parser -> Parser
parseChar char parser =
  case parser.state of
    Unescaped str ->
      case char of
        '\r' -> completeUnescaped parser [CarriageReturn]
        '\n' -> completeUnescaped parser [Linebreak]
        '\x1b' ->
          let completed = completeUnescaped parser []
          in { completed | state <- Escaped }
        _ -> { parser | state <- Unescaped (str ++ String.fromChar char) }

    Escaped ->
      case char of
        '[' -> { parser | state <- CSI [] Nothing }
        _ -> { parser | state <- Unescaped (String.fromChar char) }

    CSI codes currentCode ->
      case char of
        'm' ->
          completeBracketed parser <|
            List.concatMap
              (codeActions << Maybe.withDefault 0)
              (codes ++ [currentCode])

        'A' ->
          completeBracketed parser
            [CursorUp (Maybe.withDefault 1 currentCode)]

        'B' ->
          completeBracketed parser
            [CursorDown (Maybe.withDefault 1 currentCode)]

        'C' ->
          completeBracketed parser
            [CursorForward (Maybe.withDefault 1 currentCode)]

        'D' ->
          completeBracketed parser
            [CursorBack (Maybe.withDefault 1 currentCode)]

        'H' ->
          completeBracketed parser <|
            cursorPosition (codes ++ [currentCode])

        'J' ->
          completeBracketed parser
            [EraseDisplay (eraseMode (Maybe.withDefault 0 currentCode))]

        'K' ->
          completeBracketed parser
            [EraseLine (eraseMode (Maybe.withDefault 0 currentCode))]

        'f' ->
          completeBracketed parser <|
            cursorPosition (codes ++ [currentCode])

        's' ->
          completeBracketed parser [SaveCursorPosition]

        'u' ->
          completeBracketed parser [RestoreCursorPosition]

        ';' ->
          { parser | state <- CSI (codes ++ [currentCode]) Nothing }

        c ->
          if Char.isDigit c
            then { parser | state <- CSI codes (Just ((Maybe.withDefault 0 currentCode * 10) + (Char.toCode c - 48))) }
            else completeBracketed parser []


completeUnescaped : Parser -> List Action -> Parser
completeUnescaped parser actions =
  case parser.state of
    Unescaped "" ->
      { parser | actions <- parser.actions ++ actions }

    Unescaped str ->
      { parser | actions <- parser.actions ++ [Print str] ++ actions
               , state <- Unescaped "" }

completeBracketed : Parser -> List Action -> Parser
completeBracketed parser actions =
  { parser | actions <- parser.actions ++ actions
           , state <- Unescaped "" }

cursorPosition : List (Maybe Int) -> List Action
cursorPosition codes =
  case codes of
    [Nothing, Nothing] ->
      [CursorPosition 1 1]
    [Nothing] ->
      [CursorPosition 1 1]
    [Just row, Nothing] ->
      [CursorPosition row 1]
    [Nothing, Just col] ->
      [CursorPosition 1 col]
    [Just row, Just col] ->
      [CursorPosition row col]
    _ ->
      []

eraseMode : Int -> EraseMode
eraseMode code =
  case code of
    0 -> EraseToEnd
    1 -> EraseToBeginning
    _ -> EraseAll

codeActions : Int -> List Action
codeActions code =
  case code of
    0 -> reset
    1 -> [SetBold True]
    2 -> [SetFaint True]
    3 -> [SetItalic True]
    4 -> [SetUnderline True]
    7 -> [SetInverted True]
    30 -> [SetForeground (Just Black)]
    31 -> [SetForeground (Just Red)]
    32 -> [SetForeground (Just Green)]
    33 -> [SetForeground (Just Yellow)]
    34 -> [SetForeground (Just Blue)]
    35 -> [SetForeground (Just Magenta)]
    36 -> [SetForeground (Just Cyan)]
    37 -> [SetForeground (Just White)]
    40 -> [SetBackground (Just Black)]
    41 -> [SetBackground (Just Red)]
    42 -> [SetBackground (Just Green)]
    43 -> [SetBackground (Just Yellow)]
    44 -> [SetBackground (Just Blue)]
    45 -> [SetBackground (Just Magenta)]
    46 -> [SetBackground (Just Cyan)]
    47 -> [SetBackground (Just White)]
    90 -> [SetForeground (Just BrightBlack)]
    91 -> [SetForeground (Just BrightRed)]
    92 -> [SetForeground (Just BrightGreen)]
    93 -> [SetForeground (Just BrightYellow)]
    94 -> [SetForeground (Just BrightBlue)]
    95 -> [SetForeground (Just BrightMagenta)]
    96 -> [SetForeground (Just BrightCyan)]
    97 -> [SetForeground (Just BrightWhite)]
    100 -> [SetBackground (Just BrightBlack)]
    101 -> [SetBackground (Just BrightRed)]
    102 -> [SetBackground (Just BrightGreen)]
    103 -> [SetBackground (Just BrightYellow)]
    104 -> [SetBackground (Just BrightBlue)]
    105 -> [SetBackground (Just BrightMagenta)]
    106 -> [SetBackground (Just BrightCyan)]
    107 -> [SetBackground (Just BrightWhite)]
    _ -> []

reset : List Action
reset =
  [ SetForeground Nothing
  , SetBackground Nothing
  , SetBold False
  , SetFaint False
  , SetItalic False
  , SetUnderline False
  , SetInverted False
  ]
