module Ansi.Log (Window, Line, Chunk, init, update) where

{-| Log interprets a stream of text and ANSI escape codes.

@docs init, update

@docs Window, Line, Chunk
-}

import Array exposing (Array)
import String

import Ansi

{-| Window is populated by parsing ANSI character sequences and escape codes
via `update`.

* `lines` contains all of the output that's been parsed
* `position` is the current position of the cursor
* `style` is the style to be applied to any text that's printed
* `remainder` is a partial ANSI escape sequence left around from an incomplete
  segment from the stream
-}
type alias Window =
  { lines : Array Line
  , position : CursorPosition
  , style : Style
  , remainder : String
  }

{-| A list of arbitrarily-sized chunks of output.
-}
type alias Line = List Chunk

{-| A blob of text paired with the style that was configured at the time.
-}
type alias Chunk =
  { text : String
  , style : Style
  }

{-| The current presentation state for any text that's printed.
-}
type alias Style =
  { foreground : Maybe Ansi.Color
  , background : Maybe Ansi.Color
  , bold : Bool
  , faint : Bool
  , italic : Bool
  , underline : Bool
  , inverted : Bool
  }

{-| The coordinate in the window where text will be printed.
-}
type alias CursorPosition =
  { row : Int
  , column : Int
  }

moveCursor : Int -> Int -> CursorPosition -> CursorPosition
moveCursor r c pos =
  { pos | row <- pos.row + r, column <- pos.column + c }

{-| Construct an empty model.
-}
init : Window
init =
  { lines = Array.empty
  , position = { row = 0, column = 0 }
  , style =
    { foreground = Nothing
    , background = Nothing
    , bold = False
    , faint = False
    , italic = False
    , underline = False
    , inverted = False
    }
  , remainder = ""
  }

{-| Parse and interpret a chunk of ANSI output.

Trailing partial ANSI escape codes will be prepended to the chunk in the next
call to `update`.
-}
update : String -> Window -> Window
update str model =
  List.foldl handleAction model (Ansi.parse (model.remainder ++ str))

handleAction : Ansi.Action -> Window -> Window
handleAction action model =
  case action of
    Ansi.Print s ->
      let
        chunk = Chunk s model.style
        lineToChange = Maybe.withDefault [] (Array.get (model.position.row) model.lines)
        line = writeChunk model.position.column chunk lineToChange
      in
        { model | lines <- setLine model.position.row line model.lines
                , position <- moveCursor 0 (String.length s) model.position }

    Ansi.CarriageReturn ->
      { model | position <- moveCursor 0 (-model.position.column) model.position }

    Ansi.Linebreak ->
      { model | position <- moveCursor 1 0 model.position }

    Ansi.Remainder s ->
      { model | remainder <- s }

    _ ->
      { model | style <- updateStyle action model.style }

setLine : Int -> Line -> Array Line -> Array Line
setLine row line lines =
  let
    currentLines = Array.length lines
  in
    if row + 1 > currentLines
      then appendLine (row - currentLines) line lines
      else Array.set row line lines

appendLine : Int -> Line -> Array Line -> Array Line
appendLine after line lines =
  if after == 0
     then Array.push line lines
     else appendLine (after - 1) line (Array.push [] lines)

updateStyle : Ansi.Action -> Style -> Style
updateStyle action style =
  case action of
    Ansi.SetForeground mc ->
      { style | foreground <- mc }

    Ansi.SetBackground mc ->
      { style | background <- mc }

    Ansi.SetInverted b ->
      { style | inverted <- b }

    Ansi.SetBold b ->
      { style | bold <- b }

    Ansi.SetFaint b ->
      { style | faint <- b }

    Ansi.SetItalic b ->
      { style | italic <- b }

    Ansi.SetUnderline b ->
      { style | underline <- b }

writeChunk : Int -> Chunk -> Line -> Line
writeChunk pos chunk line =
  let
    before = takeLen pos line
    after = dropLen (pos + String.length chunk.text) line
  in
    before ++ [chunk] ++ after

dropLen : Int -> Line -> Line
dropLen len line =
  case line of
    lc :: lcs ->
      let
          chunkLen = String.length lc.text
      in
        if chunkLen > len
           then { lc | text <- String.dropLeft len lc.text } :: lcs
           else dropLen (len - chunkLen) lcs

    [] -> []

takeLen : Int -> Line -> Line
takeLen len line =
  if len == 0 then
    []
  else
    case line of
      lc :: lcs ->
        let
            chunkLen = String.length lc.text
        in
          if chunkLen < len
             then lc :: takeLen (len - chunkLen) lcs
             else [{ lc | text <- String.left len lc.text }]

      [] -> []
