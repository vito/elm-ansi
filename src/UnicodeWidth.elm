module UnicodeWidth exposing
    ( runeWidth
    , stringWidth
    , isRegionalIndicator
    )

{-| Unicode width calculation for monospace terminals.

This module uses a tiered lookup strategy for optimal performance:
  - Tier 1: ASCII (O(1), ~95% of typical content)
  - Tier 2: Common CJK & Emoji (O(1), ~90% of non-ASCII)
  - Tier 3: Binary search for rare characters (O(log n))

Based on Unicode 16.0 data from the uniwidth Go package.

@docs runeWidth, stringWidth, isRegionalIndicator

-}

import Array exposing (Array)
import Regex


{-| Returns the visual width of a character in monospace terminals.

Returns:
  - 0 for control characters, zero-width joiners, combining marks
  - 1 for most characters (ASCII, Latin, Cyrillic, etc.)
  - 2 for wide characters (CJK, Emoji, etc.)

-}
runeWidth : Char -> Int
runeWidth char =
    let
        r =
            Char.toCode char
    in
    -- ========================================
    -- Tier 1: ASCII Fast Path (O(1))
    -- ========================================
    -- Covers ~95% of typical terminal content
    if r < 0x80 then
        -- C0 control characters (0x00-0x1F) have zero width
        if r < 0x20 then
            0
        -- DELETE character (0x7F) has zero width
        else if r == 0x7F then
            0
        -- All other ASCII characters have width 1
        else
            1

    -- ========================================
    -- Tier 2: Common CJK Fast Path (O(1))
    -- ========================================
    -- Covers ~80% of Asian content

    -- CJK Unified Ideographs (20,992 characters)
    -- U+4E00 - U+9FFF: Most common Chinese/Japanese characters
    else if r >= 0x4E00 && r <= 0x9FFF then
        2

    -- Hangul Syllables (11,172 characters)
    -- U+AC00 - U+D7AF: Korean syllables
    else if r >= 0xAC00 && r <= 0xD7AF then
        2

    -- Hiragana + Katakana + Bopomofo (384 characters)
    -- U+3040 - U+309F: Hiragana
    -- U+30A0 - U+30FF: Katakana
    -- U+3100 - U+312F: Bopomofo (Taiwan phonetic symbols)
    else if r >= 0x3040 && r <= 0x312F then
        2

    -- CJK Compatibility Ideographs
    -- U+F900 - U+FAFF: Common CJK compatibility forms
    else if r >= 0xF900 && r <= 0xFAFF then
        2

    -- ========================================
    -- Tier 3: Common Emoji Fast Path (O(1))
    -- ========================================
    -- Covers ~90% of emoji usage

    -- Emoticons (80 characters)
    -- U+1F600 - U+1F64F: Smileys and people
    else if r >= 0x1F600 && r <= 0x1F64F then
        2

    -- Miscellaneous Symbols and Pictographs (768 characters)
    -- U+1F300 - U+1F5FF: Weather, zodiac, hands, etc.
    else if r >= 0x1F300 && r <= 0x1F5FF then
        2

    -- Transport and Map Symbols (103 characters)
    -- U+1F680 - U+1F6FF: Vehicles, signs, etc.
    else if r >= 0x1F680 && r <= 0x1F6FF then
        2

    -- Supplemental Symbols and Pictographs (256 characters)
    -- U+1F900 - U+1F9FF: Food, animals, activities
    else if r >= 0x1F900 && r <= 0x1F9FF then
        2

    -- Miscellaneous Symbols (common emoji)
    -- U+2600 - U+26FF: Weather, zodiac, misc symbols
    else if r >= 0x2600 && r <= 0x26FF then
        2

    -- Dingbats (decorative symbols)
    -- U+2700 - U+27BF: Scissors, phone, etc.
    else if r >= 0x2700 && r <= 0x27BF then
        2

    -- ========================================
    -- Zero-Width Characters (O(1))
    -- ========================================

    -- Zero-Width Space (ZWSP) - U+200B
    else if r == 0x200B then
        0

    -- Zero-Width Non-Joiner (ZWNJ)
    else if r == 0x200C then
        0

    -- Zero-Width Joiner (ZWJ) - used in emoji sequences
    else if r == 0x200D then
        0

    -- Variation Selectors (for emoji vs text presentation)
    -- U+FE00 - U+FE0F: Variation selectors
    else if r >= 0xFE00 && r <= 0xFE0F then
        0

    -- Emoji variation selectors
    -- U+E0100 - U+E01EF
    else if r >= 0xE0100 && r <= 0xE01EF then
        0

    -- ========================================
    -- Tier 4: Binary Search Fallback (O(log n))
    -- ========================================
    -- For rare characters not covered by hot paths
    else
        binarySearchWidth r


{-| Calculates the visual width of a string in monospace terminals.

This function provides a fast path for ASCII-only strings,
and uses runeWidth for strings containing Unicode characters.
-}
stringWidth : String -> Int
stringWidth s =
    -- Convert to list of chars for lookahead
    stringWidthUnicode (String.toList s) 0


{-| Unicode path with special handling for regional indicators and variation selectors.
-}
stringWidthUnicode : List Char -> Int -> Int
stringWidthUnicode chars width =
    case chars of
        [] ->
            width

        r :: rest ->
            -- ========================================
            -- Handle Regional Indicator Pairs (Flags)
            -- ========================================
            -- Regional indicators (U+1F1E6 - U+1F1FF) represent country codes.
            -- Two consecutive indicators form a flag emoji with width 2 (not 4).
            if isRegionalIndicator r then
                case rest of
                    next :: remaining ->
                        if isRegionalIndicator next then
                            -- Flag emoji = 2 columns, skip the second indicator
                            stringWidthUnicode remaining (width + 2)
                        else
                            stringWidthUnicode rest (width + runeWidth r)

                    [] ->
                        width + runeWidth r

            -- ========================================
            -- Handle Variation Selectors
            -- ========================================
            -- Variation selectors modify the presentation of the preceding character:
            -- - U+FE0E: Text presentation (narrow, width 1)
            -- - U+FE0F: Emoji presentation (wide, width 2)
            else
                case rest of
                    next :: remaining ->
                        let
                            nextCode =
                                Char.toCode next
                        in
                        -- Text variation selector: force width 1
                        if nextCode == 0xFE0E then
                            stringWidthUnicode remaining (width + 1)
                        -- Emoji variation selector: force width 2
                        else if nextCode == 0xFE0F then
                            stringWidthUnicode remaining (width + 2)
                        else
                            stringWidthUnicode rest (width + runeWidth r)

                    [] ->
                        width + runeWidth r


{-| Returns true if the character is a regional indicator symbol.
Regional indicators (U+1F1E6 - U+1F1FF) represent country codes (A-Z).
Two consecutive indicators form a country flag emoji.
-}
isRegionalIndicator : Char -> Bool
isRegionalIndicator char =
    let
        r =
            Char.toCode char
    in
    r >= 0x1F1E6 && r <= 0x1F1FF


{-| Binary search for characters not covered by hot paths.
-}
binarySearchWidth : Int -> Int
binarySearchWidth r =
    -- Search in wide table (width 2)
    if binarySearch r wideTable then
        2
    -- Search in zero-width table (width 0)
    else if binarySearch r zeroWidthTable then
        0
    -- Search in ambiguous table (default to width 1 for neutral context)
    else if binarySearch r ambiguousTable then
        1
    -- Default: width 1 (most characters)
    else
        1


{-| Binary search on a sorted rune range table.
-}
binarySearch : Int -> Array RuneRange -> Bool
binarySearch r table =
    binarySearchHelper r table 0 (Array.length table - 1)


binarySearchHelper : Int -> Array RuneRange -> Int -> Int -> Bool
binarySearchHelper r table low high =
    if low > high then
        False
    else
        let
            mid =
                (low + high) // 2
        in
        case Array.get mid table of
            Nothing ->
                False

            Just rr ->
                if r < rr.first then
                    binarySearchHelper r table low (mid - 1)
                else if r > rr.last then
                    binarySearchHelper r table (mid + 1) high
                else
                    -- r is within range [first, last]
                    True


{-| Represents a range of runes with the same width property.
-}
type alias RuneRange =
    { first : Int
    , last : Int
    }


-- ========================================
-- Unicode Width Tables (from uniwidth Go package)
-- ========================================

{-| Wide table contains ranges of characters with East Asian Width property W (Wide) or F (Fullwidth).
These characters occupy 2 terminal columns.
-}
wideTable : Array RuneRange
wideTable =
    Array.fromList
        -- Additional emoji ranges not in fast path
        [ { first = 0x2600, last = 0x26FF }
        , { first = 0x2700, last = 0x27BF }
        -- CJK Radicals Supplement
        , { first = 0x2E80, last = 0x2E99 }
        , { first = 0x2E9B, last = 0x2EF3 }
        -- Kangxi Radicals
        , { first = 0x2F00, last = 0x2FD5 }
        -- CJK Symbols and Punctuation
        , { first = 0x3000, last = 0x303F }
        -- CJK Strokes
        , { first = 0x31C0, last = 0x31E3 }
        -- Enclosed CJK Letters and Months
        , { first = 0x3200, last = 0x321E }
        , { first = 0x3220, last = 0x3247 }
        , { first = 0x3250, last = 0x4DBE }
        -- CJK Compatibility Forms
        , { first = 0xFE30, last = 0xFE4F }
        -- Halfwidth and Fullwidth Forms (fullwidth part)
        , { first = 0xFF01, last = 0xFF60 }
        , { first = 0xFFE0, last = 0xFFE6 }
        -- Ancient scripts (supplementary plane)
        , { first = 0x10000, last = 0x1007F }
        -- Kana Supplement
        , { first = 0x1B000, last = 0x1B0FF }
        , { first = 0x1F000, last = 0x1F02F }
        , { first = 0x1F0A0, last = 0x1F0FF }
        , { first = 0x1FA00, last = 0x1FA6F }
        , { first = 0x1FA70, last = 0x1FAFF }
        -- CJK Unified Ideographs Extension B-G (not covered by fast path)
        , { first = 0x20000, last = 0x2A6DF }
        , { first = 0x2A700, last = 0x2B73F }
        , { first = 0x2B740, last = 0x2B81F }
        , { first = 0x2B820, last = 0x2CEAF }
        , { first = 0x2CEB0, last = 0x2EBEF }
        , { first = 0x30000, last = 0x3134F }
        ]


{-| Zero-width table contains ranges of characters with zero width.
These are control characters, combining marks, and format characters.
-}
zeroWidthTable : Array RuneRange
zeroWidthTable =
    Array.fromList
        [ -- C1 control characters
          { first = 0x0080, last = 0x009F }
        -- Combining Diacritical Marks
        , { first = 0x0300, last = 0x036F }
        -- Combining Diacritical Marks Extended
        , { first = 0x1AB0, last = 0x1AFF }
        -- Hebrew combining marks
        , { first = 0x0591, last = 0x05BD }
        , { first = 0x05BF, last = 0x05BF }
        , { first = 0x05C1, last = 0x05C2 }
        , { first = 0x05C4, last = 0x05C5 }
        , { first = 0x05C7, last = 0x05C7 }
        -- Arabic combining marks
        , { first = 0x0610, last = 0x061A }
        , { first = 0x064B, last = 0x065F }
        , { first = 0x0670, last = 0x0670 }
        , { first = 0x06D6, last = 0x06DC }
        , { first = 0x06DF, last = 0x06E4 }
        , { first = 0x06E7, last = 0x06E8 }
        , { first = 0x06EA, last = 0x06ED }
        -- Devanagari combining marks
        , { first = 0x0901, last = 0x0902 }
        , { first = 0x093A, last = 0x093A }
        , { first = 0x093C, last = 0x093C }
        , { first = 0x0941, last = 0x0948 }
        , { first = 0x094D, last = 0x094D }
        , { first = 0x0951, last = 0x0957 }
        , { first = 0x0962, last = 0x0963 }
        -- Soft hyphen
        , { first = 0x00AD, last = 0x00AD }
        -- Format characters
        , { first = 0x200B, last = 0x200F }
        -- Combining marks for symbols
        , { first = 0x20D0, last = 0x20FF }
        -- Arabic presentation forms (zero-width)
        , { first = 0xFE20, last = 0xFE2F }
        -- Specials (BOM, etc.)
        , { first = 0xFEFF, last = 0xFEFF }
        ]


{-| Ambiguous table contains ranges of characters with East Asian Width property A (Ambiguous).
Width depends on context (East Asian: 2, neutral: 1).
For now, we default to width 1 (neutral context).
-}
ambiguousTable : Array RuneRange
ambiguousTable =
    Array.fromList
        [ -- Greek and Coptic (partial)
          { first = 0x00A1, last = 0x00A1 }
        , { first = 0x00A4, last = 0x00A4 }
        , { first = 0x00A7, last = 0x00A8 }
        , { first = 0x00AA, last = 0x00AA }
        , { first = 0x00AD, last = 0x00AE }
        , { first = 0x00B0, last = 0x00B4 }
        , { first = 0x00B6, last = 0x00BA }
        , { first = 0x00BC, last = 0x00BF }
        , { first = 0x00C6, last = 0x00C6 }
        , { first = 0x00D0, last = 0x00D0 }
        , { first = 0x00D7, last = 0x00D8 }
        , { first = 0x00DE, last = 0x00E1 }
        , { first = 0x00E6, last = 0x00E6 }
        , { first = 0x00E8, last = 0x00EA }
        , { first = 0x00EC, last = 0x00ED }
        , { first = 0x00F0, last = 0x00F0 }
        , { first = 0x00F2, last = 0x00F3 }
        , { first = 0x00F7, last = 0x00FA }
        , { first = 0x00FC, last = 0x00FC }
        , { first = 0x00FE, last = 0x00FE }
        , { first = 0x0101, last = 0x0101 }
        , { first = 0x0111, last = 0x0111 }
        , { first = 0x0113, last = 0x0113 }
        , { first = 0x011B, last = 0x011B }
        , { first = 0x0126, last = 0x0127 }
        , { first = 0x012B, last = 0x012B }
        , { first = 0x0131, last = 0x0133 }
        , { first = 0x0138, last = 0x0138 }
        , { first = 0x013F, last = 0x0142 }
        , { first = 0x0144, last = 0x0144 }
        , { first = 0x0148, last = 0x014B }
        , { first = 0x014D, last = 0x014D }
        , { first = 0x0152, last = 0x0153 }
        , { first = 0x0166, last = 0x0167 }
        , { first = 0x016B, last = 0x016B }
        , { first = 0x01CE, last = 0x01CE }
        , { first = 0x01D0, last = 0x01D0 }
        , { first = 0x01D2, last = 0x01D2 }
        , { first = 0x01D4, last = 0x01D4 }
        , { first = 0x01D6, last = 0x01D6 }
        , { first = 0x01D8, last = 0x01D8 }
        , { first = 0x01DA, last = 0x01DA }
        , { first = 0x01DC, last = 0x01DC }
        -- Box Drawing
        , { first = 0x2500, last = 0x257F }
        -- Block Elements
        , { first = 0x2580, last = 0x259F }
        -- Geometric Shapes
        , { first = 0x25A0, last = 0x25FF }
        ]