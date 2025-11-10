module UnicodeWidthTests exposing (suite)

import Expect
import Test exposing (..)
import UnicodeWidth exposing (isRegionalIndicator, runeWidth, stringWidth)


{-| Test suite for UnicodeWidth module
-}
suite : Test
suite =
    describe "UnicodeWidth"
        [ tier1AsciiTests
        , tier2CjkTests
        , tier3EmojiTests
        , zeroWidthTests
        , tier4BinarySearchTests
        , stringWidthTests
        , regionalIndicatorTests
        , variationSelectorTests
        , edgeCaseTests
        ]


{-| Tier 1: ASCII Fast Path Tests
-}
tier1AsciiTests : Test
tier1AsciiTests =
    describe "Tier 1: ASCII Fast Path"
        [ describe "C0 control characters (zero width)"
            [ test "NULL character" <|
                \_ -> Expect.equal 0 (runeWidth '\u{0000}')
            , test "TAB character" <|
                \_ -> Expect.equal 0 (runeWidth '\t')
            , test "NEWLINE character" <|
                \_ -> Expect.equal 0 (runeWidth '\n')
            , test "CARRIAGE RETURN character" <|
                \_ -> Expect.equal 0 (runeWidth '\u{000D}')
            , test "ESCAPE character" <|
                \_ -> Expect.equal 0 (runeWidth '\u{001B}')
            , test "Last C0 control (0x1F)" <|
                \_ -> Expect.equal 0 (runeWidth '\u{001F}')
            ]
        , describe "Printable ASCII (width 1)"
            [ test "SPACE character" <|
                \_ -> Expect.equal 1 (runeWidth ' ')
            , test "Lowercase 'a'" <|
                \_ -> Expect.equal 1 (runeWidth 'a')
            , test "Uppercase 'Z'" <|
                \_ -> Expect.equal 1 (runeWidth 'Z')
            , test "Digit '0'" <|
                \_ -> Expect.equal 1 (runeWidth '0')
            , test "Digit '9'" <|
                \_ -> Expect.equal 1 (runeWidth '9')
            , test "Exclamation mark" <|
                \_ -> Expect.equal 1 (runeWidth '!')
            , test "Tilde (last printable ASCII)" <|
                \_ -> Expect.equal 1 (runeWidth '~')
            ]
        , test "DELETE character (zero width)" <|
            \_ -> Expect.equal 0 (runeWidth '\u{007F}')
        ]


{-| Tier 2: CJK Fast Path Tests
-}
tier2CjkTests : Test
tier2CjkTests =
    describe "Tier 2: CJK Fast Path"
        [ describe "CJK Unified Ideographs (U+4E00 - U+9FFF)"
            [ test "First CJK character" <|
                \_ -> Expect.equal 2 (runeWidth '一')
            , test "Common Chinese character '中'" <|
                \_ -> Expect.equal 2 (runeWidth '中')
            , test "Common Chinese character '国'" <|
                \_ -> Expect.equal 2 (runeWidth '国')
            , test "Japanese Kanji '日'" <|
                \_ -> Expect.equal 2 (runeWidth '日')
            , test "Japanese Kanji '本'" <|
                \_ -> Expect.equal 2 (runeWidth '本')
            , test "Last CJK character in range" <|
                \_ -> Expect.equal 2 (runeWidth '\u{9FFF}')
            ]
        , describe "Hangul Syllables (U+AC00 - U+D7AF)"
            [ test "First Hangul syllable" <|
                \_ -> Expect.equal 2 (runeWidth '가')
            , test "Hangul syllable '한'" <|
                \_ -> Expect.equal 2 (runeWidth '한')
            , test "Hangul syllable '국'" <|
                \_ -> Expect.equal 2 (runeWidth '국')
            , test "Last Hangul syllable in range" <|
                \_ -> Expect.equal 2 (runeWidth '\u{D7AF}')
            ]
        , describe "Hiragana and Katakana (U+3040 - U+312F)"
            [ test "Hiragana 'あ'" <|
                \_ -> Expect.equal 2 (runeWidth 'あ')
            , test "Hiragana 'ん'" <|
                \_ -> Expect.equal 2 (runeWidth 'ん')
            , test "Katakana 'ア'" <|
                \_ -> Expect.equal 2 (runeWidth 'ア')
            , test "Katakana 'ン'" <|
                \_ -> Expect.equal 2 (runeWidth 'ン')
            , test "Bopomofo character" <|
                \_ -> Expect.equal 2 (runeWidth 'ㄅ')
            ]
        , describe "CJK Compatibility Ideographs (U+F900 - U+FAFF)"
            [ test "First compatibility character" <|
                \_ -> Expect.equal 2 (runeWidth '\u{F900}')
            , test "Last compatibility character" <|
                \_ -> Expect.equal 2 (runeWidth '\u{FAFF}')
            ]
        ]


{-| Tier 3: Emoji Fast Path Tests
-}
tier3EmojiTests : Test
tier3EmojiTests =
    describe "Tier 3: Emoji Fast Path"
        [ describe "Emoticons (U+1F600 - U+1F64F)"
            [ test "Grinning face 😀" <|
                \_ -> Expect.equal 2 (runeWidth '😀')
            , test "Crying face 😢" <|
                \_ -> Expect.equal 2 (runeWidth '😢')
            , test "Heart eyes 😍" <|
                \_ -> Expect.equal 2 (runeWidth '😍')
            ]
        , describe "Miscellaneous Symbols (U+1F300 - U+1F5FF)"
            [ test "Cyclone 🌀" <|
                \_ -> Expect.equal 2 (runeWidth '🌀')
            , test "Fire 🔥" <|
                \_ -> Expect.equal 2 (runeWidth '🔥')
            ]
        , describe "Transport Symbols (U+1F680 - U+1F6FF)"
            [ test "Rocket 🚀" <|
                \_ -> Expect.equal 2 (runeWidth '🚀')
            , test "Car 🚗" <|
                \_ -> Expect.equal 2 (runeWidth '🚗')
            ]
        , describe "Supplemental Symbols (U+1F900 - U+1F9FF)"
            [ test "Avocado 🥑" <|
                \_ -> Expect.equal 2 (runeWidth '🥑')
            , test "Brain 🧠" <|
                \_ -> Expect.equal 2 (runeWidth '🧠')
            ]
        , describe "Weather and Zodiac (U+2600 - U+26FF)"
            [ test "Sun ☀" <|
                \_ -> Expect.equal 2 (runeWidth '☀')
            , test "Cloud ☁" <|
                \_ -> Expect.equal 2 (runeWidth '☁')
            , test "Umbrella ☂" <|
                \_ -> Expect.equal 2 (runeWidth '☂')
            ]
        , describe "Dingbats (U+2700 - U+27BF)"
            [ test "Scissors ✂" <|
                \_ -> Expect.equal 2 (runeWidth '✂')
            , test "Phone ☎" <|
                \_ -> Expect.equal 2 (runeWidth '☎')
            ]
        ]


{-| Zero-Width Character Tests
-}
zeroWidthTests : Test
zeroWidthTests =
    describe "Zero-Width Characters"
        [ test "Zero-Width Space (ZWSP)" <|
            \_ -> Expect.equal 0 (runeWidth '\u{200B}')
        , test "Zero-Width Non-Joiner (ZWNJ)" <|
            \_ -> Expect.equal 0 (runeWidth '\u{200C}')
        , test "Zero-Width Joiner (ZWJ)" <|
            \_ -> Expect.equal 0 (runeWidth '\u{200D}')
        , describe "Variation Selectors"
            [ test "Variation Selector-1" <|
                \_ -> Expect.equal 0 (runeWidth '\u{FE00}')
            , test "Variation Selector-16" <|
                \_ -> Expect.equal 0 (runeWidth '\u{FE0F}')
            , test "Emoji variation selector" <|
                \_ -> Expect.equal 0 (runeWidth '\u{E0100}')
            ]
        ]


{-| Tier 4: Binary Search Tests
-}
tier4BinarySearchTests : Test
tier4BinarySearchTests =
    describe "Tier 4: Binary Search Fallback"
        [ describe "Wide characters"
            [ test "CJK Symbol" <|
                \_ ->
                    Expect.equal 2 (runeWidth '　')
            , test "Fullwidth Latin" <|
                \_ -> Expect.equal 2 (runeWidth 'Ａ')
            ]
        , describe "Zero-width combining marks"
            [ test "Combining Grave Accent" <|
                \_ -> Expect.equal 0 (runeWidth '\u{0300}')
            , test "Combining Acute Accent" <|
                \_ -> Expect.equal 0 (runeWidth '\u{0301}')
            ]
        , describe "Ambiguous width (default to 1)"
            [ test "Inverted Exclamation" <|
                \_ -> Expect.equal 1 (runeWidth '¡')
            , test "Multiplication Sign" <|
                \_ -> Expect.equal 1 (runeWidth '×')
            ]
        , describe "Default width (1)"
            [ test "Latin Extended" <|
                \_ -> Expect.equal 1 (runeWidth 'ā')
            , test "Cyrillic" <|
                \_ -> Expect.equal 1 (runeWidth 'Б')
            , test "Arabic" <|
                \_ -> Expect.equal 1 (runeWidth 'ب')
            ]
        ]


{-| Regional Indicator Tests
-}
regionalIndicatorTests : Test
regionalIndicatorTests =
    describe "Regional Indicators"
        [ describe "isRegionalIndicator"
            [ test "First regional indicator (A)" <|
                \_ -> Expect.equal True (isRegionalIndicator '🇦')
            , test "Last regional indicator (Z)" <|
                \_ -> Expect.equal True (isRegionalIndicator '🇿')
            , test "Non-regional indicator" <|
                \_ -> Expect.equal False (isRegionalIndicator 'A')
            , test "Emoji (not regional indicator)" <|
                \_ -> Expect.equal False (isRegionalIndicator '😀')
            ]
        , describe "Flag emoji (regional indicator pairs)"
            [ test "US flag 🇺🇸" <|
                \_ -> Expect.equal 2 (stringWidth "🇺🇸")
            , test "Japan flag 🇯🇵" <|
                \_ -> Expect.equal 2 (stringWidth "🇯🇵")
            , test "UK flag 🇬🇧" <|
                \_ -> Expect.equal 2 (stringWidth "🇬🇧")
            , test "Single regional indicator" <|
                \_ -> Expect.equal 1 (stringWidth "🇦")
            , test "Multiple flags" <|
                \_ -> Expect.equal 4 (stringWidth "🇺🇸🇯🇵")
            ]
        ]


{-| Variation Selector Tests
-}
variationSelectorTests : Test
variationSelectorTests =
    describe "Variation Selectors"
        [ test "Text variation selector (force width 1)" <|
            \_ -> Expect.equal 1 (stringWidth "☀\u{FE0E}")
        , test "Emoji variation selector (force width 2)" <|
            \_ -> Expect.equal 2 (stringWidth "☀\u{FE0F}")
        , test "Without variation selector (default)" <|
            \_ -> Expect.equal 2 (stringWidth "☀")
        ]


{-| String Width Tests
-}
stringWidthTests : Test
stringWidthTests =
    describe "stringWidth"
        [ describe "ASCII-only fast path"
            [ test "Empty string" <|
                \_ -> Expect.equal 0 (stringWidth "")
            , test "Simple ASCII" <|
                \_ -> Expect.equal 5 (stringWidth "Hello")
            , test "ASCII with spaces" <|
                \_ -> Expect.equal 11 (stringWidth "Hello World")
            , test "ASCII with punctuation" <|
                \_ -> Expect.equal 13 (stringWidth "Hello, World!")
            ]
        , describe "Unicode strings"
            [ test "CJK characters" <|
                \_ -> Expect.equal 8 (stringWidth "你好世界")
            , test "Mixed ASCII and CJK" <|
                \_ -> Expect.equal 9 (stringWidth "Hello世界")
            , test "Japanese Hiragana" <|
                \_ -> Expect.equal 10 (stringWidth "こんにちは")
            , test "Korean Hangul" <|
                \_ -> Expect.equal 10 (stringWidth "안녕하세요")
            ]
        , describe "Emoji strings"
            [ test "Single emoji" <|
                \_ -> Expect.equal 2 (stringWidth "😀")
            , test "Multiple emoji" <|
                \_ -> Expect.equal 6 (stringWidth "😀😁😂")
            , test "Mixed text and emoji" <|
                \_ -> Expect.equal 7 (stringWidth "Hello😀")
            ]
        , describe "Zero-width characters"
            [ test "String with ZWJ" <|
                \_ -> Expect.equal 5 (stringWidth "Hello\u{200D}")
            , test "String with ZWSP" <|
                \_ -> Expect.equal 5 (stringWidth "Hello\u{200B}")
            ]
        , describe "Control characters"
            [ test "String with newline" <|
                \_ -> Expect.equal 10 (stringWidth "Hello\nWorld")
            , test "String with tab" <|
                \_ -> Expect.equal 10 (stringWidth "Hello\tWorld")
            ]
        ]


{-| Edge Case Tests
-}
edgeCaseTests : Test
edgeCaseTests =
    describe "Edge Cases"
        [ test "Very long ASCII string" <|
            \_ ->
                let
                    longString =
                        String.repeat 100 "a"
                in
                Expect.equal 100 (stringWidth longString)
        , test "Very long CJK string" <|
            \_ ->
                let
                    longString =
                        String.repeat 50 "中"
                in
                Expect.equal 100 (stringWidth longString)
        , test "Mixed script complexity" <|
            \_ -> Expect.equal 15 (stringWidth "Hello世界🚀안녕")
        , test "Combining marks don't add width" <|
            \_ -> Expect.equal 1 (stringWidth "é")
        , test "Boundary testing: just before CJK range" <|
            \_ -> Expect.equal 1 (runeWidth '\u{4DFF}')
        , test "Boundary testing: just after CJK range" <|
            \_ -> Expect.equal 1 (runeWidth '\u{A000}')
        , test "Boundary testing: just before Hangul range" <|
            \_ -> Expect.equal 1 (runeWidth '\u{ABFF}')
        , test "Boundary testing: just after Hangul range" <|
            \_ -> Expect.equal 1 (runeWidth '\u{D7B0}')
        ]