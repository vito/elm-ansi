module AnsiLogBenchmark exposing (main)

{-| Realistic benchmark suite for Ansi.Log performance testing.

Focuses on real-world scenarios with actual build log data patterns.

-}

import Ansi
import Ansi.Log as Log
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "Ansi.Log Real-World Performance"
        [ typicalBuildLogBenchmark
        , largeOutputBenchmark
        , colorIntensiveBenchmark
        , progressBarBenchmark
        , unicodeMixedBenchmark
        , streamingUpdatesBenchmark
        ]


{-| Realistic build log with typical compiler output patterns
-}
buildLogData : String
buildLogData =
    String.concat
        [ "\u{001B}[32m[1/12]\u{001B}[0m Compiling src/Main.elm\n"
        , "\u{001B}[33mWarning:\u{001B}[0m Unused import in line 45\n"
        , "  \u{001B}[2m|\u{001B}[0m\n"
        , "45| import Dict exposing (Dict)\n"
        , "  \u{001B}[2m|\u{001B}[0m        \u{001B}[31m^^^^\u{001B}[0m\n"
        , "\u{001B}[32m[2/12]\u{001B}[0m Compiling src/Utils.elm\n"
        , "\u{001B}[32m[3/12]\u{001B}[0m Compiling src/Types.elm\n"
        , "\u{001B}[32m✓\u{001B}[0m Compilation successful\n"
        , "\u{001B}[36mBuild time: 2.34s\u{001B}[0m\n"
        , "Sending build context to Docker daemon  15.36MB\r\n"
        , "Step 1/8 : FROM node:18-alpine\n"
        , " ---> \u{001B}[32mUsing cache\u{001B}[0m\n"
        , " ---> a1b2c3d4e5f6\n"
        , "Step 2/8 : WORKDIR /app\n"
        , " ---> \u{001B}[32mUsing cache\u{001B}[0m\n"
        , "added 234 packages in 12s\n"
        , "\u{001B}[32mSuccessfully built e5f6a1b2c3d4\u{001B}[0m\n"
        ]


{-| Test runner output with pass/fail indicators
-}
testRunnerLog : String
testRunnerLog =
    String.concat
        [ "\u{001B}[1mRunning tests...\u{001B}[0m\n\n"
        , "  \u{001B}[32m✓\u{001B}[0m should parse basic ANSI codes\n"
        , "  \u{001B}[32m✓\u{001B}[0m should handle colors correctly\n"
        , "  \u{001B}[31m✗\u{001B}[0m should handle cursor movements\n"
        , "    \u{001B}[2mExpected:\u{001B}[0m { row: 5, col: 10 }\n"
        , "    \u{001B}[2mReceived:\u{001B}[0m { row: 5, col: 9 }\n"
        , "  \u{001B}[32m✓\u{001B}[0m should merge consecutive chunks\n"
        , "  \u{001B}[33m○\u{001B}[0m should render unicode (skipped)\n\n"
        , "\u{001B}[32m4 passed\u{001B}[0m, \u{001B}[31m1 failed\u{001B}[0m, \u{001B}[33m1 skipped\u{001B}[0m\n"
        ]


{-| Benchmark 1: Typical build log output
Most common real-world use case
-}
typicalBuildLogBenchmark : Benchmark
typicalBuildLogBenchmark =
    benchmark "typical build log (compiler + docker + tests)" <|
        \_ ->
            Log.update (buildLogData ++ testRunnerLog) (Log.init Log.Cooked)


{-| Benchmark 2: Large output (100 lines)
Tests performance with substantial logs
-}
largeOutputBenchmark : Benchmark
largeOutputBenchmark =
    let
        largeLog = String.repeat 10 (buildLogData ++ testRunnerLog)
    in
    benchmark "large output (100 lines)" <|
        \_ ->
            Log.update largeLog (Log.init Log.Cooked)


{-| Benchmark 3: Color-intensive output
Heavy SGR code parsing with RGB colors
-}
colorIntensiveBenchmark : Benchmark
colorIntensiveBenchmark =
    let
        rgbColors =
            String.concat
                [ "\u{001B}[38;2;255;100;50mOrange\u{001B}[0m "
                , "\u{001B}[38;2;50;150;255mBlue\u{001B}[0m "
                , "\u{001B}[1;3;4;31mBold Italic Underline\u{001B}[0m "
                , "\u{001B}[38;5;196mBright Red\u{001B}[0m\n"
                ]
        coloredOutput = String.repeat 10 (testRunnerLog ++ rgbColors)
    in
    benchmark "color-intensive (RGB + styles)" <|
        \_ ->
            Log.update coloredOutput (Log.init Log.Cooked)


{-| Benchmark 4: Progress bars with carriage returns
In-place updates common in CLI tools
-}
progressBarBenchmark : Benchmark
progressBarBenchmark =
    let
        progressBar =
            String.concat
                [ "Downloading: [          ] 0%\r"
                , "Downloading: [##        ] 20%\r"
                , "Downloading: [####      ] 40%\r"
                , "Downloading: [######    ] 60%\r"
                , "Downloading: [########  ] 80%\r"
                , "Downloading: [##########] 100%\n"
                , "Loading |  \r"
                , "Loading /  \r"
                , "Loading -  \r"
                , "Loading \\  \r"
                , "Done!\n"
                ]
        repeated = String.repeat 5 progressBar
    in
    benchmark "progress bars (carriage returns)" <|
        \_ ->
            Log.update repeated (Log.init Log.Cooked)


{-| Benchmark 5: Unicode with emoji and CJK
Wide character width calculations
-}
unicodeMixedBenchmark : Benchmark
unicodeMixedBenchmark =
    let
        unicodeLog =
            String.concat
                [ "\u{001B}[32m✓ 编译成功\u{001B}[0m Build successful 🎉\n"
                , "构建时间: 3.21秒 Time: 3.21s\n"
                , "こんにちは 안녕하세요 Hello 世界 🚀💻🔥\n"
                , "\u{001B}[33mcommit a1b2c3d\u{001B}[0m ✨ Add feature\n"
                ]
        mixed = String.repeat 10 unicodeLog
    in
    benchmark "unicode mixed (emoji + CJK + ASCII)" <|
        \_ ->
            Log.update mixed (Log.init Log.Cooked)


{-| Benchmark 6: Incremental streaming
Realistic chunked updates with split ANSI codes
-}
streamingUpdatesBenchmark : Benchmark
streamingUpdatesBenchmark =
    let
        -- Simulate realistic streaming where ANSI codes are split
        chunks =
            [ "\u{001B}[32m[1"
            , "/12]\u{001B}[0m Comp"
            , "iling src/Ma"
            , "in.elm\n\u{001B}[33mWarn"
            , "ing:\u{001B}[0m Unu"
            , "sed import\n"
            , "\u{001B}[32m[2/12]"
            , "\u{001B}[0m Compili"
            , "ng src/Utils.elm\n"
            , "\u{001B}[32m✓\u{001B}[0m Done\n"
            , "\u{001B}[2K\u{001B}[1G"
            , "fetching... [", "1/4", "]\r"
            , "\u{001B}[2K\u{001B}[1G"
            , "Done\n"
            ]
        -- Repeat to make benchmark meaningful
        allChunks = List.concat (List.repeat 3 chunks)
    in
    benchmark "incremental streaming (42 chunks)" <|
        \_ ->
            List.foldl Log.update (Log.init Log.Cooked) allChunks