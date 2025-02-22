PATH = %PATH%;C:\msys64\ucrt64\bin;C:\Program Files\LLVM\bin

rem call :test_rah2 g 14 32 0
call :test_rah2 g 14 64 0 DEBUG
call :test_rah2 clang 20 64 3 NDEBUG
call :test_rah2 clang 17 64 0 DEBUG
call :test_rah2 clang 14 64 0 DEBUG
call :test_rah2 g 20 64 0 DEBUG
call :test_rah2 g 14 64 3 NDEBUG
call :test_rah2 g 20 64 3 NDEBUG
call :test_rah2 clang 20 64 0 DEBUG
call :test_rah2 clang 14 64 3 NDEBUG

pause
exit /b

:test_rah2
rem 1 = compiler exe name  (g, clang)
rem 2 = c++ version (14/17/20)
rem 3 = arch
rem 4 = optim
rem 5 = config  (_DEGUG or NDEBUG)

rem -DFAST_TEST -DPERF_TEST

del %~1_rah2_%~2_%~3_%~5.exe
%~1++ -O%~4 -m%~3 -ftemplate-backtrace-limit=0 -o%~1_rah2_%~2_%~3_%~5.exe test_range_factories.cpp rah2_unittests.cpp test_algo.cpp test_algo2.cpp test_algo3.cpp test_algo4.cpp test_algo5.cpp test_range_adaptors3.cpp test_range_adaptors2.cpp test_range_adaptors.cpp test_concat_view.cpp test_set_difference_view.cpp test_zip_view.cpp test_zip_transform_view.cpp -std=c++%~2 -I../includes -D%~5 -Wno-nonnull -Wall -Wextra -Werror
%~1_rah2_%~2_%~3_%~5.exe
echo %~1_rah2_%~2_%~3_%~5.exe Done
exit /b
