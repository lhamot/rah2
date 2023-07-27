PATH = %PATH%;C:\C_libs\llvm-mingw-20230614-msvcrt-x86_64\bin;C:\Program Files\LLVM\bin

call :test_rah2 g 14 64
rem call :test_rah2 g 14 32
call :test_rah2 g 20 64
call :test_rah2 clang 14 64
call :test_rah2 clang 20 64
call :test_rah2 clang 20 32

pause
exit /b

:test_rah2
rem 1 = compiler exe name  (g, clang)
rem 2 = c++ version (14/17/20)
rem 3 = arch
del %~1_rah2_%~2_%~3.exe
%~1++ -O0 -m%~3 -ftemplate-backtrace-limit=0 -o%~1_rah2_%~2_%~3.exe rah2_unittests.cpp test_algo.cpp test_views.cpp -std=c++%~2 -I../includes -Wall -Wno-c++17-extensions
%~1_rah2_%~2_%~3.exe
exit /b
