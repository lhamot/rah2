PATH = %PATH%;C:\msys64\ucrt64\bin;C:\Program Files\LLVM\bin

rem call :test_rah2 g 14 32 0
rem call :test_rah2 g 14 64 0
call :test_rah2 g 17 64 0
call :test_rah2 g 17 64 3
call :test_rah2 g 17 32 3
call :test_rah2 g 20 64 0
call :test_rah2 clang 14 64 0
call :test_rah2 clang 14 64 3
call :test_rah2 clang 20 64 0
call :test_rah2 clang 20 32 0

pause
exit /b

:test_rah2
rem 1 = compiler exe name  (g, clang)
rem 2 = c++ version (14/17/20)
rem 3 = arch
rem 4 = optim
del %~1_rah2_%~2_%~3_%~4.exe
%~1++ -O%~4 -m%~3 -ftemplate-backtrace-limit=0 -o%~1_rah2_%~2_%~3_%~4.exe rah2_unittests.cpp test_algo.cpp test_views.cpp -std=c++%~2 -I../includes -Wall -Wno-c++17-extensions
%~1_rah2_%~2_%~3_%~4.exe
exit /b
