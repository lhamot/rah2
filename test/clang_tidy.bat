PATH = %PATH%;C:\C_libs\llvm-mingw-20230614-msvcrt-x86_64\bin;C:\Program Files\LLVM\bin

call :test_rah2 14

pause
exit /b

:test_rah2
rem 1 = c++ version (14/17/20)
clang-tidy rah2_unittests.cpp test_algo.cpp test_views.cpp -fix -header-filter=.* -- -O0 -ftemplate-backtrace-limit=0 -ftemplate-backtrace-limit=0 -std=c++%~1 -I../includes -isystem ../includes/rah2/mpark -Wall -Wno-c++17-extensions
exit /b
