PATH = %PATH%;C:\C_libs\llvm-mingw-20230614-msvcrt-x86_64\bin

rem del gcc_rah2_11.exe
rem g++ -O0 -ftemplate-backtrace-limit=0 -ogcc_rah2_11.exe rah2_unittests.cpp test_algo.cpp test_views.cpp -std=c++11 -Wall -m64 -Wno-c++17-extensions
rem pause
rem gcc_rah2_11.exe
rem pause
del gcc_rah2_14.exe
g++ -O0 -ftemplate-backtrace-limit=0 -ogcc_rah2_14.exe rah2_unittests.cpp test_algo.cpp test_views.cpp -std=c++14 -I../includes -Wall -m64 -Wno-c++17-extensions
gcc_rah2_14.exe
del gcc_rah2_20.exe
g++ -O0 -ftemplate-backtrace-limit=0 -ogcc_rah2_20.exe rah2_unittests.cpp test_algo.cpp test_views.cpp -std=c++20 -I../includes -Wall -m64
gcc_rah2_20.exe
del clang_rah2_14.exe
"C:\Program Files\LLVM\bin\clang++.exe" -O0 -ftemplate-backtrace-limit=0 -oclang_rah2_14.exe rah2_unittests.cpp test_algo.cpp test_views.cpp -std=c++14 -I../includes -Wall -m64 -Wno-c++17-extensions
clang_rah2_14.exe
del clang_rah2_20.exe
"C:\Program Files\LLVM\bin\clang++.exe" -O0 -ftemplate-backtrace-limit=0 -oclang_rah2_20.exe rah2_unittests.cpp test_algo.cpp test_views.cpp -std=c++20 -I../includes -Wall -m64
clang_rah2_20.exe
pause
