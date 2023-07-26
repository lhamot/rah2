PATH = %PATH%;C:\C_libs\llvm-mingw-20230614-msvcrt-x86_64\bin

rem del gcc_rah4_11.exe
rem g++ -O0 -ftemplate-backtrace-limit=0 -ogcc_rah4_11.exe rah4_unittests.cpp test_algo.cpp test_views.cpp -std=c++11 -Wall -m64 -Wno-c++17-extensions
rem pause
rem gcc_rah4_11.exe
rem pause
del gcc_rah4_14.exe
g++ -O0 -ftemplate-backtrace-limit=0 -ogcc_rah4_14.exe rah4_unittests.cpp test_algo.cpp test_views.cpp -std=c++14 -Wall -m64 -Wno-c++17-extensions
gcc_rah4_14.exe
del gcc_rah4_20.exe
g++ -O0 -ftemplate-backtrace-limit=0 -ogcc_rah4_20.exe rah4_unittests.cpp test_algo.cpp test_views.cpp -std=c++20 -Wall -m64
gcc_rah4_20.exe
del clang_rah4_14.exe
"C:\Program Files\LLVM\bin\clang++.exe" -O0 -ftemplate-backtrace-limit=0 -oclang_rah4_14.exe rah4_unittests.cpp test_algo.cpp test_views.cpp -std=c++14 -Wall -m64 -Wno-c++17-extensions
clang_rah4_14.exe
del clang_rah4_20.exe
"C:\Program Files\LLVM\bin\clang++.exe" -O0 -ftemplate-backtrace-limit=0 -oclang_rah4_20.exe rah4_unittests.cpp test_algo.cpp test_views.cpp -std=c++20 -Wall -m64
clang_rah4_20.exe
pause
