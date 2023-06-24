PATH = %PATH%;C:\MinGW\bin


del clang_rah4_14.exe
"C:\Program Files\LLVM\bin\clang++.exe" -ftemplate-backtrace-limit=0 -oclang_rah4_14.exe rah4_unittests.cpp -std=c++20 -Wall -m64
clang_rah4_14.exe
del clang_rah4_20.exe
"C:\Program Files\LLVM\bin\clang++.exe" -ftemplate-backtrace-limit=0 -oclang_rah4_20.exe rah4_unittests.cpp -std=c++20 -Wall -m64
clang_rah4_20.exe
pause
