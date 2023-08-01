
cmake_minimum_required(VERSION 3.5)

project(rah2)
set(CMAKE_CXX_STANDARD 14)

if( MSVC)
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} /Wall /WX /wd\"5027\" /wd\"4626\" /wd\"4623\" /wd\"4514\" /wd\"5045\" /wd\"4868\" /wd\"4711\" /wd\"4710\" /wd\"4820\" /wd\"4625\" /wd\"4365\" /wd\"4371\"  ")
endif()

enable_testing()

file(GLOB UNITTEST_SOURCES "test/*.cpp" "includes/rah2/*.hpp")
add_executable(UnitTests ${UNITTEST_SOURCES})
target_include_directories(UnitTests PRIVATE includes)
add_test(UnitTests UnitTests)
