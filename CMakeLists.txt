
cmake_minimum_required(VERSION 3.5)

project(rah2)
set(CXX_STANDARD_REQUIRED 14)

if(MSVC)
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} /bigobj /Wall /WX /wd\"5214\" /wd\"5220\" /wd\"5246\" /wd\"5027\" /wd\"4626\" /wd\"4623\" /wd\"4514\" /wd\"5045\" /wd\"4868\" /wd\"4711\" /wd\"4710\" /wd\"4820\" /wd\"4625\" /wd\"4365\" /wd\"4371\"  ")
    set(CMAKE_CXX_FLAGS_DEBUG  "${CMAKE_CXX_FLAGS_DEBUG} /D \"_DEBUG\"")
else()
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror")
    set(CMAKE_CXX_FLAGS_DEBUG  "${CMAKE_CXX_FLAGS_DEBUG} -D_DEBUG")
endif()

enable_testing()

if (${RAH2_INSIDE_EASTL})
    set(RAH2_USE_EASTL 1)
endif()

if (${RAH2_USE_EASTL})
    FIND_PATH(
      EASTL_ROOT_DIR
        include/EASTL/internal/config.h
      HINTS
        ${EASTL_DIR}
    )

    if (MSVC)
      set(EASTL_LIBRARY debug ${EASTL_ROOT_DIR}/build/Debug/EASTL.lib optimized ${EASTL_ROOT_DIR}/build/Release/EASTL.lib)
    else()
      set(EASTL_LIBRARY ${EASTL_ROOT_DIR}/build/libEASTL.a)
    endif()
    add_custom_target(NatVis SOURCES ${EASTL_ROOT_DIR}/doc/EASTL.natvis)
    set(RAH2_USE_EASTL 1)
else()
    set(RAH2_USE_EASTL 0)
endif()

if (${RAH2_INSIDE_EASTL})
    set(RAH2_INSIDE_EASTL 1)
else()
    set(RAH2_INSIDE_EASTL 0)
endif()

file(GLOB UNITTEST_SOURCES "test/*.cpp" "test/*.hpp" "includes/rah2/*.hpp")
add_executable(UnitTests ${UNITTEST_SOURCES})
target_include_directories(UnitTests PRIVATE includes)
add_test(UnitTests UnitTests)
if (${RAH2_INSIDE_EASTL} OR ${RAH2_USE_EASTL})
    if (${RAH2_INSIDE_EASTL})
        target_compile_definitions(UnitTests PUBLIC RAH2_INSIDE_EASTL)
    else()
        target_compile_definitions(UnitTests PUBLIC RAH2_USE_EASTL)
    endif()
    target_include_directories(UnitTests SYSTEM PUBLIC ${EASTL_ROOT_DIR}/include)
    target_include_directories(UnitTests SYSTEM PUBLIC ${EASTL_ROOT_DIR}/build/_deps/eabase-src/include/Common)
    target_include_directories(UnitTests SYSTEM PUBLIC ${EASTL_ROOT_DIR}/test/packages/EAAssert/include)
    target_include_directories(UnitTests SYSTEM PUBLIC ${EASTL_ROOT_DIR}/test/packages/EABase/include/Common)
    target_include_directories(UnitTests SYSTEM PUBLIC ${EASTL_ROOT_DIR}/test/packages/EAMain/include)
    target_include_directories(UnitTests SYSTEM PUBLIC ${EASTL_ROOT_DIR}/test/packages/EAStdC/include)
    target_include_directories(UnitTests SYSTEM PUBLIC ${EASTL_ROOT_DIR}/test/packages/EATest/include)
    target_include_directories(UnitTests SYSTEM PUBLIC ${EASTL_ROOT_DIR}/test/packages/EAThread/include)
    if (${RAH2_NS})
        target_compile_definitions(UnitTests PUBLIC RAH2_NS=${RAH2_NS})
    endif()
    target_link_libraries(UnitTests ${EASTL_LIBRARY})
endif()
