cmake_minimum_required(VERSION 3.13)
project(MiniJava)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=address")
set(CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=address")

enable_testing()

find_package(Python3 REQUIRED)
find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

include_directories(src)

add_subdirectory(third_party/fmt)
add_subdirectory(src/minijava)
