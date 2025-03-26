cmake_minimum_required(VERSION 3.13)

project(compiler VERSION 1.0 LANGUAGES C CXX)

if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
  set(CMAKE_CXX_EXTENSIONS OFF)
  set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
  set(CMAKE_CXX_STANDARD 20)
  set(CMAKE_C_STANDARD 11)
  include(CTest)

  add_compile_options(-Wall -Wextra -Wno-unused-variable -Wno-unused-parameter -Wno-unused-function)

  add_compile_options($<$<CONFIG:Debug>:-fsanitize=address>)
  add_link_options($<$<CONFIG:Debug>:-fsanitize=address>)
endif()

add_subdirectory(lib)
add_subdirectory(tools)
