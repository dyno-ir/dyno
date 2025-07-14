
add_subdirectory(slang)
add_executable(dyno-sv dyno-sv.cpp)

target_include_directories(dyno-sv PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(dyno-sv PRIVATE hw)
target_link_libraries(dyno-sv PRIVATE slang_slang)
