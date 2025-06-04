
add_subdirectory(slang)
add_executable(dyno-sv dyno-sv.cpp)

target_link_libraries(dyno-sv PRIVATE hw)
target_link_libraries(dyno-sv PRIVATE slang_slang)
