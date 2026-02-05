


add_executable(dyno-test dyno-test.cpp)
target_include_directories(dyno-test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(dyno-test PRIVATE hw)
