
add_executable(dyno-sim dyno-sim.cpp)

target_include_directories(dyno-sim PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(dyno-sim PRIVATE hw)
