


add_executable(dyno-opt dyno-opt.cpp)
target_include_directories(dyno-opt PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(dyno-opt PRIVATE hw)
