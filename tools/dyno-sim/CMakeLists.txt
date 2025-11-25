
# libfst is meson, but just a couple files, so own build instructions
add_library(fst STATIC
  ${CMAKE_CURRENT_SOURCE_DIR}/libfst/src/fastlz.c
  ${CMAKE_CURRENT_SOURCE_DIR}/libfst/src/fstapi.c
  ${CMAKE_CURRENT_SOURCE_DIR}/libfst/src/lz4.c)
target_include_directories(fst PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/libfst/src)
#find_package(BZip2)
#target_link_libraries(fst PRIVATE BZip2::BZip2)

find_package(ZLIB REQUIRED)
target_link_libraries(fst PRIVATE ZLIB::ZLIB)

add_executable(dyno-sim dyno-sim.cpp)
target_include_directories(dyno-sim PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(dyno-sim PRIVATE hw)
target_link_libraries(dyno-sim PRIVATE fst)
