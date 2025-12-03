# libfst is meson, but just a couple files, so own build instructions
add_library(fst STATIC
  ${CMAKE_CURRENT_SOURCE_DIR}/libfst/src/fastlz.c
  ${CMAKE_CURRENT_SOURCE_DIR}/libfst/src/fstapi.c
  ${CMAKE_CURRENT_SOURCE_DIR}/libfst/src/lz4.c)
target_include_directories(fst PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/libfst/src)
#find_package(BZip2)
#target_link_libraries(fst PRIVATE BZip2::BZip2)

find_package(ZLIB)
if (NOT ZLIB_FOUND) # FindZLIB broken on Nix, so fallback to pkg-config.
  pkg_check_modules(ZLIB REQUIRED IMPORTED_TARGET zlib)
  add_library(ZLIB::ZLIB INTERFACE IMPORTED)
  target_link_libraries(ZLIB::ZLIB INTERFACE PkgConfig::ZLIB)
endif()

target_link_libraries(fst PRIVATE ZLIB::ZLIB)

add_executable(dyno-sim dyno-sim.cpp)
target_include_directories(dyno-sim PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(dyno-sim PRIVATE hw)
target_link_libraries(dyno-sim PRIVATE fst)
