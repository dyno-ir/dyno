#add_executable(cli CLI.cpp)
#target_link_libraries(cli PRIVATE core)

add_executable(abstrtest AbstrTest.cpp)
target_link_libraries(abstrtest PRIVATE hw)

add_executable(rttitest RTTI.cpp)
target_link_libraries(rttitest PRIVATE hw)

add_executable(biginttest BigIntTest.cpp)
target_link_libraries(biginttest PRIVATE hw)

add_subdirectory(dyno-sv)
add_subdirectory(dyno-sim)
add_subdirectory(arrpat)
add_subdirectory(dynomite)
