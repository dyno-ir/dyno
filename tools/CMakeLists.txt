#add_executable(cli CLI.cpp)
#target_link_libraries(cli PRIVATE core)

add_executable(abstrtest AbstrTest.cpp)
target_link_libraries(abstrtest PRIVATE coreSupport)

add_executable(rttitest RTTI.cpp)
target_link_libraries(rttitest PRIVATE coreSupport)

add_executable(biginttest BigIntTest.cpp)
target_link_libraries(biginttest PRIVATE coreSupport)

add_subdirectory(dyno-sv)
