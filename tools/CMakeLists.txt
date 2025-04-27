#add_executable(cli CLI.cpp)
#target_link_libraries(cli PRIVATE core)

add_executable(objtest ObjTest.cpp)
target_link_libraries(objtest PRIVATE coreSupport)

add_executable(abstrtest AbstrTest.cpp)
target_link_libraries(abstrtest PRIVATE coreSupport)

add_executable(rttitest RTTI.cpp)
target_link_libraries(rttitest PRIVATE coreSupport)

add_subdirectory(dyno-sv)
