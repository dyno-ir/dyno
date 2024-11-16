add_executable(cli CLI.cpp)
target_link_libraries(cli PRIVATE core)

add_executable(objtest ObjTest.cpp)
target_link_libraries(objtest PRIVATE coreSupport)

add_executable(dstest DSTest.cpp)
target_link_libraries(dstest PRIVATE coreSupport)
