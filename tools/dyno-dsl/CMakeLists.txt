add_executable(dyno-dsl dyno-dsl.cpp)

target_link_libraries(dyno-dsl PRIVATE core)

add_test(
    NAME dyno_dsl_tuple
    COMMAND dyno-dsl --parse "${CMAKE_SOURCE_DIR}/test/tools/dyno-dsl/test3.dysl"
)
add_test(
    NAME dyno_dsl_operators
    COMMAND dyno-dsl --parse "${CMAKE_SOURCE_DIR}/test/tools/dyno-dsl/operators.dysl"
)
add_test(
    NAME dyno_dsl_elaboration
    COMMAND dyno-dsl "${CMAKE_SOURCE_DIR}/test/tools/dyno-dsl/elaboration.dysl"
)
add_test(
    NAME dyno_dsl_wait_on
    COMMAND dyno-dsl "${CMAKE_SOURCE_DIR}/test/tools/dyno-dsl/wait-on.dysl"
)
