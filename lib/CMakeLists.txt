add_library(coreSupport INTERFACE)
target_include_directories(coreSupport INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
)

# split libs at some point
add_library(core STATIC
    dyno/Constant.cpp
    hw/HWPrinter.cpp
    hw/passes/InstCombine.cpp
    support/ErrorRecovery.cpp
)

add_library(hw STATIC
    hw/HWPrinter.cpp
)


target_compile_features(core PUBLIC cxx_std_23)
target_link_libraries(core PUBLIC coreSupport)
target_link_libraries(hw PUBLIC core)
