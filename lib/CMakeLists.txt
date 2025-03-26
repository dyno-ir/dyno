#add_library(core
#)

add_library(coreSupport INTERFACE)

#target_compile_features(core PUBLIC cxx_std_20)
#target_include_directories(core PUBLIC ../include)
#target_include_directories(core PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/dsl)

target_include_directories(coreSupport INTERFACE ../include)
