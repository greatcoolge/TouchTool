cmake_minimum_required(VERSION 3.18.1)

include_directories(${CMAKE_SOURCE_DIR})

add_library(touch_tool SHARED native-lib.cpp)

add_library(opencv_core STATIC IMPORTED)
add_library(opencv_imgproc STATIC IMPORTED)
add_library(cpufeatures STATIC IMPORTED)
add_library(tegra_hal STATIC IMPORTED)

set_target_properties(opencv_core PROPERTIES IMPORTED_LOCATION ${CMAKE_SOURCE_DIR}/../jniLibs/${ANDROID_ABI}/libopencv_core.a)
set_target_properties(opencv_imgproc PROPERTIES IMPORTED_LOCATION ${CMAKE_SOURCE_DIR}/../jniLibs/${ANDROID_ABI}/libopencv_imgproc.a)
set_target_properties(cpufeatures PROPERTIES IMPORTED_LOCATION ${CMAKE_SOURCE_DIR}/../jniLibs/${ANDROID_ABI}/libcpufeatures.a)
set_target_properties(tegra_hal PROPERTIES IMPORTED_LOCATION ${CMAKE_SOURCE_DIR}/../jniLibs/${ANDROID_ABI}/libtegra_hal.a)

find_library(log-lib log)
find_library(jnigraphics-lib jnigraphics)

target_link_libraries(
        touch_tool

        opencv_imgproc
        opencv_core
        cpufeatures
        tegra_hal

        ${jnigraphics-lib}
        ${log-lib})