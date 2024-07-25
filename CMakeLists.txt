cmake_minimum_required(VERSION 3.20)

project(tpack
	DESCRIPTION "Modern C++ type pack/list library with efficient value-based approach"
	LANGUAGES CXX)

add_library(tpack INTERFACE)

target_include_directories(
	tpack INTERFACE
	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
	$<INSTALL_INTERFACE:include>
)

target_compile_features(tpack INTERFACE cxx_std_17)

if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
	include(CTest)
	if (BUILD_TESTING)
		add_library(tpack_tests STATIC ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_tpack.cpp)
		target_link_libraries(tpack_tests tpack)
	endif ()
endif ()
