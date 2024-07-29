cmake_minimum_required(VERSION 3.23)

project(tpack
	DESCRIPTION "Modern C++ type pack/list library with efficient value-based approach"
	LANGUAGES CXX)

add_library(tpack INTERFACE)
add_library(tp::tpack ALIAS tpack)

target_include_directories(
	tpack INTERFACE
	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
	$<INSTALL_INTERFACE:include>
)
target_sources(
	tpack INTERFACE
	FILE_SET HEADERS
	BASE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include
	FILES ${CMAKE_CURRENT_SOURCE_DIR}/include/tp/tpack.h
)

target_compile_features(tpack INTERFACE cxx_std_17)

if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
	include(CTest)
	if (BUILD_TESTING)
		add_library(tpack_tests STATIC ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_tpack.cpp)
		target_link_libraries(tpack_tests tpack)
	endif ()
endif ()

include(GNUInstallDirs)
install(
	TARGETS tpack
	EXPORT tpack-targets
	INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
	FILE_SET HEADERS DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(
	EXPORT tpack-targets NAMESPACE tp::
	DESTINATION ${CMAKE_INSTALL_LIB_DIR}/cmake/tpack
)
export(EXPORT tpack-targets NAMESPACE tp:: FILE tpack-config.cmake)
set(CMAKE_EXPORT_PACKAGE_REGISTRY ON)
export(PACKAGE tpack)