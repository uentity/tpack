cmake_minimum_required(VERSION 3.31)

project(tpack
	VERSION 0.3
	DESCRIPTION "Modern C++ type pack/list library with efficient value-based approach"
	LANGUAGES CXX)

set(TPACK_NAMESPACE tp::)

add_library(tpack INTERFACE)
add_library(tp::tpack ALIAS tpack)
target_compile_features(tpack INTERFACE cxx_std_20)

target_sources(
	tpack PUBLIC
	FILE_SET HEADERS
	BASE_DIRS include
	FILES include/tp/tpack.h
)

if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
	option(TPACK_ENABLE_BENCHMARK "Enable benchmark compile with tests" OFF)
	option(TPACK_ENABLE_TIME_TRACE "Enable compile time trace for Clang" ON)

	include(CTest)
	if (BUILD_TESTING)
		add_library(
			tpack_tests STATIC
			${CMAKE_CURRENT_SOURCE_DIR}/tests/test_tpack.cpp
		)
		if (TPACK_ENABLE_BENCHMARK)
			target_sources(tpack_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_benchmark.cpp)
		endif()
		target_compile_options(tpack_tests PRIVATE
			$<$<CXX_COMPILER_ID:MSVC>:/W4>
			$<$<CXX_COMPILER_ID:Clang>:-fbracket-depth=100000>
			$<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-ftemplate-depth=100000 -Wall -Wextra -Wpedantic>
		)
		if (TPACK_ENABLE_TIME_TRACE)
			target_compile_options(tpack_tests PRIVATE
				$<$<CXX_COMPILER_ID:Clang>:-ftime-trace>
			)
		endif ()
		target_link_libraries(tpack_tests tpack)
	endif ()
endif ()

## Install and export
set(PACKAGE_CONFIG_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

# Lib artifacts which is only headers installed into default location
install(
	TARGETS tpack
	EXPORT tpack-targets
	FILE_SET HEADERS
)
# Install generated package definition file
install(
	EXPORT tpack-targets
	NAMESPACE ${TPACK_NAMESPACE}
	FILE tpack-targets.cmake
	DESTINATION ${PACKAGE_CONFIG_DESTINATION}
)

# Generate CMake package config files
include(CMakePackageConfigHelpers)

configure_package_config_file(
	"${CMAKE_CURRENT_SOURCE_DIR}/cmake/tpack-config.cmake.in"
	"${CMAKE_CURRENT_BINARY_DIR}/tpack-config.cmake"
	INSTALL_DESTINATION ${PACKAGE_CONFIG_DESTINATION}
)
write_basic_package_version_file(
	"${CMAKE_CURRENT_BINARY_DIR}/tpack-version.cmake"
	COMPATIBILITY ExactVersion
)

# Install generated config files
install(
	FILES
		"${CMAKE_CURRENT_BINARY_DIR}/tpack-config.cmake"
		"${CMAKE_CURRENT_BINARY_DIR}/tpack-version.cmake"
	DESTINATION ${PACKAGE_CONFIG_DESTINATION}
)

set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
	APPEND PROPERTY EXPORT_PACKAGE_REGISTRY ${PROJECT_NAME}
)
set(CMAKE_EXPORT_PACKAGE_REGISTRY ON)
export(EXPORT tpack-targets NAMESPACE ${TPACK_NAMESPACE} FILE "${CMAKE_CURRENT_BINARY_DIR}/tpack-targets.cmake")

if (DEFINED CMAKE_USER_BUILD_DEV_MODE AND CMAKE_USER_BUILD_DEV_MODE)
	message(STATUS "Local Package Registry ENABLED for development.")
	export(PACKAGE tpack)
endif ()
