cmake_minimum_required(VERSION 3.12)

# Project
project(4ku VERSION 1.0 LANGUAGES CXX)

# Default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Flags
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wshadow -Wconversion")
set(CMAKE_CXX_FLAGS_DEBUG "-g")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -pthread")

# Add the executable
add_executable(
    4ku
    main.cpp
)
