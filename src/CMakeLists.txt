cmake_minimum_required(VERSION 3.12)

# Project
project(4ku VERSION 1.0 LANGUAGES CXX)

# Default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

# Flags
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wshadow -Wconversion")
set(CMAKE_CXX_FLAGS_DEBUG "-g")
if (CMAKE_SYSTEM_NAME MATCHES "Darwin")
    # -march=native is broken on current macOS
    set(CMAKE_CXX_FLAGS_RELEASE "-O3")
else()
    set(CMAKE_CXX_FLAGS_RELEASE "-O3 -march=native")
endif()

# Add the executable
add_executable(
    4ku
    main.cpp
)

target_link_libraries(4ku Threads::Threads)
