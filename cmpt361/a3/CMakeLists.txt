cmake_minimum_required(VERSION 2.6)
project(raycast)

# Settings
set(CMAKE_CXX_FLAGS "-g -O -fdiagnostics-color -Wall -Wshadow -Wunreachable-code")

# My OpenGL libs for C
include_directories("$(PROJECT_SOURCE_DIR)/cog")
add_subdirectory(cog)

add_executable(raycast raycast.c sphere.c material.c)
target_link_libraries(raycast m GL glfw GLEW pthread cogl cogs)
