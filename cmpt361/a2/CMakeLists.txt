cmake_minimum_required(VERSION 2.6)
project(fetris)

# Settings
set(CMAKE_CXX_FLAGS "-g -O -fdiagnostics-color -Wall -Wshadow -Wunreachable-code")

# My OpenGL libs for C
include_directories("$(PROJECT_SOURCE_DIR)/cog")
add_subdirectory(cog)

add_executable(fetris fetris.c block.c collision.c util.c)
target_link_libraries(fetris m GL glfw GLEW pthread cogl cogc cogs)
