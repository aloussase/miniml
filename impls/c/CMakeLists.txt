cmake_minimum_required(VERSION 3.18)

project(miniml LANGUAGES C)

add_executable(miniml
    main.c
    lexer.c)

target_compile_options(miniml PRIVATE
    -Wall
    -g)
