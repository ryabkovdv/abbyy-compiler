FLEX_TARGET(Lexer
        ${CMAKE_SOURCE_DIR}/src/minijava/parser/lexer.l
        "${CMAKE_BINARY_DIR}/lexer.cpp"
        COMPILE_FLAGS --header=${CMAKE_BINARY_DIR}/lexer.hpp)

BISON_TARGET(Parser
        ${CMAKE_SOURCE_DIR}/src/minijava/parser/parser.y
        "${CMAKE_BINARY_DIR}/parser.cpp"
        VERBOSE)

add_executable(MiniJava
        main.cpp
        ${FLEX_Lexer_OUTPUTS}
        ${BISON_Parser_OUTPUTS})

target_include_directories(MiniJava PRIVATE ${CMAKE_BINARY_DIR})

file(GLOB test_samples "${CMAKE_SOURCE_DIR}/tests/*")
foreach(file ${test_samples})
    get_filename_component(fname ${file} NAME)
    add_test(NAME Scan_${fname}
             COMMAND ${CMAKE_BINARY_DIR}/src/minijava/MiniJava scan ${file})
endforeach(file ${test_samples})

add_test(NAME Parse_Expression
         COMMAND ${CMAKE_BINARY_DIR}/src/minijava/MiniJava
         parse "${CMAKE_SOURCE_DIR}/tests/Expression.java")
