FLEX_TARGET(
        FlexLexer
        ${PROJECT_SOURCE_DIR}/src/minijava/parser/lexer.l
        "${PROJECT_BINARY_DIR}/lexer_impl.cpp"
        COMPILE_FLAGS --header=${PROJECT_BINARY_DIR}/lexer_impl.hpp)

BISON_TARGET(
        BisonParser
        ${PROJECT_SOURCE_DIR}/src/minijava/parser/parser.y
        "${PROJECT_BINARY_DIR}/parser_impl.cpp"
        VERBOSE)

ADD_FLEX_BISON_DEPENDENCY(FlexLexer BisonParser)

add_custom_command(
        OUTPUT  ${PROJECT_BINARY_DIR}/ast_decl.hpp
        COMMAND ${Python3_EXECUTABLE}
                "${PROJECT_SOURCE_DIR}/src/minijava/ast/ast-gen.py"
                "${PROJECT_SOURCE_DIR}/src/minijava/ast/ast.json"
                "${PROJECT_BINARY_DIR}/ast_decl.hpp"
        VERBATIM
        DEPENDS "${PROJECT_SOURCE_DIR}/src/minijava/ast/ast-gen.py"
                "${PROJECT_SOURCE_DIR}/src/minijava/ast/ast.json"
        COMMENT "Generating AST declarations")

set_source_files_properties(
        ${PROJECT_SOURCE_DIR}/src/minijava/parser/parser.cpp
        PROPERTIES OBJECT_DEPENDS ${PROJECT_BINARY_DIR}/ast_decl.hpp)

add_executable(
        minijava
        main.cpp
        utility.cpp
        diagnostic.cpp
        ast/graphviz.cpp
        parser/parser.cpp
        ${FLEX_FlexLexer_OUTPUTS}
        ${BISON_BisonParser_OUTPUTS})

target_include_directories(minijava PRIVATE ${PROJECT_BINARY_DIR})
target_link_libraries(minijava PRIVATE fmt::fmt)

if (CMAKE_COMPILER_IS_GNUCC)
    target_compile_options(minijava PRIVATE "-Wall" "-Wextra" "-Wno-missing-braces")
endif()
if (MSVC)
    target_compile_options(minijava PRIVATE "/W4")
endif()

file(GLOB test_samples "${PROJECT_SOURCE_DIR}/tests/*")
foreach(file ${test_samples})
    get_filename_component(fname ${file} NAME)
    add_test(NAME Parse_${fname}
             COMMAND ${PROJECT_BINARY_DIR}/minijava ${file})
endforeach(file ${test_samples})
