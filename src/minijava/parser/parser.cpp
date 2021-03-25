#define YY_EXTRA_TYPE minijava::BumpAllocator*
#include <parser_impl.hpp>
#include <lexer_impl.hpp>

#include <minijava/parser/parser.hpp>

using namespace minijava;

void print_token(int token);

int minijava::parse(FILE* file, AstTree* tree)
{
    yyscan_t scanner;
    if (yylex_init_extra(&tree->pool, &scanner) != 0)
        return 1;

    YY_BUFFER_STATE buffer = yy_create_buffer(file, YY_BUF_SIZE, scanner);
    yy_switch_to_buffer(buffer, scanner);

    int parse_status = yyparse(scanner, tree);

    yy_delete_buffer(buffer, scanner);
    yylex_destroy(scanner);
    if (!tree->valid)
        return 2;
    if (parse_status != 0)
        return 3;
    return 0;
}

int minijava::print_tokens(FILE* file)
{
    BumpAllocator pool;

    yyscan_t scanner;
    if (yylex_init_extra(&pool, &scanner) != 0)
        return 1;

    YY_BUFFER_STATE buffer = yy_create_buffer(file, YY_BUF_SIZE, scanner);
    yy_switch_to_buffer(buffer, scanner);

    YYSTYPE yylval;
    YYLTYPE yylloc;
    while (int token = yylex(&yylval, &yylloc, scanner))
        print_token(token);

    yy_delete_buffer(buffer, scanner);
    yylex_destroy(scanner);
    return 0;
}
