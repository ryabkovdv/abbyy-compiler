#define YY_EXTRA_TYPE minijava::BumpAllocator*
#include <parser_impl.hpp>
#include <lexer_impl.hpp>

#include <minijava/parser/parser.hpp>

using namespace minijava;

namespace {

enum struct ParseError {
    SyntaxError = 1,
    OutOfMemory = 2,
    InitFailure = 3,
};

class ParseErrorCategory : public std::error_category {
public:
    const char* name() const noexcept override
    {
        return "parse error";
    }

    std::string message(int error) const override
    {
        switch (ParseError{error}) {
        case ParseError::SyntaxError:
            return "syntax error";
        case ParseError::OutOfMemory:
            return "out of memory";
        case ParseError::InitFailure:
            return "initialization failure";
        }
        return "<unknown>";
    }
};

[[maybe_unused]]
std::error_code make_error_code(ParseError error)
{
    static const ParseErrorCategory Category;
    return {int(error), Category};
}

} // namespace

template <>
struct std::is_error_code_enum<ParseError> : std::true_type {
};

std::error_code minijava::parse(FILE* file, AstTree* tree)
{
    yyscan_t scanner;
    if (yylex_init_extra(&tree->pool, &scanner) != 0)
        return ParseError::InitFailure;
    ScopeExit free_scanner([&] { yylex_destroy(scanner); });

    YY_BUFFER_STATE buffer = yy_create_buffer(file, YY_BUF_SIZE, scanner);
    ScopeExit free_buffer([&] { yy_delete_buffer(buffer, scanner); });
    yy_switch_to_buffer(buffer, scanner);

    if (int status = yyparse(scanner, tree))
        return ParseError{status};
    return {};
}
