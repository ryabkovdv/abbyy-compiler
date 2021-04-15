#include <minijava/parser/parser.hpp>

#define YY_EXTRA_TYPE minijava::detail::yy_extra_type*
#include <minijava/parser/yyextra.hpp>
#include <parser_impl.hpp>
#include <lexer_impl.hpp>

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

std::error_code
minijava::parse(std::string_view content, DiagnosticEngine& diag, AstTree* tree)
{
    yyscan_t scanner;
    detail::yy_extra_type yyextra_val{{}, &tree->pool};
    if (yylex_init_extra(&yyextra_val, &scanner) != 0)
        return ParseError::InitFailure;
    ScopeExit free_scanner([&] { yylex_destroy(scanner); });

    YY_BUFFER_STATE buffer =
        yy_scan_bytes(content.data(), content.size(), scanner);
    ScopeExit free_buffer([&] { yy_delete_buffer(buffer, scanner); });
    yy_switch_to_buffer(buffer, scanner);

    if (int status = yyparse(scanner, tree, diag))
        return ParseError{status};
    return {};
}

std::error_code
minijava::print_tokens(std::string_view content, DiagnosticEngine& diag)
{
    yyscan_t scanner;
    BumpAllocator pool;
    detail::yy_extra_type yyextra_val{{}, &pool};
    if (yylex_init_extra(&yyextra_val, &scanner) != 0)
        return ParseError::InitFailure;
    ScopeExit free_scanner([&] { yylex_destroy(scanner); });

    YY_BUFFER_STATE buffer =
        yy_scan_bytes(content.data(), content.size(), scanner);
    ScopeExit free_buffer([&] { yy_delete_buffer(buffer, scanner); });
    yy_switch_to_buffer(buffer, scanner);

    YYSTYPE yylval;
    YYLTYPE yylloc = {1, 1, 1, 1};
    while (int _ = yylex(&yylval, &yylloc, scanner))
        diag.warn(yylloc, "test");

    return {};
}
