#include <algorithm>
#include <cstdio>
#include <string_view>

#include <lexer.hpp>
#include <parser.hpp>

static const char* token_to_string(int token)
{
    switch (token) {
    case TOK_IDENTIFIER:    return "TOK_IDENTIFIER";
    case TOK_BOOL:          return "TOK_BOOL";
    case TOK_CLASS:         return "TOK_CLASS";
    case TOK_ELSE:          return "TOK_ELSE";
    case TOK_EXTENDS:       return "TOK_EXTENDS";
    case TOK_FALSE:         return "TOK_FALSE";
    case TOK_IF:            return "TOK_IF";
    case TOK_INT:           return "TOK_INT";
    case TOK_NEW:           return "TOK_NEW";
    case TOK_PRIVATE:       return "TOK_PRIVATE";
    case TOK_PUBLIC:        return "TOK_PUBLIC";
    case TOK_RETURN:        return "TOK_RETURN";
    case TOK_STATIC:        return "TOK_STATIC";
    case TOK_STRING:        return "TOK_STRING";
    case TOK_THIS:          return "TOK_THIS";
    case TOK_TRUE:          return "TOK_TRUE";
    case TOK_VOID:          return "TOK_VOID";
    case TOK_WHILE:         return "TOK_WHILE";
    case TOK_LBRACE:        return "TOK_LBRACE";
    case TOK_RBRACE:        return "TOK_RBRACE";
    case TOK_LBRACKET:      return "TOK_LBRACKET";
    case TOK_RBRACKET:      return "TOK_RBRACKET";
    case TOK_LPAREN:        return "TOK_LPAREN";
    case TOK_RPAREN:        return "TOK_RPAREN";
    case TOK_EXCLAIM:       return "TOK_EXCLAIM";
    case TOK_EXCLAIMEQUAL:  return "TOK_EXCLAIMEQUAL";
    case TOK_EQUAL:         return "TOK_EQUAL";
    case TOK_EQUALEQUAL:    return "TOK_EQUALEQUAL";
    case TOK_LESS:          return "TOK_LESS";
    case TOK_LESSEQUAL:     return "TOK_LESSEQUAL";
    case TOK_GREATER:       return "TOK_GREATER";
    case TOK_GREATEREQUAL:  return "TOK_GREATEREQUAL";
    case TOK_AMPAMP:        return "TOK_AMPAMP";
    case TOK_PIPEPIPE:      return "TOK_PIPEPIPE";
    case TOK_PLUS:          return "TOK_PLUS";
    case TOK_MINUS:         return "TOK_MINUS";
    case TOK_STAR:          return "TOK_STAR";
    case TOK_SLASH:         return "TOK_SLASH";
    case TOK_PERCENT:       return "TOK_PERCENT";
    case TOK_COMMA:         return "TOK_COMMA";
    case TOK_DOT:           return "TOK_DOT";
    case TOK_SEMICOLON:     return "TOK_SEMICOLON";
    case TOK_ERROR:         return "TOK_ERROR";
    case TOK_NUMBER:        return "TOK_NUMBER";
    default:                return "<invalid token>";
    }
}

enum struct Command {
    Scan,
    Parse,
};

static constexpr std::pair<std::string_view, Command> CommandMap[] = {
    {"scan", Command::Scan},
    {"parse", Command::Parse},
};

int main(int argc, const char** argv)
{
    if (argc != 3) {
        std::fprintf(stderr, "Usage: %s command FILE\n", argv[0]);
        return 1;
    }

    Command command;
    {
        auto* it = std::find_if(std::begin(CommandMap), std::end(CommandMap),
                                [&](auto& e) { return e.first == argv[1]; });
        if (it == std::end(CommandMap)) {
            std::fprintf(stderr,
                         "Invalid command: %s.\n"
                         "Valid commands: scan parse.\n",
                         argv[1]);
            return 2;
        }
        command = it->second;
    }

    const char* filename = argv[2];
    FILE* file = std::fopen(filename, "r");
    if (file == nullptr) {
        std::fprintf(stderr, "Unable to open file %s\n", filename);
        return 3;
    }

    yyscan_t lexer;
    if (yylex_init(&lexer) != 0) {
        std::fprintf(stderr, "Lexer initialization failed\n");
        return 4;
    }

    YY_BUFFER_STATE buffer = yy_create_buffer(file, YY_BUF_SIZE, lexer);
    yy_switch_to_buffer(buffer, lexer);

    switch (command) {
    case Command::Scan:
        while (int token = yylex(lexer)) {
            std::puts(token_to_string(token));
        }
        break;
    case Command::Parse:
        yyparse(lexer);
        break;
    }

    yy_delete_buffer(buffer, lexer);
    return 0;
}
