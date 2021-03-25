#include <minijava/parser/parser.hpp>

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
        std::fprintf(stderr, "Usage: %s COMMAND FILE\n", argv[0]);
        return -1;
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
            return -2;
        }
        command = it->second;
    }

    const char* filename = argv[2];
    FILE* file = std::fopen(filename, "r");
    if (file == nullptr) {
        std::fprintf(stderr, "Unable to open file %s\n", filename);
        return -3;
    }

    int error;
    switch (command) {
    case Command::Scan: {
        error = minijava::print_tokens(file);
        break;
    }
    case Command::Parse: {
        minijava::AstTree tree;
        error = minijava::parse(file, &tree);
        auto class_count = tree.classes.size();
        auto stmt_count = tree.main.size();
        if (error == 0) {
            std::printf("Parsed successfully, "
                        "%zu class%s in file, "
                        "%zu statement%s in main\n",
                        class_count, (class_count == 1) ? "" : "es",
                        stmt_count, (stmt_count == 1) ? "" : "s");
        }
        break;
    }
    }
    return error;
}
