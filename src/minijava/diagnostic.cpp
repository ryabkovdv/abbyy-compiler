#include <minijava/diagnostic.hpp>

#include <minijava/utility.hpp>

#include <fmt/color.h>

using namespace minijava;

DiagnosticEngine::DiagnosticEngine(std::string_view content, FILE* output)
    : m_output(output)
{
    auto it = content.begin();
    while (true) {
        auto nl = std::find(it, content.end(), '\n');
        m_lines.emplace_back(it, nl - it);
        if (it == content.end())
            break;
        it = nl + 1;
    }
}

void DiagnosticEngine::do_diagnostic(DiagKind kind, std::string_view format,
                                     fmt::format_args args)
{
    using fmt::emphasis;
    using fmt::terminal_color;

    std::string_view msg;
    fmt::text_style style;
    switch (kind) {
    case DiagKind::Error:
        ++m_errors;
        msg = "error: ";
        style = emphasis::bold | fg(terminal_color::red);
        break;
    case DiagKind::Warning:
        ++m_warnings;
        msg = "warning: ";
        style = emphasis::bold | fg(terminal_color::magenta);
        break;
    case DiagKind::Note:
        msg = "note: ";
        style = emphasis::bold | fg(terminal_color::white);
        break;
    }

    fmt::print(m_output, style, msg);
    fmt::vprint(m_output, emphasis::bold, format, args);
    std::fputc('\n', m_output);
}

void DiagnosticEngine::do_diagnostic(DiagKind kind, Location loc,
                                     std::string_view format,
                                     fmt::format_args args)
{
    using fmt::emphasis;
    using fmt::terminal_color;

    fmt::print(m_output, emphasis::bold, "{}:{}: ", loc.first_line,
               loc.first_column);
    do_diagnostic(kind, format, args);

    std::string_view line = "<invalid line>";
    if (size_t(loc.first_line) - 1 < m_lines.size())
        line = m_lines[loc.first_line - 1];

    std::fwrite(line.data(), 1, line.size(), m_output);
    fmt::print(m_output, emphasis::bold | fg(terminal_color::green),
               "\n{:>{}}\n", '^', loc.first_column);
}
