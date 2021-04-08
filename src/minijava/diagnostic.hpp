#ifndef MINIJAVA_DIAGNOSTIC_HPP
#define MINIJAVA_DIAGNOSTIC_HPP

#include <fmt/core.h>

#include <vector>

namespace minijava {

struct Location {
    int first_line = 0;
    int first_column = 0;
    int last_line = 0;
    int last_column = 0;
};

class DiagnosticEngine {
public:
    explicit DiagnosticEngine(std::string_view content, FILE* output = stderr);

    int error_count() const noexcept
    {
        return m_errors;
    }

    int warning_count() const noexcept
    {
        return m_warnings;
    }

    template <typename... Args>
    void error(std::string_view format, const Args&... args)
    {
        do_diagnostic(DiagKind::Error, format, fmt::make_format_args(args...));
    }

    template <typename... Args>
    void warn(std::string_view format, const Args&... args)
    {
        do_diagnostic(DiagKind::Warning, format,
                      fmt::make_format_args(args...));
    }

    template <typename... Args>
    void note(std::string_view format, const Args&... args)
    {
        do_diagnostic(DiagKind::Note, format, fmt::make_format_args(args...));
    }

    template <typename... Args>
    void error(Location loc, std::string_view format, const Args&... args)
    {
        do_diagnostic(DiagKind::Error, loc, format,
                      fmt::make_format_args(args...));
    }

    template <typename... Args>
    void warn(Location loc, std::string_view format, const Args&... args)
    {
        do_diagnostic(DiagKind::Warning, loc, format,
                      fmt::make_format_args(args...));
    }

    template <typename... Args>
    void note(Location loc, std::string_view format, const Args&... args)
    {
        do_diagnostic(DiagKind::Note, loc, format,
                      fmt::make_format_args(args...));
    }

private:
    enum struct DiagKind {
        Error,
        Warning,
        Note,
    };

    int m_errors = 0;
    int m_warnings = 0;
    FILE* m_output;
    std::vector<std::string_view> m_lines;

    void do_diagnostic(DiagKind kind, std::string_view format,
                       fmt::format_args args);

    void do_diagnostic(DiagKind kind, Location loc, std::string_view format,
                       fmt::format_args args);
};

} // namespace minijava

#endif
