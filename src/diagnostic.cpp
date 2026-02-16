#include "tuz/diagnostic.h"

#include <algorithm>
#include <fstream>
#include <iomanip>
#include <sstream>

namespace tuz {

// =============================================================================
// Source File
// =============================================================================

SourceFile::SourceFile(std::string path, std::string content)
    : path_(std::move(path)), content_(std::move(content)) {
  compute_line_offsets();
}

void SourceFile::compute_line_offsets() {
  line_offsets_.clear();
  line_offsets_.push_back(0); // Line 1 starts at offset 0

  for (size_t i = 0; i < content_.size(); ++i) {
    if (content_[i] == '\n') {
      line_offsets_.push_back(i + 1);
    }
  }
}

std::string SourceFile::get_line(uint32_t line_num) const {
  if (line_num == 0 || line_num > line_offsets_.size()) {
    return "";
  }

  size_t start = line_offsets_[line_num - 1];
  size_t end = content_.size();

  if (line_num < line_offsets_.size()) {
    end = line_offsets_[line_num] - 1; // Exclude the newline
  }

  // Also exclude \r for Windows line endings
  if (end > start && content_[end - 1] == '\r') {
    --end;
  }

  return content_.substr(start, end - start);
}

// =============================================================================
// Source Manager
// =============================================================================

std::shared_ptr<SourceFile> SourceManager::load_file(const std::string& path) {
  auto it = files_.find(path);
  if (it != files_.end()) {
    return it->second;
  }

  std::ifstream file(path);
  if (!file) {
    return nullptr;
  }

  std::stringstream buffer;
  buffer << file.rdbuf();

  auto source_file = std::make_shared<SourceFile>(path, buffer.str());
  files_[path] = source_file;
  return source_file;
}

std::shared_ptr<SourceFile> SourceManager::get_file(const std::string& path) const {
  auto it = files_.find(path);
  if (it != files_.end()) {
    return it->second;
  }
  return nullptr;
}

// =============================================================================
// Console Diagnostic Consumer
// =============================================================================

ConsoleDiagnosticConsumer::ConsoleDiagnosticConsumer(bool use_colors, bool show_source_context)
    : use_colors_(use_colors), show_source_context_(show_source_context) {
}

void ConsoleDiagnosticConsumer::reset() {
  error_count_ = 0;
  warning_count_ = 0;
}

std::string ConsoleDiagnosticConsumer::color_for_level(DiagnosticLevel level) const {
  if (!use_colors_)
    return "";

  switch (level) {
  case DiagnosticLevel::Note:
    return "\033[36m"; // Cyan
  case DiagnosticLevel::Warning:
    return "\033[33m"; // Yellow
  case DiagnosticLevel::Error:
    return "\033[31m"; // Red
  case DiagnosticLevel::Fatal:
    return "\033[35m"; // Magenta
  default:
    return "";
  }
}

std::string ConsoleDiagnosticConsumer::reset_color() const {
  return use_colors_ ? "\033[0m" : "";
}

std::string ConsoleDiagnosticConsumer::level_to_string(DiagnosticLevel level) const {
  switch (level) {
  case DiagnosticLevel::Note:
    return "note";
  case DiagnosticLevel::Warning:
    return "warning";
  case DiagnosticLevel::Error:
    return "error";
  case DiagnosticLevel::Fatal:
    return "fatal error";
  default:
    return "unknown";
  }
}

void ConsoleDiagnosticConsumer::print_source_context(const DiagnosticMessage& diagnostic) {
  if (!diagnostic.file || !diagnostic.location.is_valid()) {
    return;
  }

  auto file = diagnostic.file;
  uint32_t line_num = diagnostic.location.line;
  uint32_t col_num = diagnostic.location.column;
  uint32_t length = diagnostic.location.length;

  // Show up to 3 lines of context
  uint32_t start_line = (line_num > 1) ? line_num - 1 : 1;
  uint32_t end_line = std::min(line_num + 1, file->line_count());

  // Calculate line number width for alignment
  uint32_t line_num_width = std::to_string(end_line).length();

  for (uint32_t current_line = start_line; current_line <= end_line; ++current_line) {
    std::string line_content = file->get_line(current_line);

    // Print line number and content
    std::cerr << std::setw(line_num_width) << current_line << " | " << line_content << "\n";

    // Print caret line for the error line
    if (current_line == line_num && col_num > 0) {
      std::cerr << std::string(line_num_width, ' ') << " | ";

      // Calculate visual column (accounting for tabs)
      size_t visual_col = 0;
      std::string_view line_view = line_content;
      for (size_t i = 0; i < col_num - 1 && i < line_view.size(); ++i) {
        if (line_view[i] == '\t') {
          visual_col = (visual_col + 8) & ~7; // Tab to 8 spaces
        } else {
          ++visual_col;
        }
      }

      std::cerr << std::string(visual_col, ' ');

      // Print carets
      if (use_colors_) {
        std::cerr << color_for_level(diagnostic.level);
      }

      // Default length to 1 if not specified
      uint32_t caret_count = (length > 0) ? length : 1;

      // Clamp caret count to remaining line length
      if (col_num - 1 + caret_count > line_content.size()) {
        caret_count = static_cast<uint32_t>(line_content.size()) - (col_num - 1);
      }

      std::cerr << std::string(std::max(1u, caret_count), '^');

      if (use_colors_) {
        std::cerr << reset_color();
      }

      std::cerr << "\n";
    }
  }
}

void ConsoleDiagnosticConsumer::print_message(const DiagnosticMessage& msg, bool is_note) {
  // Location info
  if (msg.file && msg.location.is_valid()) {
    std::cerr << msg.file->path() << ":" << msg.location.line << ":" << msg.location.column << ": ";
  }

  // Level and color
  if (use_colors_) {
    std::cerr << color_for_level(msg.level);
  }

  if (is_note) {
    std::cerr << "note: ";
  } else {
    std::cerr << level_to_string(msg.level) << ": ";
  }

  if (use_colors_) {
    std::cerr << reset_color();
  }

  // Message
  std::cerr << msg.message << "\n";

  // Source context
  if (show_source_context_ && !is_note) {
    print_source_context(msg);
  }

  // Print notes
  for (const auto& note : msg.notes) {
    print_message(note, true);
  }
}

void ConsoleDiagnosticConsumer::consume(const DiagnosticMessage& diagnostic) {
  switch (diagnostic.level) {
  case DiagnosticLevel::Error:
  case DiagnosticLevel::Fatal:
    ++error_count_;
    break;
  case DiagnosticLevel::Warning:
    ++warning_count_;
    break;
  default:
    break;
  }

  print_message(diagnostic, false);
}

// =============================================================================
// Diagnostic Engine
// =============================================================================

DiagnosticEngine::DiagnosticEngine() = default;

void DiagnosticEngine::report(DiagnosticLevel level, const std::string& message, SourceLocation loc,
                              std::shared_ptr<SourceFile> file) {
  if (!consumer_)
    return;

  // If no file provided but location is valid, try to get from source manager
  if (!file && source_manager_ && source_manager_->get_main_file() && loc.is_valid()) {
    file = source_manager_->get_main_file();
  }

  consumer_->consume(DiagnosticMessage(level, message, loc, file));
}

void DiagnosticEngine::note(const std::string& message, SourceLocation loc,
                            std::shared_ptr<SourceFile> file) {
  report(DiagnosticLevel::Note, message, loc, file);
}

void DiagnosticEngine::warning(const std::string& message, SourceLocation loc,
                               std::shared_ptr<SourceFile> file) {
  report(DiagnosticLevel::Warning, message, loc, file);
}

void DiagnosticEngine::error(const std::string& message, SourceLocation loc,
                             std::shared_ptr<SourceFile> file) {
  report(DiagnosticLevel::Error, message, loc, file);
}

void DiagnosticEngine::fatal(const std::string& message, SourceLocation loc,
                             std::shared_ptr<SourceFile> file) {
  report(DiagnosticLevel::Fatal, message, loc, file);
}

void DiagnosticEngine::report_at(DiagnosticLevel level, const std::string& message, uint32_t line,
                                 uint32_t column) {
  report(level, message, SourceLocation(line, column));
}

// =============================================================================
// Global diagnostic instance
// =============================================================================

DiagnosticEngine& get_global_diagnostics() {
  static DiagnosticEngine instance;
  return instance;
}

} // namespace tuz
