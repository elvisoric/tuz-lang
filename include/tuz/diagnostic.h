#pragma once

#include <cstdint>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

namespace tuz {

// =============================================================================
// Source Location
// =============================================================================

struct SourceLocation {
  uint32_t line;
  uint32_t column;
  uint32_t length; // Length of the token/span

  SourceLocation(uint32_t ln = 0, uint32_t col = 0, uint32_t len = 0)
      : line(ln), column(col), length(len) {}

  bool is_valid() const { return line > 0; }
};

// =============================================================================
// Source File
// =============================================================================

class SourceFile {
public:
  SourceFile(std::string path, std::string content);

  const std::string& path() const { return path_; }
  const std::string& content() const { return content_; }

  // Get a specific line (1-indexed)
  std::string get_line(uint32_t line_num) const;

  // Get line count
  uint32_t line_count() const { return static_cast<uint32_t>(line_offsets_.size()); }

private:
  std::string path_;
  std::string content_;
  std::vector<size_t> line_offsets_; // Offset of each line start

  void compute_line_offsets();
};

// =============================================================================
// Source Manager
// =============================================================================

class SourceManager {
public:
  // Load a source file, returns nullptr if file cannot be read
  std::shared_ptr<SourceFile> load_file(const std::string& path);

  // Get a loaded file by path
  std::shared_ptr<SourceFile> get_file(const std::string& path) const;

  // Get/set the main source file
  void set_main_file(std::shared_ptr<SourceFile> file) { main_file_ = file; }
  std::shared_ptr<SourceFile> get_main_file() const { return main_file_; }

private:
  std::unordered_map<std::string, std::shared_ptr<SourceFile>> files_;
  std::shared_ptr<SourceFile> main_file_;
};

// =============================================================================
// Diagnostic Level
// =============================================================================

enum class DiagnosticLevel {
  Note,    // Additional information
  Warning, // Warning, compilation continues
  Error,   // Error, compilation continues but will fail
  Fatal,   // Fatal error, stops immediately
};

// =============================================================================
// Diagnostic Message
// =============================================================================

struct DiagnosticMessage {
  DiagnosticLevel level;
  std::string message;
  SourceLocation location;
  std::shared_ptr<SourceFile> file;
  std::vector<DiagnosticMessage> notes; // Related notes

  DiagnosticMessage(DiagnosticLevel lvl, std::string msg, SourceLocation loc = {},
                    std::shared_ptr<SourceFile> src = nullptr)
      : level(lvl), message(std::move(msg)), location(loc), file(std::move(src)) {}
};

// =============================================================================
// Diagnostic Consumer (base class for output)
// =============================================================================

class DiagnosticConsumer {
public:
  virtual ~DiagnosticConsumer() = default;
  virtual void consume(const DiagnosticMessage& diagnostic) = 0;
  virtual bool has_errors() const = 0;
  virtual void reset() = 0;
};

// =============================================================================
// Console Diagnostic Consumer
// =============================================================================

class ConsoleDiagnosticConsumer : public DiagnosticConsumer {
public:
  ConsoleDiagnosticConsumer(bool use_colors = true, bool show_source_context = true);

  void consume(const DiagnosticMessage& diagnostic) override;
  bool has_errors() const override { return error_count_ > 0; }
  void reset() override;

  uint32_t error_count() const { return error_count_; }
  uint32_t warning_count() const { return warning_count_; }

private:
  bool use_colors_;
  bool show_source_context_;
  uint32_t error_count_ = 0;
  uint32_t warning_count_ = 0;

  std::string color_for_level(DiagnosticLevel level) const;
  std::string reset_color() const;
  std::string level_to_string(DiagnosticLevel level) const;

  void print_source_context(const DiagnosticMessage& diagnostic);
  void print_message(const DiagnosticMessage& msg, bool is_note = false);
};

// =============================================================================
// Diagnostic Engine
// =============================================================================

class DiagnosticEngine {
public:
  DiagnosticEngine();

  // Set the source manager for looking up source context
  void set_source_manager(std::shared_ptr<SourceManager> sm) { source_manager_ = std::move(sm); }
  std::shared_ptr<SourceManager> get_source_manager() const { return source_manager_; }

  // Set the consumer for output
  void set_consumer(std::unique_ptr<DiagnosticConsumer> consumer) {
    consumer_ = std::move(consumer);
  }
  DiagnosticConsumer* get_consumer() const { return consumer_.get(); }

  // Report a diagnostic
  void report(DiagnosticLevel level, const std::string& message, SourceLocation loc = {},
              std::shared_ptr<SourceFile> file = nullptr);

  // Convenience methods
  void note(const std::string& message, SourceLocation loc = {},
            std::shared_ptr<SourceFile> file = nullptr);
  void warning(const std::string& message, SourceLocation loc = {},
               std::shared_ptr<SourceFile> file = nullptr);
  void error(const std::string& message, SourceLocation loc = {},
             std::shared_ptr<SourceFile> file = nullptr);
  void fatal(const std::string& message, SourceLocation loc = {},
             std::shared_ptr<SourceFile> file = nullptr);

  // Report with current source file context
  void report_at(DiagnosticLevel level, const std::string& message, uint32_t line, uint32_t column);

  // Check if errors occurred
  bool has_errors() const { return consumer_ && consumer_->has_errors(); }
  void reset() {
    if (consumer_)
      consumer_->reset();
  }

private:
  std::shared_ptr<SourceManager> source_manager_;
  std::unique_ptr<DiagnosticConsumer> consumer_;
};

// =============================================================================
// Enhanced CodeGenError with Source Location
// =============================================================================

class CodeGenError : public std::runtime_error {
public:
  CodeGenError(const std::string& msg, SourceLocation loc = {})
      : std::runtime_error(msg), location_(loc) {}

  const SourceLocation& location() const { return location_; }
  bool has_location() const { return location_.is_valid(); }

private:
  SourceLocation location_;
};

// =============================================================================
// Global diagnostic instance (for convenience)
// =============================================================================

DiagnosticEngine& get_global_diagnostics();

} // namespace tuz
