#include "test_framework.h"
#include "tuz/diagnostic.h"

#include <cstring>
#include <sstream>

using namespace tuz;
using namespace tuz::test;

// =============================================================================
// SourceLocation Tests
// =============================================================================

TEST(source_location_default_constructor) {
  SourceLocation loc;
  TEST_ASSERT_EQ(0u, loc.line);
  TEST_ASSERT_EQ(0u, loc.column);
  TEST_ASSERT_EQ(0u, loc.length);
  TEST_ASSERT_FALSE(loc.is_valid());
}

TEST(source_location_constructor) {
  SourceLocation loc(5, 10, 3);
  TEST_ASSERT_EQ(5u, loc.line);
  TEST_ASSERT_EQ(10u, loc.column);
  TEST_ASSERT_EQ(3u, loc.length);
  TEST_ASSERT_TRUE(loc.is_valid());
}

TEST(source_location_zero_line_invalid) {
  SourceLocation loc(0, 5, 1);
  TEST_ASSERT_FALSE(loc.is_valid());
}

// =============================================================================
// SourceFile Tests
// =============================================================================

TEST(source_file_basic_line_access) {
  std::string content = "line one\nline two\nline three\n";
  SourceFile file("test.tz", content);

  TEST_ASSERT_EQ(std::string("line one"), file.get_line(1));
  TEST_ASSERT_EQ(std::string("line two"), file.get_line(2));
  TEST_ASSERT_EQ(std::string("line three"), file.get_line(3));
}

TEST(source_file_no_trailing_newline) {
  std::string content = "line one\nline two";
  SourceFile file("test.tz", content);

  TEST_ASSERT_EQ(2u, file.line_count());
  TEST_ASSERT_EQ(std::string("line one"), file.get_line(1));
  TEST_ASSERT_EQ(std::string("line two"), file.get_line(2));
}

TEST(source_file_empty_content) {
  SourceFile file("test.tz", "");
  TEST_ASSERT_EQ(1u, file.line_count()); // Empty file has one empty line
  TEST_ASSERT_EQ(std::string(""), file.get_line(1));
}

TEST(source_file_single_line) {
  SourceFile file("test.tz", "single line");
  TEST_ASSERT_EQ(1u, file.line_count());
  TEST_ASSERT_EQ(std::string("single line"), file.get_line(1));
}

TEST(source_file_windows_line_endings) {
  std::string content = "line one\r\nline two\r\n";
  SourceFile file("test.tz", content);

  TEST_ASSERT_EQ(std::string("line one"), file.get_line(1));
  TEST_ASSERT_EQ(std::string("line two"), file.get_line(2));
}

TEST(source_file_invalid_line) {
  SourceFile file("test.tz", "line one\nline two\n");

  TEST_ASSERT_EQ(std::string(""), file.get_line(0));   // Invalid
  TEST_ASSERT_EQ(std::string(""), file.get_line(100)); // Out of range
}

TEST(source_file_path) {
  SourceFile file("/path/to/test.tz", "content");
  TEST_ASSERT_EQ(std::string("/path/to/test.tz"), file.path());
}

TEST(source_file_content) {
  std::string content = "hello world\n";
  SourceFile file("test.tz", content);
  TEST_ASSERT_EQ(content, file.content());
}

// =============================================================================
// SourceManager Tests
// =============================================================================

TEST(source_manager_load_and_get) {
  // Create a temporary file
  const char* temp_file = "/tmp/tuz_test_source.tz";
  {
    FILE* f = fopen(temp_file, "w");
    fprintf(f, "fn main() -> int {\n    return 0;\n}");
    fclose(f);
  }

  SourceManager manager;
  auto file = manager.load_file(temp_file);

  TEST_ASSERT_TRUE(file != nullptr);
  TEST_ASSERT_EQ(std::string(temp_file), file->path());
  TEST_ASSERT_EQ(3u, file->line_count());

  // Get already loaded file
  auto file2 = manager.get_file(temp_file);
  TEST_ASSERT_TRUE(file2 != nullptr);
  TEST_ASSERT_EQ(file.get(), file2.get()); // Same pointer

  // Cleanup
  remove(temp_file);
}

TEST(source_manager_load_nonexistent_file) {
  SourceManager manager;
  auto file = manager.load_file("/nonexistent/path/file.tz");
  TEST_ASSERT_TRUE(file == nullptr);
}

TEST(source_manager_get_nonexistent_file) {
  SourceManager manager;
  auto file = manager.get_file("/nonexistent/path/file.tz");
  TEST_ASSERT_TRUE(file == nullptr);
}

TEST(source_manager_main_file) {
  SourceManager manager;
  auto file = std::make_shared<SourceFile>("test.tz", "content");

  manager.set_main_file(file);
  auto main = manager.get_main_file();

  TEST_ASSERT_TRUE(main != nullptr);
  TEST_ASSERT_EQ(file.get(), main.get());
}

// =============================================================================
// DiagnosticMessage Tests
// =============================================================================

TEST(diagnostic_message_constructor) {
  auto file = std::make_shared<SourceFile>("test.tz", "content");
  DiagnosticMessage msg(DiagnosticLevel::Error, "test error", SourceLocation(1, 2, 3), file);

  TEST_ASSERT_TRUE(msg.level == DiagnosticLevel::Error);
  TEST_ASSERT_EQ(std::string("test error"), msg.message);
  TEST_ASSERT_EQ(1u, msg.location.line);
  TEST_ASSERT_EQ(2u, msg.location.column);
  TEST_ASSERT_EQ(3u, msg.location.length);
  TEST_ASSERT_TRUE(msg.file != nullptr);
}

TEST(diagnostic_message_notes) {
  DiagnosticMessage msg(DiagnosticLevel::Error, "main error", SourceLocation(), nullptr);
  msg.notes.emplace_back(DiagnosticLevel::Note, "helpful note", SourceLocation(5, 1, 1), nullptr);

  TEST_ASSERT_EQ(1u, msg.notes.size());
  TEST_ASSERT_TRUE(msg.notes[0].level == DiagnosticLevel::Note);
  TEST_ASSERT_EQ(std::string("helpful note"), msg.notes[0].message);
}

// =============================================================================
// ConsoleDiagnosticConsumer Tests
// =============================================================================

TEST(console_consumer_initial_state) {
  ConsoleDiagnosticConsumer consumer(false, false);
  TEST_ASSERT_FALSE(consumer.has_errors());
  TEST_ASSERT_EQ(0u, consumer.error_count());
  TEST_ASSERT_EQ(0u, consumer.warning_count());
}

TEST(console_consumer_error_count) {
  ConsoleDiagnosticConsumer consumer(false, false);

  DiagnosticMessage msg(DiagnosticLevel::Error, "test error", SourceLocation(), nullptr);
  consumer.consume(msg);

  TEST_ASSERT_TRUE(consumer.has_errors());
  TEST_ASSERT_EQ(1u, consumer.error_count());
  TEST_ASSERT_EQ(0u, consumer.warning_count());
}

TEST(console_consumer_warning_count) {
  ConsoleDiagnosticConsumer consumer(false, false);

  DiagnosticMessage msg(DiagnosticLevel::Warning, "test warning", SourceLocation(), nullptr);
  consumer.consume(msg);

  TEST_ASSERT_FALSE(consumer.has_errors());
  TEST_ASSERT_EQ(0u, consumer.error_count());
  TEST_ASSERT_EQ(1u, consumer.warning_count());
}

TEST(console_consumer_note_does_not_count) {
  ConsoleDiagnosticConsumer consumer(false, false);

  DiagnosticMessage msg(DiagnosticLevel::Note, "test note", SourceLocation(), nullptr);
  consumer.consume(msg);

  TEST_ASSERT_FALSE(consumer.has_errors());
  TEST_ASSERT_EQ(0u, consumer.error_count());
  TEST_ASSERT_EQ(0u, consumer.warning_count());
}

TEST(console_consumer_reset) {
  ConsoleDiagnosticConsumer consumer(false, false);

  consumer.consume(DiagnosticMessage(DiagnosticLevel::Error, "error", SourceLocation(), nullptr));
  consumer.consume(
      DiagnosticMessage(DiagnosticLevel::Warning, "warning", SourceLocation(), nullptr));

  TEST_ASSERT_TRUE(consumer.has_errors());
  TEST_ASSERT_EQ(1u, consumer.error_count());
  TEST_ASSERT_EQ(1u, consumer.warning_count());

  consumer.reset();

  TEST_ASSERT_FALSE(consumer.has_errors());
  TEST_ASSERT_EQ(0u, consumer.error_count());
  TEST_ASSERT_EQ(0u, consumer.warning_count());
}

TEST(console_consumer_multiple_errors) {
  ConsoleDiagnosticConsumer consumer(false, false);

  consumer.consume(DiagnosticMessage(DiagnosticLevel::Error, "error1", SourceLocation(), nullptr));
  consumer.consume(DiagnosticMessage(DiagnosticLevel::Error, "error2", SourceLocation(), nullptr));
  consumer.consume(
      DiagnosticMessage(DiagnosticLevel::Warning, "warning", SourceLocation(), nullptr));

  TEST_ASSERT_EQ(2u, consumer.error_count());
  TEST_ASSERT_EQ(1u, consumer.warning_count());
}

// =============================================================================
// DiagnosticEngine Tests
// =============================================================================

TEST(diagnostic_engine_no_consumer) {
  DiagnosticEngine engine;
  // Should not crash with no consumer
  engine.error("test error");
  engine.warning("test warning");
  engine.note("test note");
  engine.fatal("test fatal");

  // Without consumer, has_errors should return false
  TEST_ASSERT_FALSE(engine.has_errors());
}

TEST(diagnostic_engine_error_reporting) {
  DiagnosticEngine engine;
  auto consumer = std::make_unique<ConsoleDiagnosticConsumer>(false, false);
  auto* consumer_ptr = consumer.get();

  engine.set_consumer(std::move(consumer));
  engine.error("test error");

  TEST_ASSERT_TRUE(engine.has_errors());
  TEST_ASSERT_EQ(1u, consumer_ptr->error_count());
}

TEST(diagnostic_engine_with_source_location) {
  DiagnosticEngine engine;
  auto consumer = std::make_unique<ConsoleDiagnosticConsumer>(false, false);
  auto* consumer_ptr = consumer.get();
  auto file = std::make_shared<SourceFile>("test.tz", "fn main() {}");

  engine.set_consumer(std::move(consumer));
  engine.error("test error", SourceLocation(1, 5, 4), file);

  TEST_ASSERT_TRUE(engine.has_errors());
}

TEST(diagnostic_engine_report_at) {
  DiagnosticEngine engine;
  auto consumer = std::make_unique<ConsoleDiagnosticConsumer>(false, false);

  engine.set_consumer(std::move(consumer));
  engine.report_at(DiagnosticLevel::Error, "error at", 10, 5);

  TEST_ASSERT_TRUE(engine.has_errors());
}

TEST(diagnostic_engine_reset) {
  DiagnosticEngine engine;
  engine.set_consumer(std::make_unique<ConsoleDiagnosticConsumer>(false, false));

  engine.error("test error");
  TEST_ASSERT_TRUE(engine.has_errors());

  engine.reset();
  TEST_ASSERT_FALSE(engine.has_errors());
}

TEST(diagnostic_engine_all_levels) {
  DiagnosticEngine engine;
  auto consumer = std::make_unique<ConsoleDiagnosticConsumer>(false, false);
  auto* consumer_ptr = consumer.get();

  engine.set_consumer(std::move(consumer));

  engine.note("note");
  engine.warning("warning");
  engine.error("error");
  engine.fatal("fatal");

  TEST_ASSERT_EQ(2u, consumer_ptr->error_count()); // error and fatal
  TEST_ASSERT_EQ(1u, consumer_ptr->warning_count());
}

TEST(diagnostic_engine_source_manager) {
  DiagnosticEngine engine;
  auto sm = std::make_shared<SourceManager>();

  engine.set_source_manager(sm);
  TEST_ASSERT_EQ(sm.get(), engine.get_source_manager().get());
}

TEST(global_diagnostics_singleton) {
  auto& diag1 = get_global_diagnostics();
  auto& diag2 = get_global_diagnostics();

  // Should be the same instance
  TEST_ASSERT_EQ(&diag1, &diag2);
}

// =============================================================================
// CodeGenError Tests
// =============================================================================

TEST(codegen_error_basic) {
  CodeGenError error("test error");
  TEST_ASSERT_EQ(std::string("test error"), std::string(error.what()));
  TEST_ASSERT_FALSE(error.has_location());
}

TEST(codegen_error_with_location) {
  SourceLocation loc(5, 10, 3);
  CodeGenError error("test error", loc);

  TEST_ASSERT_EQ(std::string("test error"), std::string(error.what()));
  TEST_ASSERT_TRUE(error.has_location());
  TEST_ASSERT_EQ(5u, error.location().line);
  TEST_ASSERT_EQ(10u, error.location().column);
  TEST_ASSERT_EQ(3u, error.location().length);
}

TEST(codegen_error_inherits_runtime_error) {
  CodeGenError error("test error");
  const std::runtime_error& base = error;
  TEST_ASSERT_EQ(std::string("test error"), std::string(base.what()));
}

TEST_MAIN()
