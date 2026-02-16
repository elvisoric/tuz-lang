#pragma once

#include <cmath>
#include <functional>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

namespace tuz {
namespace test {

// Test result tracking
struct TestResult {
  std::string name;
  bool passed;
  std::string message;
  std::string file;
  int line;
};

class TestRunner {
public:
  static TestRunner& instance() {
    static TestRunner runner;
    return runner;
  }

  void register_test(const std::string& name, std::function<void()> test_func,
                     const std::string& file, int line) {
    tests_.push_back({name, test_func, file, line});
  }

  int run_all() {
    int passed = 0;
    int failed = 0;

    std::cout << "\n=== Running Tests ===\n\n";

    for (auto& test : tests_) {
      current_test_name_ = test.name;
      test_results_.clear();

      try {
        test.func();

        if (test_results_.empty()) {
          std::cout << "[PASS] " << test.name << "\n";
          passed++;
        } else {
          bool all_passed = true;
          for (auto& result : test_results_) {
            if (!result.passed) {
              all_passed = false;
              break;
            }
          }

          if (all_passed) {
            std::cout << "[PASS] " << test.name << "\n";
            passed++;
          } else {
            std::cout << "[FAIL] " << test.name << "\n";
            failed++;
            for (auto& result : test_results_) {
              if (!result.passed) {
                std::cout << "       " << result.message << "\n";
                std::cout << "       at " << result.file << ":" << result.line << "\n";
              }
            }
          }
        }
      } catch (const std::exception& e) {
        std::cout << "[FAIL] " << test.name << " (exception: " << e.what() << ")\n";
        std::cout << "       at " << test.file << ":" << test.line << "\n";
        failed++;
      } catch (...) {
        std::cout << "[FAIL] " << test.name << " (unknown exception)\n";
        std::cout << "       at " << test.file << ":" << test.line << "\n";
        failed++;
      }
    }

    std::cout << "\n=== Results ===\n";
    std::cout << "Total:  " << (passed + failed) << "\n";
    std::cout << "Passed: " << passed << "\n";
    std::cout << "Failed: " << failed << "\n";

    return failed > 0 ? 1 : 0;
  }

  void add_result(bool passed, const std::string& message, const std::string& file, int line) {
    test_results_.push_back({current_test_name_, passed, message, file, line});
  }

private:
  struct Test {
    std::string name;
    std::function<void()> func;
    std::string file;
    int line;
  };

  std::vector<Test> tests_;
  std::vector<TestResult> test_results_;
  std::string current_test_name_;
};

// Test registration helper
struct TestRegistrar {
  TestRegistrar(const std::string& name, std::function<void()> func, const std::string& file,
                int line) {
    TestRunner::instance().register_test(name, func, file, line);
  }
};

// Assertion macros
#define TEST_ASSERT_TRUE(expr)                                                                     \
  do {                                                                                             \
    bool result = (expr);                                                                          \
    tuz::test::TestRunner::instance().add_result(                                                  \
        result, std::string("ASSERT_TRUE failed: ") + #expr, __FILE__, __LINE__);                  \
  } while (0)

#define TEST_ASSERT_FALSE(expr)                                                                    \
  do {                                                                                             \
    bool result = !(expr);                                                                         \
    tuz::test::TestRunner::instance().add_result(                                                  \
        result, std::string("ASSERT_FALSE failed: ") + #expr, __FILE__, __LINE__);                 \
  } while (0)

#define TEST_ASSERT_EQ(expected, actual)                                                           \
  do {                                                                                             \
    bool result = (expected) == (actual);                                                          \
    std::stringstream ss;                                                                          \
    if (!result) {                                                                                 \
      ss << "ASSERT_EQ failed: expected " << (expected) << " but got " << (actual);                \
    }                                                                                              \
    tuz::test::TestRunner::instance().add_result(result, ss.str(), __FILE__, __LINE__);            \
  } while (0)

#define TEST_ASSERT_NE(expected, actual)                                                           \
  do {                                                                                             \
    bool result = (expected) != (actual);                                                          \
    std::stringstream ss;                                                                          \
    if (!result) {                                                                                 \
      ss << "ASSERT_NE failed: expected not equal to " << (expected) << " but got " << (actual);   \
    }                                                                                              \
    tuz::test::TestRunner::instance().add_result(result, ss.str(), __FILE__, __LINE__);            \
  } while (0)

#define TEST_ASSERT_THROW(expr, exception_type)                                                    \
  do {                                                                                             \
    bool caught = false;                                                                           \
    try {                                                                                          \
      expr;                                                                                        \
    } catch (const exception_type&) {                                                              \
      caught = true;                                                                               \
    } catch (...) {                                                                                \
    }                                                                                              \
    tuz::test::TestRunner::instance().add_result(                                                  \
        caught, std::string("ASSERT_THROW failed: expected ") + #exception_type, __FILE__,         \
        __LINE__);                                                                                 \
  } while (0)

#define TEST_ASSERT_NO_THROW(expr)                                                                 \
  do {                                                                                             \
    bool no_throw = true;                                                                          \
    std::string error_msg;                                                                         \
    try {                                                                                          \
      expr;                                                                                        \
    } catch (const std::exception& e) {                                                            \
      no_throw = false;                                                                            \
      error_msg = e.what();                                                                        \
    } catch (...) {                                                                                \
      no_throw = false;                                                                            \
      error_msg = "unknown exception";                                                             \
    }                                                                                              \
    std::stringstream ss;                                                                          \
    if (!no_throw) {                                                                               \
      ss << "ASSERT_NO_THROW failed: threw " << error_msg;                                         \
    }                                                                                              \
    tuz::test::TestRunner::instance().add_result(no_throw, ss.str(), __FILE__, __LINE__);          \
  } while (0)

// Test registration macro
#define TEST(name)                                                                                 \
  void test_##name();                                                                              \
  static tuz::test::TestRegistrar registrar_##name(#name, test_##name, __FILE__, __LINE__);        \
  void test_##name()

} // namespace test
} // namespace tuz

// Main entry point
#define TEST_MAIN()                                                                                \
  int main() {                                                                                     \
    return tuz::test::TestRunner::instance().run_all();                                            \
  }
