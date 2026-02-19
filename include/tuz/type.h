#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace tuz {

// Forward declarations
class Type;
using TypePtr = std::shared_ptr<Type>;

enum class TypeKind {
  Void,
  Int8,
  Int16,
  Int32,
  Int64,
  UInt8,
  UInt16,
  UInt32,
  UInt64,
  Float32,
  Float64,
  Bool,
  Pointer,
  Array,
  Function,
  Struct,
  Reference,
  TypeName
};

class Type : public std::enable_shared_from_this<Type> {
public:
  TypeKind kind;

  explicit Type(TypeKind k) : kind(k) {}
  virtual ~Type() = default;

  bool is_integer() const;
  bool is_signed_integer() const;
  bool is_unsigned_integer() const;
  bool is_floating_point() const;
  bool is_numeric() const;
  bool is_boolean() const;
  bool is_void() const;
  bool is_pointer() const;
  bool is_array() const;
  bool is_function() const;
  bool is_struct() const;
  bool is_reference() const;

  virtual std::string to_string() const;
  virtual bool equals(const Type& other) const;
  virtual size_t size() const;
  virtual size_t alignment() const;
};

class TypeName : public Type {
public:
  std::string type_name;

  explicit TypeName(std::string type_name)
      : Type(TypeKind::TypeName), type_name(std::move(type_name)) {}

  std::string to_string() const override { return type_name; }

  bool equals(const Type& other) const override { return false; }

  size_t size() const override { return 0; }

  size_t alignment() const override { return 0; }
};

// Pointer type
class PointerType : public Type {
public:
  TypePtr pointee;

  explicit PointerType(TypePtr p) : Type(TypeKind::Pointer), pointee(std::move(p)) {}

  std::string to_string() const override;
  bool equals(const Type& other) const override;
  size_t size() const override { return 8; } // 64-bit
  size_t alignment() const override { return 8; }

  static TypePtr get(TypePtr pointee);
};

// Array type
class ArrayType : public Type {
public:
  TypePtr element_type;
  size_t size_val;

  ArrayType(TypePtr elem, size_t sz)
      : Type(TypeKind::Array), element_type(std::move(elem)), size_val(sz) {}

  std::string to_string() const override;
  bool equals(const Type& other) const override;
  size_t size() const override;
  size_t alignment() const override { return element_type->alignment(); }
};

// Function type
class FunctionType : public Type {
public:
  std::vector<TypePtr> param_types;
  TypePtr return_type;

  FunctionType(std::vector<TypePtr> params, TypePtr ret)
      : Type(TypeKind::Function), param_types(std::move(params)), return_type(std::move(ret)) {}

  std::string to_string() const override;
  bool equals(const Type& other) const override;
  size_t size() const override { return 8; } // Function pointer size
  size_t alignment() const override { return 8; }
};

// Struct type
class StructType : public Type {
public:
  std::string name;
  std::vector<std::pair<std::string, TypePtr>> fields;
  mutable std::optional<size_t> cached_size;
  mutable std::optional<size_t> cached_alignment;

  StructType(std::string n, std::vector<std::pair<std::string, TypePtr>> f)
      : Type(TypeKind::Struct), name(std::move(n)), fields(std::move(f)) {}

  std::string to_string() const override;
  bool equals(const Type& other) const override;
  size_t size() const override;
  size_t alignment() const override;
  std::optional<size_t> get_field_offset(const std::string& field_name) const;
  int get_field_index(const std::string& field_name) const;
  TypePtr get_field_type(const std::string& field_name) const;
};

// Reference type (for mutable references)
class ReferenceType : public Type {
public:
  TypePtr referent;
  bool is_mutable;

  ReferenceType(TypePtr ref, bool mut)
      : Type(TypeKind::Reference), referent(std::move(ref)), is_mutable(mut) {}

  std::string to_string() const override;
  bool equals(const Type& other) const override;
  size_t size() const override { return 8; } // Pointer size
  size_t alignment() const override { return 8; }
};

// Type factory - singleton types
TypePtr get_void_type();
TypePtr get_int8_type();
TypePtr get_int16_type();
TypePtr get_int32_type();
TypePtr get_int64_type();
TypePtr get_uint8_type();
TypePtr get_uint16_type();
TypePtr get_uint32_type();
TypePtr get_uint64_type();
TypePtr get_float32_type();
TypePtr get_float64_type();
TypePtr get_bool_type();

// Type parsing from string
TypePtr parse_type(std::string_view name);

// Type checking helpers
bool is_implicitly_convertible(TypePtr from, TypePtr to);
TypePtr common_type(TypePtr a, TypePtr b);

} // namespace tuz
