#include "tuz/type.h"

#include <algorithm>

namespace tuz {

// Singleton type instances
static TypePtr void_type = std::make_shared<Type>(TypeKind::Void);
static TypePtr int8_type = std::make_shared<Type>(TypeKind::Int8);
static TypePtr int16_type = std::make_shared<Type>(TypeKind::Int16);
static TypePtr int32_type = std::make_shared<Type>(TypeKind::Int32);
static TypePtr int64_type = std::make_shared<Type>(TypeKind::Int64);
static TypePtr uint8_type = std::make_shared<Type>(TypeKind::UInt8);
static TypePtr uint16_type = std::make_shared<Type>(TypeKind::UInt16);
static TypePtr uint32_type = std::make_shared<Type>(TypeKind::UInt32);
static TypePtr uint64_type = std::make_shared<Type>(TypeKind::UInt64);
static TypePtr float32_type = std::make_shared<Type>(TypeKind::Float32);
static TypePtr float64_type = std::make_shared<Type>(TypeKind::Float64);
static TypePtr bool_type = std::make_shared<Type>(TypeKind::Bool);

bool Type::is_integer() const {
  return kind >= TypeKind::Int8 && kind <= TypeKind::UInt64;
}

bool Type::is_signed_integer() const {
  return kind >= TypeKind::Int8 && kind <= TypeKind::Int64;
}

bool Type::is_unsigned_integer() const {
  return kind >= TypeKind::UInt8 && kind <= TypeKind::UInt64;
}

bool Type::is_floating_point() const {
  return kind == TypeKind::Float32 || kind == TypeKind::Float64;
}

bool Type::is_numeric() const {
  return is_integer() || is_floating_point();
}

bool Type::is_boolean() const {
  return kind == TypeKind::Bool;
}

bool Type::is_void() const {
  return kind == TypeKind::Void;
}

bool Type::is_pointer() const {
  return kind == TypeKind::Pointer;
}

bool Type::is_array() const {
  return kind == TypeKind::Array;
}

bool Type::is_function() const {
  return kind == TypeKind::Function;
}

bool Type::is_struct() const {
  return kind == TypeKind::Struct;
}

bool Type::is_reference() const {
  return kind == TypeKind::Reference;
}

std::string Type::to_string() const {
  switch (kind) {
  case TypeKind::Void:
    return "void";
  case TypeKind::Int8:
    return "i8";
  case TypeKind::Int16:
    return "i16";
  case TypeKind::Int32:
    return "i32";
  case TypeKind::Int64:
    return "i64";
  case TypeKind::UInt8:
    return "u8";
  case TypeKind::UInt16:
    return "u16";
  case TypeKind::UInt32:
    return "u32";
  case TypeKind::UInt64:
    return "u64";
  case TypeKind::Float32:
    return "f32";
  case TypeKind::Float64:
    return "f64";
  case TypeKind::Bool:
    return "bool";
  default:
    return "unknown";
  }
}

bool Type::equals(const Type& other) const {
  if (kind != other.kind)
    return false;
  return true; // Base types are equal if kinds match
}

size_t Type::size() const {
  switch (kind) {
  case TypeKind::Void:
    return 0;
  case TypeKind::Int8:
    return 1;
  case TypeKind::Int16:
    return 2;
  case TypeKind::Int32:
    return 4;
  case TypeKind::Int64:
    return 8;
  case TypeKind::UInt8:
    return 1;
  case TypeKind::UInt16:
    return 2;
  case TypeKind::UInt32:
    return 4;
  case TypeKind::UInt64:
    return 8;
  case TypeKind::Float32:
    return 4;
  case TypeKind::Float64:
    return 8;
  case TypeKind::Bool:
    return 1;
  default:
    return 8; // Pointer size
  }
}

size_t Type::alignment() const {
  return size(); // Simple alignment strategy
}

// PointerType
std::string PointerType::to_string() const {
  return "*" + pointee->to_string();
}

bool PointerType::equals(const Type& other) const {
  if (other.kind != TypeKind::Pointer)
    return false;
  const auto& other_ptr = static_cast<const PointerType&>(other);
  return pointee->equals(*other_ptr.pointee);
}

TypePtr PointerType::get(TypePtr pointee) {
  return std::make_shared<PointerType>(pointee);
}

// ArrayType
std::string ArrayType::to_string() const {
  return "[" + element_type->to_string() + "; " + std::to_string(size_val) + "]";
}

bool ArrayType::equals(const Type& other) const {
  if (other.kind != TypeKind::Array)
    return false;
  const auto& other_arr = static_cast<const ArrayType&>(other);
  return size_val == other_arr.size_val && element_type->equals(*other_arr.element_type);
}

size_t ArrayType::size() const {
  return element_type->size() * size_val;
}

// FunctionType
std::string FunctionType::to_string() const {
  std::string result = "fn(";
  for (size_t i = 0; i < param_types.size(); i++) {
    if (i > 0)
      result += ", ";
    result += param_types[i]->to_string();
  }
  result += ") -> " + return_type->to_string();
  return result;
}

bool FunctionType::equals(const Type& other) const {
  if (other.kind != TypeKind::Function)
    return false;
  const auto& other_fn = static_cast<const FunctionType&>(other);
  if (!return_type->equals(*other_fn.return_type))
    return false;
  if (param_types.size() != other_fn.param_types.size())
    return false;
  for (size_t i = 0; i < param_types.size(); i++) {
    if (!param_types[i]->equals(*other_fn.param_types[i]))
      return false;
  }
  return true;
}

// StructType
std::string StructType::to_string() const {
  return "struct " + name;
}

bool StructType::equals(const Type& other) const {
  if (other.kind != TypeKind::Struct)
    return false;
  const auto& other_struct = static_cast<const StructType&>(other);
  return name == other_struct.name;
}

size_t StructType::size() const {
  if (cached_size)
    return *cached_size;

  size_t total = 0;
  size_t max_align = 1;

  for (const auto& [name, type] : fields) {
    size_t align = type->alignment();
    size_t padding = (align - (total % align)) % align;
    total += padding + type->size();
    max_align = std::max(max_align, align);
  }

  // Final padding
  size_t padding = (max_align - (total % max_align)) % max_align;
  total += padding;

  cached_size = total;
  return total;
}

size_t StructType::alignment() const {
  if (cached_alignment)
    return *cached_alignment;

  size_t max_align = 1;
  for (const auto& [name, type] : fields) {
    max_align = std::max(max_align, type->alignment());
  }

  cached_alignment = max_align;
  return max_align;
}

std::optional<size_t> StructType::get_field_offset(const std::string& field_name) const {
  size_t offset = 0;
  for (const auto& [name, type] : fields) {
    size_t align = type->alignment();
    offset += (align - (offset % align)) % align;
    if (name == field_name) {
      return offset;
    }
    offset += type->size();
  }
  return std::nullopt;
}

int StructType::get_field_index(const std::string& field_name) const {
  for (size_t i = 0; i < fields.size(); ++i) {
    if (fields[i].first == field_name) {
      return (int)i;
    }
  }
  return -1;
}

TypePtr StructType::get_field_type(const std::string& field_name) const {
  for (const auto& [name, type] : fields) {
    if (name == field_name) {
      return type;
    }
  }
  return nullptr;
}

// ReferenceType
std::string ReferenceType::to_string() const {
  return std::string(is_mutable ? "&mut " : "&") + referent->to_string();
}

bool ReferenceType::equals(const Type& other) const {
  if (other.kind != TypeKind::Reference)
    return false;
  const auto& other_ref = static_cast<const ReferenceType&>(other);
  return is_mutable == other_ref.is_mutable && referent->equals(*other_ref.referent);
}

// Type factories
TypePtr get_void_type() {
  return void_type;
}
TypePtr get_int8_type() {
  return int8_type;
}
TypePtr get_int16_type() {
  return int16_type;
}
TypePtr get_int32_type() {
  return int32_type;
}
TypePtr get_int64_type() {
  return int64_type;
}
TypePtr get_uint8_type() {
  return uint8_type;
}
TypePtr get_uint16_type() {
  return uint16_type;
}
TypePtr get_uint32_type() {
  return uint32_type;
}
TypePtr get_uint64_type() {
  return uint64_type;
}
TypePtr get_float32_type() {
  return float32_type;
}
TypePtr get_float64_type() {
  return float64_type;
}
TypePtr get_bool_type() {
  return bool_type;
}

TypePtr parse_type(std::string_view name) {
  if (name == "void")
    return get_void_type();
  if (name == "i8")
    return get_int8_type();
  if (name == "i16")
    return get_int16_type();
  if (name == "i32" || name == "int")
    return get_int32_type();
  if (name == "i64")
    return get_int64_type();
  if (name == "u8")
    return get_uint8_type();
  if (name == "u16")
    return get_uint16_type();
  if (name == "u32")
    return get_uint32_type();
  if (name == "u64")
    return get_uint64_type();
  if (name == "f32" || name == "float")
    return get_float32_type();
  if (name == "f64" || name == "double")
    return get_float64_type();
  if (name == "bool")
    return get_bool_type();
  return nullptr;
}

bool is_implicitly_convertible(TypePtr from, TypePtr to) {
  if (from->equals(*to))
    return true;

  // Numeric conversions
  if (from->is_numeric() && to->is_numeric()) {
    // Allow any numeric to numeric (with potential loss)
    return true;
  }

  // Pointer to pointer (TODO: check pointee compatibility)
  if (from->is_pointer() && to->is_pointer()) {
    return true;
  }

  // Null pointer to any pointer
  // (would need a null pointer type)

  return false;
}

TypePtr common_type(TypePtr a, TypePtr b) {
  if (a->equals(*b))
    return a;

  if (a->is_floating_point() || b->is_floating_point()) {
    // Return the larger float type
    if (a->kind == TypeKind::Float64 || b->kind == TypeKind::Float64) {
      return get_float64_type();
    }
    return get_float32_type();
  }

  if (a->is_integer() && b->is_integer()) {
    // Return the larger integer type
    size_t a_size = a->size();
    size_t b_size = b->size();

    if (a_size > b_size)
      return a;
    if (b_size > a_size)
      return b;

    // Same size - prefer unsigned if either is unsigned
    if (a->is_unsigned_integer() || b->is_unsigned_integer()) {
      switch (a->kind) {
      case TypeKind::Int8:
        return get_uint8_type();
      case TypeKind::Int16:
        return get_uint16_type();
      case TypeKind::Int32:
        return get_uint32_type();
      case TypeKind::Int64:
        return get_uint64_type();
      default:
        return a;
      }
    }
    return a;
  }

  return nullptr; // No common type
}

} // namespace tuz
