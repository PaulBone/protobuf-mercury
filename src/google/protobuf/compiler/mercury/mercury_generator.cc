// Protocol Buffers for Mercury
// Copyright 2008 Mission Critical Australia.
// http://code.google.com/p/protobuf-mercury/
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Author: Ian MacLarty (iml@missioncriticalit.com).

#include <utility>
#include <map>
#include <string>
#include <sstream>
#include <vector>

#include <google/protobuf/compiler/mercury/mercury_generator.h>
#include <google/protobuf/compiler/mercury/mercury_strutil.h>
#include <google/protobuf/descriptor.pb.h>

#include <google/protobuf/stubs/common.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/zero_copy_stream.h>
//#include <google/protobuf/stubs/substitute.h>

namespace google {
namespace protobuf {
namespace compiler {
namespace mercury {

// Strip the .proto suffix.  Copied from python_generator.cc.
string StripProto(const string& filename) {
  const char* suffix = HasSuffixString(filename, ".protodevel")
      ? ".protodevel" : ".proto";
  return StripSuffixString(filename, suffix);
}

// Returns the module name.
// TODO: Allow module name to be given as an option.
string ModuleName(const FileDescriptor* file)
{
    return StripProto(file->name());
}

// The following functions return a vector containing all the
// message types or enums in a proto file.  We generate a separate
// type for each message type and enum, since Mercury doesn't have the
// concept of nested types (you can't define a new type inside the
// definition of another type).

void FlattenNestedMessageTypes(const Descriptor *message,
    vector<const Descriptor*> *message_types)
{
    int i;

    for (i = 0; i < message->nested_type_count(); i++) {
        message_types->push_back(message->nested_type(i));
        FlattenNestedMessageTypes(message->nested_type(i), message_types);
    }
}

void FlattenMessageTypes(const FileDescriptor* file,
    vector<const Descriptor*> *message_types)
{
    int i;

    for (i = 0; i < file->message_type_count(); i++) {
        message_types->push_back(file->message_type(i));
        FlattenNestedMessageTypes(file->message_type(i), message_types);
    }
}

void FlattenNestedEnums(const Descriptor *message,
    vector<const EnumDescriptor*> *enums)
{
    int i;

    for (i = 0; i < message->enum_type_count(); i++) {
        enums->push_back(message->enum_type(i));
    }
    for (i = 0; i < message->nested_type_count(); i++) {
        FlattenNestedEnums(message->nested_type(i), enums);
    }
}

void FlattenEnums(const FileDescriptor* file,
    vector<const EnumDescriptor*> *enums)
{
    int i;

    for (i = 0; i < file->enum_type_count(); i++) {
        enums->push_back(file->enum_type(i));
    }
    for (i = 0; i < file->message_type_count(); i++) {
        FlattenNestedEnums(file->message_type(i), enums);
    }
}

// Converts camel case or uppercase string to a lowercase string with
// underscores.
string StringToLowerCaseWithUnderScores(const string str)
{
    string              rstr = "";
    int                 i;
    string              capitals   = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    string              lower_case = "abcdefghijklmnopqrstuvwxyz";
    string::size_type   pos;

    for (i = 0; i < str.size(); i++) {
        pos = capitals.find(str.at(i));
        if (pos == string::npos) {
            rstr.push_back(str.at(i));
        } else {
            // Put underscores before capitals, but only if it is not the
            // first letter and the previous letter is not a capital.
            if (i != 0 && capitals.find(str[i] != string::npos) &&
                    (capitals.find(str[i - 1]) == string::npos)) {
                rstr.push_back('_');
            }
            rstr.push_back(lower_case.at(pos));
        }
    }

    return rstr;
}

// Return the Mercury type name for a message type.
string MessageTypeName(const Descriptor *message_type)
{
    string type_name;

    type_name = StringToLowerCaseWithUnderScores(message_type->name());

    if (message_type->containing_type()) {
        type_name = MessageTypeName(message_type->containing_type()) + "_" +
            type_name;
    }

    return type_name;
}

// Return the Mercury type name for an enumeration.
string EnumTypeName(const EnumDescriptor *enumeration)
{
    string type_name;

    type_name = StringToLowerCaseWithUnderScores(enumeration->name());

    if (enumeration->containing_type()) {
        type_name = MessageTypeName(enumeration->containing_type()) + "_" +
            type_name;
    }

    return type_name;
}

// Return the Mercury field name for a message type field.
string FieldName(const FieldDescriptor *field)
{
    return MessageTypeName(field->containing_type()) + "_" +
        StringToLowerCaseWithUnderScores(field->name());
}

// Return the Mercury type for a field as a string.
string FieldTypeName(const FieldDescriptor *field)
{
    string type_name;

    switch(field->type()) {
        case FieldDescriptor::TYPE_DOUBLE: {
            type_name = "float";
            break;
        }
        case FieldDescriptor::TYPE_INT32:
        case FieldDescriptor::TYPE_SFIXED32:
        case FieldDescriptor::TYPE_SINT32:
        {
            type_name = "int";
            break;
        }
        case FieldDescriptor::TYPE_BOOL: {
            type_name = "bool";
            break;
        }
        case FieldDescriptor::TYPE_STRING: {
            type_name = "string";
            break;
        }
        case FieldDescriptor::TYPE_BYTES: {
            type_name = "bitmap.bitmap";
            break;
        }
        case FieldDescriptor::TYPE_MESSAGE: {
            type_name = MessageTypeName(field->message_type());
            break;
        }
        case FieldDescriptor::TYPE_ENUM: {
            type_name = EnumTypeName(field->enum_type());
            break;
        }
        default: {
            throw field;
        }
    }

    if (field->is_repeated()) {
        type_name = "list(" + type_name + ")";
    }

    return type_name;
}

// Write one field of a message type.
void WriteMessageField(io::Printer *printer, const FieldDescriptor *field)
{
    printer->Print("$field_name$ :: $field_type_string$",
        "field_name", FieldName(field),
        "field_type_string", FieldTypeName(field));
}

// Write the Mercury type for a proto message type.
// Also writes the pb_message instance declaration.
void WriteMessageType(io::Printer *printer, const Descriptor *message_type)
{
    int     i;
    int     field_count = message_type->field_count();
    string  type_name = MessageTypeName(message_type);

    if (field_count == 0) {
        printer->Print(":- type $type_name$ ---> $type_name$.\n\n",
            "type_name", type_name);
    } else {
        printer->Print(":- type $type_name$\n", "type_name", type_name);
        printer->Print("    --->    $type_name$(\n", "type_name", type_name);
    
        for (i = 0; i < field_count; i++) {
            printer->Print("                ");
            WriteMessageField(printer, message_type->field(i));
            if (i < field_count - 1) {
                printer->Print(",");
            }
            printer->Print("\n");
        }

        printer->Print("            ).\n\n");
    }

    printer->Print((":- instance protobuf_runtime.pb_message(" + type_name +
        ").\n\n").c_str());
}

// Return the functor name for an enumeration value.
string EnumValueName(const EnumValueDescriptor *value)
{
    return EnumTypeName(value->type()) + "_" +
        StringToLowerCaseWithUnderScores(value->name());
}

// Write the Mercury type for a proto enumeration.
// Also writes the pb_enumeration instance declaration.
void WriteEnumType(io::Printer *printer, const EnumDescriptor *enumeration)
{
    int     i;
    int     value_count = enumeration->value_count();
    string  type_name = EnumTypeName(enumeration);

    // protoc guarantees that there will always be at least one
    // enum value.
    printer->Print(":- type $type_name$\n", "type_name", type_name);
    printer->Print("    --->    ");
    
    for (i = 0; i < value_count; i++) {
        printer->Print(EnumValueName(enumeration->value(i)).c_str());
        if (i < value_count - 1) {
            printer->Print("\n    ;       ");
        }
    }
    printer->Print(".\n\n");

    printer->Print((":- instance protobuf_runtime.pb_enumeration(" + type_name +
        ").\n\n").c_str());
}

// Write the interface section.
void WriteInterface(io::Printer *printer, const FileDescriptor* file,
    vector<const Descriptor*> *message_types,
    vector<const EnumDescriptor*> *enums)
{
    int     i;
    string  module = ModuleName(file);

    printer->Print(":- interface.\n\n");
    printer->Print(
        ":- import_module protobuf_runtime, bitmap, bool, list.\n\n");

    for (i = 0; i < message_types->size(); i++) {
        WriteMessageType(printer, message_types->at(i));
    }

    for (i = 0; i < enums->size(); i++) {
        WriteEnumType(printer, enums->at(i));
    }
}

// Convert an integer into a string.
string IntToStr(int i)
{
    std::stringstream    ss;
    ss << i;
    return ss.str();
}

// Convert a double to a string.
string DoubleToStr(double d)
{
    std::stringstream    ss;
    ss.setf(ios::scientific);
    ss.precision(100);
    ss << d;
    return ss.str();
}

// Return the protobuf_runtime.field_type value for the
// given field.
string FieldToPBRuntimeTypeStr(const FieldDescriptor *field)
{
    switch(field->type()) {
        case FieldDescriptor::TYPE_DOUBLE: {
            return "pb_double";
        }
        case FieldDescriptor::TYPE_FLOAT: {
            return "pb_float";
        }
        case FieldDescriptor::TYPE_INT64: {
            return "pb_int64";
        }
        case FieldDescriptor::TYPE_UINT64: {
            return "pb_uint64";
        }
        case FieldDescriptor::TYPE_INT32: {
            return "pb_int32";
        }
        case FieldDescriptor::TYPE_FIXED64: {
            return "pb_fixed64";
        }
        case FieldDescriptor::TYPE_FIXED32: {
            return "pb_fixed32";
        }
        case FieldDescriptor::TYPE_BOOL: {
            return "pb_bool";
        }
        case FieldDescriptor::TYPE_STRING: {
            return "pb_string";
        }
        case FieldDescriptor::TYPE_MESSAGE: {
            return "protobuf_runtime.'new embedded_message'("
                "protobuf_runtime.default_value:" +
                MessageTypeName(field->message_type()) + ")";
        }
        case FieldDescriptor::TYPE_BYTES: {
            return "pb_bytes";
        }
        case FieldDescriptor::TYPE_UINT32: {
            return "pb_uint32";
        }
        case FieldDescriptor::TYPE_ENUM: {
            return "protobuf_runtime.'new enumeration'(" +
                EnumValueName(field->enum_type()->value(0)) + ")";
        }
        case FieldDescriptor::TYPE_SFIXED32: {
            return "pb_sfixed32";
        }
        case FieldDescriptor::TYPE_SFIXED64: {
            return "pb_sfixed64";
        }
        case FieldDescriptor::TYPE_SINT32: {
            return "pb_sint32";
        }
        case FieldDescriptor::TYPE_SINT64: {
            return "pb_sint64";
        }
        default: {
            throw field;
        }
    }
}

string FieldLabelToPBRuntimeCardStr(FieldDescriptor::Label label)
{
    switch(label) {
        case FieldDescriptor::LABEL_OPTIONAL: {
            return "optional";
        }
        case FieldDescriptor::LABEL_REQUIRED: {
            return "required";
        }
        case FieldDescriptor::LABEL_REPEATED: {
            return "repeated";
        }
        default: {
            throw "should never happen";
        }
    }
}

// Return the default value of a field as a Mercury term.  If the
// field doesn't have a specified default value then a sensible
// default is returned.
string FieldDefaultValueStr(const FieldDescriptor *field)
{
    string  default_value;

    /*
     * Default values don't (seem to) apply to repeated fields.
     */
    if (field->is_repeated()) {
        return "[]";
    }

    switch(field->type()) {
        case FieldDescriptor::TYPE_DOUBLE: {
            if (field->has_default_value()) {
                default_value = DoubleToStr(field->default_value_double());
            } else {
                default_value = "0.0";
            }
            break;
        }
        case FieldDescriptor::TYPE_INT32:
        case FieldDescriptor::TYPE_SFIXED32:
        case FieldDescriptor::TYPE_SINT32:
        {
            if (field->has_default_value()) {
                default_value = IntToStr(field->default_value_int32());
            } else {
                default_value = "0";
            }
            break;
        }
        case FieldDescriptor::TYPE_BOOL: {
            if (field->has_default_value()) {
                default_value = field->default_value_bool() ? "yes" : "no";
            } else {
                default_value = "no";
            }
            break;
        }
        case FieldDescriptor::TYPE_STRING: {
            if (field->has_default_value()) {
                default_value =
                    "\"" + CEscape(field->default_value_string()) + "\"";
            } else {
                default_value = "\"\"";
            }
            break;
        }
        case FieldDescriptor::TYPE_MESSAGE: {
            default_value = "protobuf_runtime.default_value:" +
                MessageTypeName(field->message_type());
            break;
        }
        case FieldDescriptor::TYPE_ENUM: {
            if (field->has_default_value()) {
                default_value = EnumValueName(field->default_value_enum());
            } else {
                default_value = EnumValueName(field->enum_type()->value(0));
            }
            break;
        }
        case FieldDescriptor::TYPE_BYTES: {
            if (field->has_default_value()) {
                default_value =
                    "protobuf_runtime.string_to_bitmap(\"" +
                        CEscape(field->default_value_string()) + "\")";
            } else {
                default_value = "bitmap.new(0)";
            }
            break;
        }
        default: {
            throw field;
        }
    }

    return default_value;
}

// Write the pb_message instance definition for a message type.
void WriteMessageTypeInstance(io::Printer *printer,
    const Descriptor *message_type)
{
    int                     i;
    int                     field_count = message_type->field_count();
    string                  type_name = MessageTypeName(message_type);
    map<string, string>     vars;

    printer->Print(
        ":- instance protobuf_runtime.pb_message($type_name$) where [\n",
            "type_name", type_name);

    if (field_count == 0) {
        printer->Print("    field_info(_, 0, 0, pb_int32, required) :- semidet_fail,\n");
        printer->Print("    default_value = $type_name$\n",
            "type_name", type_name);
    } else {
        for (i = 0; i < field_count; i++) {
            vars["field_id"] = IntToStr(message_type->field(i)->number());
            vars["arg_num"] = IntToStr(i);
            vars["type"] =
                FieldToPBRuntimeTypeStr(message_type->field(i));
            vars["card"] =
                FieldLabelToPBRuntimeCardStr(message_type->field(i)->label());
            printer->Print(vars,
                "    field_info(_, $field_id$, $arg_num$, $type$, $card$),\n");
        }
        printer->Print("\n    default_value = $type_name$(",
            "type_name", type_name);
        
        for (i = 0; i < field_count; i++) {
            printer->Print(FieldDefaultValueStr(message_type->field(i)).c_str());
            if (i < field_count - 1) {
                printer->Print(", ");
            }
        }

        printer->Print(")\n");
    }

    printer->Print("].\n\n");
}

// Write the pb_enumeration instance definition for an enumeration.
void WriteEnumTypeInstance(io::Printer *printer,
    const EnumDescriptor *enumeration)
{
    int                     i;
    int                     value_count = enumeration->value_count();
    string                  type_name = EnumTypeName(enumeration);

    printer->Print(
        ":- instance protobuf_runtime.pb_enumeration($type_name$) where [\n",
            "type_name", type_name);

    for (i = 0; i < value_count; i++) {
        printer->Print("    enum_int($enum$, $int$)",
            "enum", EnumValueName(enumeration->value(i)),
            "int", IntToStr(enumeration->value(i)->number()));
        if (i < value_count - 1) {
            printer->Print(",");
        }
        printer->Print("\n");
    }

    printer->Print("].\n\n");
}

// Write the implementation section.
void WriteImplementation(io::Printer *printer, const FileDescriptor* file,
    vector<const Descriptor*> *message_types,
    vector<const EnumDescriptor*> *enums)
{
    int     i;

    printer->Print(":- implementation.\n\n");

    for (i = 0; i < message_types->size(); i++) {
        WriteMessageTypeInstance(printer, message_types->at(i));
    }

    for (i = 0; i < enums->size(); i++) {
        WriteEnumTypeInstance(printer, enums->at(i));
    }
}

MercuryGenerator::MercuryGenerator() {
}

MercuryGenerator::~MercuryGenerator() {
}

bool MercuryGenerator::Generate(const FileDescriptor* file,
                         const string& parameter,
                         OutputDirectory* output_directory,
                         string* error) const
{
    string module = ModuleName(file);
    string filename = module + ".m";

    scoped_ptr<io::ZeroCopyOutputStream>
        output(output_directory->Open(filename));
    io::Printer printer(output.get(), '$');

    // Initialise error in case we forget to set it when we encounter
    // an error.
    *error = "";

    vector<const Descriptor*> message_types;
    vector<const EnumDescriptor*> enums;

    FlattenMessageTypes(file, &message_types);
    FlattenEnums(file, &enums);

    printer.Print("% Generated by protoc.  DO NOT EDIT.\n");
    printer.Print(":- module $module$.\n\n", "module", module);

    try {
        WriteInterface(&printer, file, &message_types, &enums);
        WriteImplementation(&printer, file, &message_types, &enums);
    }
    catch (FieldDescriptor const *field) {
        *error = "\nError: unsupported type for field: \n\t" +
            field->DebugString();
        return false;
    }

    printer.Print(":- end_module $module$.\n", "module", module);

    return true;
}

}  // namespace mercury
}  // namespace compiler
}  // namespace protobuf
}  // namespace google
