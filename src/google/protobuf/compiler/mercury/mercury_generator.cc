// Protocol Buffers for Mercury
// Copyright 2008 Mission Critical Australia.
// http://code.google.com/p/protobuf-mercury/
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Mission Critical Australia nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

namespace google {
namespace protobuf {
namespace compiler {
namespace mercury {

//----------------------------------------------------------------------------

// The following functions return a vector containing all the
// message types or enums in a proto file.  We generate a separate
// type for each message type and enum, since Mercury doesn't have the
// concept of nested types (you can't define a new type inside the
// definition of another type).

void FlattenNestedMessageTypes(const Descriptor *message,
    vector<const Descriptor*> *message_types);

void FlattenMessageTypes(const FileDescriptor* file,
    vector<const Descriptor*> *message_types);

void FlattenNestedEnums(const Descriptor *message,
    vector<const EnumDescriptor*> *enums);

void FlattenEnums(const FileDescriptor* file,
    vector<const EnumDescriptor*> *enums);

// Return the Mercury type name for a message type.
string MessageTypeName(const Descriptor *message_type);

// Return the Mercury type name for an enumeration.
string EnumTypeName(const EnumDescriptor *enumeration);

// Return the Mercury field name for a message type field.
string FieldName(const FieldDescriptor *field);

// Return the Mercury type for a field as a string.
// If the wrap argument is true, then the type will be wrapped in
// maybe or list if applicable.
string FieldTypeName(const FieldDescriptor *field, bool wrap);

// Return the default value of a field as a Mercury term.
string FieldDefaultValueStr(const FieldDescriptor *field);

// Return the functor name for an enumeration value.
string EnumValueName(const EnumValueDescriptor *value);

// Return the protobuf_runtime.field_type value for the
// given field.
string FieldToPBRuntimeTypeStr(const FieldDescriptor *field);

// Return the protobuf_runtime.field_cardinality value for the
// given field.
string FieldLabelToPBRuntimeCardStr(FieldDescriptor::Label label);

// Return the initial value for a field as a Mercury term.
string FieldInitValueStr(const FieldDescriptor *field);

// Write one field of a message type.
void WriteMessageField(io::Printer *printer, const FieldDescriptor *field);

// Write the Mercury type for a proto message type.
// Also writes the pb_message instance declaration.
void WriteMessageType(io::Printer *printer, const Descriptor *message_type);

// Write declarations for functions to return the value of optional fields, or
// the default value if the field is not set.
void WriteOptionalFieldAccessorDecls(io::Printer *printer,
    const Descriptor *message_type);

// Write definitions for functions to return the value of optional fields, or
// the default value if the field is not set.
void WriteOptionalFieldAccessorDefs(io::Printer *printer,
    const Descriptor *message_type);

// Write the Mercury type for a proto enumeration.
// Also writes the pb_enumeration instance declaration.
void WriteEnumType(io::Printer *printer, const EnumDescriptor *enumeration);

// Write the interface section.
void WriteInterface(io::Printer *printer, const FileDescriptor* file,
    vector<const Descriptor*> *message_types,
    vector<const EnumDescriptor*> *enums);

// Write the pb_message instance definition for a message type.
void WriteMessageTypeInstance(io::Printer *printer,
    const Descriptor *message_type);

// Write the pb_enumeration instance definition for an enumeration.
void WriteEnumTypeInstance(io::Printer *printer,
    const EnumDescriptor *enumeration);

// Write the implementation section.
void WriteImplementation(io::Printer *printer, const FileDescriptor* file,
    vector<const Descriptor*> *message_types,
    vector<const EnumDescriptor*> *enums);

//----------------------------------------------------------------------------
// Misc stuff.
//

// Strip the .proto suffix.  Copied from python_generator.cc.
string StripProto(const string& filename);

// Returns the module name.
// TODO: Allow module name to be given as an option.
string ModuleName(const FileDescriptor* file);

// Convert a double to a string.
string DoubleToStr(double d);

// Convert an integer into a string.
string IntToStr(int i);

// Converts camel case or uppercase string to a lowercase string with
// underscores.
string StringToLowerCaseWithUnderScores(const string str);

//----------------------------------------------------------------------------

string StripProto(const string& filename) {
  const char* suffix = HasSuffixString(filename, ".protodevel")
      ? ".protodevel" : ".proto";
  return StripSuffixString(filename, suffix);
}

string ModuleName(const FileDescriptor* file)
{
    return StripProto(file->name());
}

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

string FieldName(const FieldDescriptor *field)
{
    return MessageTypeName(field->containing_type()) + "_" +
        StringToLowerCaseWithUnderScores(field->name());
}

string FieldTypeName(const FieldDescriptor *field, bool wrap)
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
    
    if (wrap) {
        if (field->is_optional()) {
            type_name = "maybe(" + type_name + ")";
        } else if (field->is_repeated()) {
            type_name = "list(" + type_name + ")";
        }
    }

    return type_name;
}

void WriteMessageField(io::Printer *printer, const FieldDescriptor *field)
{
    printer->Print("$field_name$ :: $field_type_string$",
        "field_name", FieldName(field),
        "field_type_string", FieldTypeName(field, true));
}

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

void WriteOptionalFieldAccessorDecls(io::Printer *printer,
    const Descriptor *message_type)
{
    int                     i;
    int                     field_count = message_type->field_count();
    string                  type_name = MessageTypeName(message_type);
    const FieldDescriptor   *field;
    map<string, string>     vars;

    for (i = 0; i < field_count; i++) {
        field = message_type->field(i);
        if (field->is_optional() &&
                field->type() != FieldDescriptor::TYPE_MESSAGE)
        {
            vars["type_name"] = type_name;
            vars["field_name"] = FieldName(field);
            vars["field_type"] = FieldTypeName(field, false);
            printer->Print(vars,
                ":- func $field_name$_or_default($type_name$) = "
                "$field_type$.\n\n");
        }
    }
}

string FieldDefaultValueStr(const FieldDescriptor *field)
{
    switch(field->type()) {
        case FieldDescriptor::TYPE_DOUBLE: {
            if (field->has_default_value()) {
                return DoubleToStr(field->default_value_double());
            } else {
                return "0.0";
            }
            break;
        }
        case FieldDescriptor::TYPE_INT32:
        case FieldDescriptor::TYPE_FIXED32:
        case FieldDescriptor::TYPE_SFIXED32:
        case FieldDescriptor::TYPE_SINT32:
        {
            if (field->has_default_value()) {
                return IntToStr(field->default_value_int32());
            } else {
                return "0";
            }
            break;
        }
        case FieldDescriptor::TYPE_BOOL: {
            if (field->has_default_value()) {
                return field->default_value_bool() ? "yes" : "no";
            } else {
                return "no";
            }
            break;
        }
        case FieldDescriptor::TYPE_STRING: {
            if (field->has_default_value()) {
                return "\"" + CEscape(field->default_value_string()) + "\"";
            } else {
                return "\"\"";
            }
            break;
        }
        case FieldDescriptor::TYPE_MESSAGE: {
            return "protobuf_runtime.init_message:" +
                MessageTypeName(field->message_type());
        }
        case FieldDescriptor::TYPE_ENUM: {
            if (field->has_default_value()) {
                return EnumValueName(field->default_value_enum());
            } else {
                return EnumValueName(field->enum_type()->value(0));
            }
        }
        case FieldDescriptor::TYPE_BYTES: {
            if (field->has_default_value()) {
                return "protobuf_runtime.string_to_bitmap(\"" +
                        CEscape(field->default_value_string()) + "\")";
            } else {
                return "bitmap.new(0)";
            }
            break;
        }
        default: {
            throw field;
        }
    }
}

void WriteOptionalFieldAccessorDefs(io::Printer *printer,
    const Descriptor *message_type)
{
    int                     i;
    int                     field_count = message_type->field_count();
    string                  type_name = MessageTypeName(message_type);
    const FieldDescriptor   *field;
    map<string, string>     vars;

    for (i = 0; i < field_count; i++) {
        field = message_type->field(i);
        if (field->is_optional() &&
                field->type() != FieldDescriptor::TYPE_MESSAGE)
        {
            vars["field_name"] = FieldName(field);
            vars["default_value"] = FieldDefaultValueStr(field);
            printer->Print(vars,
                "$field_name$_or_default(Message) = Value :-\n"
                "    ( Message ^ $field_name$ = yes(Value)\n"
                "    ; Message ^ $field_name$ = no,\n"
                "        Value = $default_value$\n"
                "    ).\n\n");
        }
    }
}

string EnumValueName(const EnumValueDescriptor *value)
{
    return EnumTypeName(value->type()) + "_" +
        StringToLowerCaseWithUnderScores(value->name());
}

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

void WriteInterface(io::Printer *printer, const FileDescriptor* file,
    vector<const Descriptor*> *message_types,
    vector<const EnumDescriptor*> *enums)
{
    int     i;
    string  module = ModuleName(file);

    printer->Print(":- interface.\n\n");
    printer->Print(
        ":- import_module protobuf_runtime, bitmap, bool, list, maybe.\n\n");

    for (i = 0; i < message_types->size(); i++) {
        WriteMessageType(printer, message_types->at(i));
        WriteOptionalFieldAccessorDecls(printer, message_types->at(i));
    }

    for (i = 0; i < enums->size(); i++) {
        WriteEnumType(printer, enums->at(i));
    }
}

string IntToStr(int i)
{
    std::stringstream    ss;
    ss << i;
    return ss.str();
}

string DoubleToStr(double d)
{
    std::stringstream    ss;
    ss.setf(ios::scientific);
    ss.precision(100);
    ss << d;
    return ss.str();
}

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
                "protobuf_runtime.init_message:" +
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

string FieldInitValueStr(const FieldDescriptor *field)
{
    if (field->is_optional()) {
        return "no";
    }

    if (field->is_repeated()) {
        return "[]";
    }

    switch(field->type()) {
        case FieldDescriptor::TYPE_DOUBLE: {
            return "0.0";
        }
        case FieldDescriptor::TYPE_INT32:
        case FieldDescriptor::TYPE_SFIXED32:
        case FieldDescriptor::TYPE_SINT32:
        {
            return "0";
        }
        case FieldDescriptor::TYPE_BOOL: {
            return "no";
        }
        case FieldDescriptor::TYPE_STRING: {
            return "\"\"";
        }
        case FieldDescriptor::TYPE_MESSAGE: {
            return "protobuf_runtime.init_message:" +
                MessageTypeName(field->message_type());
        }
        case FieldDescriptor::TYPE_ENUM: {
            return EnumValueName(field->enum_type()->value(0));
        }
        case FieldDescriptor::TYPE_BYTES: {
            return "bitmap.new(0)";
        }
        default: {
            throw field;
        }
    }
}

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
        printer->Print("    init_message = $type_name$\n",
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
        printer->Print("\n    init_message = $type_name$(",
            "type_name", type_name);
        
        for (i = 0; i < field_count; i++) {
            printer->Print(FieldInitValueStr(message_type->field(i)).c_str());
            if (i < field_count - 1) {
                printer->Print(", ");
            }
        }

        printer->Print(")\n");
    }

    printer->Print("].\n\n");
}

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

void WriteImplementation(io::Printer *printer, const FileDescriptor* file,
    vector<const Descriptor*> *message_types,
    vector<const EnumDescriptor*> *enums)
{
    int     i;

    printer->Print(":- implementation.\n\n");

    for (i = 0; i < message_types->size(); i++) {
        WriteMessageTypeInstance(printer, message_types->at(i));
        WriteOptionalFieldAccessorDefs(printer, message_types->at(i));
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

    printer.Print("% Generated by mprotoc.  DO NOT EDIT.\n");
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
