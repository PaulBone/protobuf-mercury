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
// Generates Mercury code for a given .proto file.

#ifndef GOOGLE_PROTOBUF_COMPILER_MERCURY_GENERATOR_H__
#define GOOGLE_PROTOBUF_COMPILER_MERCURY_GENERATOR_H__

#include <string>

#include <google/protobuf/compiler/code_generator.h>
#include <google/protobuf/stubs/common.h>

namespace google {
namespace protobuf {

class Descriptor;
class EnumDescriptor;
class EnumValueDescriptor;
class FieldDescriptor;
class ServiceDescriptor;

namespace io { class Printer; }

namespace compiler {
namespace mercury {

class LIBPROTOC_EXPORT MercuryGenerator : public CodeGenerator {
 public:
  MercuryGenerator();
  virtual ~MercuryGenerator();

  // CodeGenerator methods.
  virtual bool Generate(const FileDescriptor* file,
                        const string& parameter,
                        OutputDirectory* output_directory,
                        string* error) const;

 private:
};

}  // namespace python
}  // namespace compiler
}  // namespace protobuf

}  // namespace google
#endif  // GOOGLE_PROTOBUF_COMPILER_MERCURY_GENERATOR_H__
