#include <google/protobuf/compiler/command_line_interface.h>
#include <google/protobuf/compiler/mercury/mercury_generator.h>


int main(int argc, char* argv[]) {
  google::protobuf::compiler::CommandLineInterface cli;

  // Support generation of Mercury code.
  google::protobuf::compiler::c::MercuryGenerator mercury_generator;
  cli.RegisterGenerator("--out", &mercury_generator,
  "Generate Mercury files.");
  
  return cli.Run(argc, argv);
}
