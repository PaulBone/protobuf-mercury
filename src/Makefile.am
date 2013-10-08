bin_PROGRAMS = mprotoc
mprotoc_SOURCES =							\
	google/protobuf/compiler/mercury/mercury_generator.cc		\
	google/protobuf/compiler/mercury/mercury_strutil.cc		\
	google/protobuf/compiler/mercury/main.cc

mprotoc_CXXFLAGS = $(protobuf_CFLAGS)
mprotoc_LDFLAGS = $(protobuf_LIBS)

noinst_HEADERS = 							\
	google/protobuf/compiler/mercury/mercury_generator.h		\
	google/protobuf/compiler/mercury/mercury_strutil.h
