include ../Make.common

TESTS =	test_int32				\
	test_sint32				\
	test_sfixed32				\
	test_double				\
	test_string				\
	test_enum				\
	test_bytes				\
	field_key				\
	defaults				\
	embedded

check: $(TESTS:%=%_cppreader) $(TESTS:%=%_mwriter) $(TESTS:%=%_mreader) $(TESTS:%=%_pprinter) test_defaults
	./runtests $(TESTS)

$(TESTS:%=%_cppreader): %_cppreader: %_cppreader.o %.pb.o
	$(CXX) -o $@ $^ -lprotobuf -lpthread

$(TESTS:%=%_cppreader.o): %_cppreader.o: %_cppreader.cc %.pb.cc
	$(CXX) -c $^

%_cppreader.cc: %.class cppreader_template 
	@cat cppreader_template | sed 's/TEST/$(@:%_cppreader.cc=%)/g' | \
		sed 's/MESSAGE_CLASS/$(shell cat $<)/g' > $@

%_mwriter.m: %.inp mwriter_template 
	@cat mwriter_template | sed 's/TEST/$(@:%_mwriter.m=%)/g' | \
		sed 's/MESSAGE_TERM/$(shell cat $<)/g' > $@

%_mreader.m: %.type mreader_template 
	@cat mreader_template | sed 's/TEST/$(@:%_mreader.m=%)/g' | \
		sed 's/MESSAGE_TYPE/$(shell cat $<)/g' > $@

%_pprinter.m: %.type pprinter_template 
	@cat pprinter_template | sed 's/TEST/$(@:%_pprinter.m=%)/g' | \
		sed 's/MESSAGE_TYPE/$(shell cat $<)/g' > $@

%_mwriter: %_mwriter.m %.m Mercury.modules ../mercury/protobuf_runtime.m
	$(MMC) --make $@

%_mreader: %_mreader.m %.m Mercury.modules ../mercury/protobuf_runtime.m
	$(MMC) --make $@

%_pprinter: %_pprinter.m %.m Mercury.modules ../mercury/protobuf_runtime.m
	$(MMC) --make $@

$(TESTS:%=%.pb.cc): %.pb.cc: %.proto
	$(PROTOC) --cpp_out . $<

$(TESTS:%=%.m): %.m: %.proto ../src/mprotoc
	../src/mprotoc --out . $<

Mercury.modules:
	$(MMC) -f ../mercury/protobuf_runtime.m

test_defaults: read_defaults defaults_mwriter
	./defaults_mwriter defaults.out3
	./read_defaults defaults.out3 > defaults.mout3
	cmp -s defaults.mout3 defaults.defexp

read_defaults: defaults.m read_defaults.m ../mercury/protobuf_runtime.m Mercury.modules
	$(MMC) --make $@

distdir:
	cp Makefile *_template *.proto *.type *.class *.inp *.mexp \
		*.cppexp runtest runtests Mercury.options read_defaults.m \
		*.defexp $(distdir)

#############################################################################
# Other targets required by automake:

all:
install:
install-data:
install-exec:
uninstall:
install-dvi:
install-html:
install-info:
install-ps:
install-pdf:
installdirs:
installcheck:
mostlyclean:
clean:
distclean:
maintainer-clean:
dvi:
pdf:
ps:
info:
html:
tags:
ctags:
