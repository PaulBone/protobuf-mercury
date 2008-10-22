include ../Make.common

writeread_person: writeread_person.m Mercury.modules ../mercury/protobuf_runtime.m person.m
	$(MMC) --make $@

person.m: person.proto ../src/mprotoc
	../src/mprotoc person.proto --out .

Mercury.modules:
	mmc -f ../mercury/protobuf_runtime.m

distdir:
	cp person.proto writeread_person.m Makefile README $(distdir)

#############################################################################
# Other targets required by automake:

all:
check:
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
