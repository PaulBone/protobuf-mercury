AC_INIT([protobuf-mercury], [DEV], [iml@missioncriticalit.com])
AM_INIT_AUTOMAKE([-Wall -Werror foreign subdir-objects])
AC_PROG_CXX
AC_PROG_CC

AC_PATH_PROG(PROTOC,protoc)
if test "$PROTOC" = "" ;
then
	AC_MSG_ERROR(protoc not found)
fi
AC_SUBST(PROTOC)

AC_PATH_PROG(MMC,mmc)
if test "$MMC" = "" ;
then
	AC_MSG_WARN(mmc not found. mmc is required to run the tests)
fi
AC_SUBST(MMC)

PKG_CHECK_MODULES([protobuf], [protobuf >= 2.4], , [
    AC_MSG_ERROR([Could not find protocol buffers library])])

AC_LANG(C++)
AC_CHECK_LIB([protoc], [main], , [
    AC_MSG_ERROR([Could not find protocol buffers compiler library])]) 

AC_CONFIG_FILES([
    Makefile
    src/Makefile
    Make.common
])
AC_OUTPUT
