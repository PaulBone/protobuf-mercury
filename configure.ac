AC_INIT([protobuf-mercury], [0.2], [iml@missioncriticalit.com])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
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

AC_CONFIG_FILES([
    Makefile
    src/Makefile
    Make.common
])
AC_OUTPUT
