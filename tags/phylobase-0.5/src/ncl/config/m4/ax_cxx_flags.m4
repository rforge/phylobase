dnl @synopsis AX_CXXFLAGS()
dnl @author Mark Holder based on examples by Dave Swofford
dnl @version 2006-01-02
dnl @license AllPermissive
AC_DEFUN([AX_CXXFLAGS],
[
	
	dnl Check for debugging mode.
	AC_ARG_ENABLE(
		debugging,
		AC_HELP_STRING(
			[--enable-debugging],
			[build for debugging]
			), 
		, 
		[enable_debugging=no]
		)
	if test "$enable_debugging" = yes
	then
		AC_MSG_NOTICE([*** NOTE: debugging is enabled; optimization is suppressed!])
	fi

	
	if test "$enable_debugging" = yes; then
		CXXFLAGS_OPTIM_SPEED="-O0"
		CXXFLAGS="$CXXFLAGS -g"
	else
		CXXFLAGS_OPTIM_SPEED="-O"
	fi

	if test "$CC" = "icc"; then
			#	Intel C compiler for Linux
		if test "$enable_debugging" = no; then
			CXXFLAGS_OPTIM_SPEED="-O3"
			CXXFLAGS_OPTIM_SIZE="-O2"
		fi
	elif test "$CC" = "ccc"; then
			#	Compaq C compiler for Linux
		if test "x$arch" = "x"; then
			arch="host"
		fi
		if test "$enable_debugging" = no; then
			CXXFLAGS_OPTIM_SPEED="-fast -inline speed -arch $arch"
			CXXFLAGS_OPTIM_SIZE="-fast -inline size -unroll 1 -arch $arch"
		fi
	elif test "$CC" = "xlc"; then
			#	IBM XL C compiler
		if test "x$arch" = "x"; then
			arch="auto"
		fi
		if test "$enable_debugging" = no; then
			CXXFLAGS_OPTIM_SPEED="-O3 -qarch=$arch"
			CXXFLAGS_OPTIM_SIZE="-O3 -qarch=$arch"
		fi
	elif test "$GCC" = "yes" ; then
		if test "$enable_debugging" = yes; then
				#	Suppress warnings about possibly uninitialized variables but show everything else (used for
				#   development, but these warnings should also not trip for release builds)
			CXXFLAGS_WARNINGS="$CXXFLAGS_WARNINGS -Wall -Wimplicit -Wreturn-type -Wunused -Wredundant-decls -Wcast-align -Wcomment -Wextra"
		else
				#	Just suppress warnings about possibly uninitialized variables	
			CXXFLAGS_WARNINGS="$CXXFLAGS_WARNINGS"
			CXXFLAGS_OPTIM_SPEED="-O3 -ffast-math"
			CXXFLAGS_OPTIM_SIZE="-Os -ffast-math"
		fi
		case "$build_os" in
			darwin*) ## no-op; hack for GCC 4.2.1 compat ## CXXFLAGS_WARNINGS="$CXXFLAGS_WARNINGS" -Wno-long-double";;
			*);;
		esac	
	fi
	if test "x$CXXFLAGS_OPTIM_SIZE" = "x"; then
		CXXFLAGS_OPTIM_SIZE=$CXXFLAGS_OPTIM_SPEED
	fi
	
	CXXFLAGS="$CXXFLAGS $CXXFLAGS_OPTIM_SPEED $CXXFLAGS_WARNINGS"

])
