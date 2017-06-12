# USAGE: make arch=system where system is one of 
#          (osx, linux, solaris, sun, alpha, dec)
#

SITEOBJ=	tvguide.o

OBJ=	tvguide.o \
	cartesian.o \
	dcd_arg.o \
	dcd_date.o \
	eclip.o \
	getlun.o \
	julian.o \
	lentrim.o \
	nutate.o \
	prec.o \
	rd_astro.o \
	rd_real.o \
	upcase.o \
	wt_astro.o \
	sys.o

CC	= gcc

all : linklib
	make tvguide

linklib :
	@if [ '$(arch)' = 'sun' ]; \
	then \
		rm -f sys.f; ln sun.sys sys.f; \
	elif [ '$(arch)' = 'solaris' ]; \
	then \
		rm -f sys.f; ln solaris.sys sys.f; \
	elif [ '$(arch)' = 'dec' ]; \
	then \
		rm -f sys.f; ln ultrix.sys sys.f; \
	elif [ '$(arch)' = 'alpha' ]; \
	then \
		rm -f sys.f; ln osf.sys sys.f; \
	elif [ '$(arch)' = 'linux' ]; \
	then \
		rm -f sys.f; ln linux.sys sys.f; \
	elif [ '$(arch)' = 'osx' ]; \
	then \
		rm -f sys.f; ln macosx.sys sys.f; \
	else \
		echo "Architecture not supported: arch=$(arch)";  exit 1 ; \
	fi

tvguide :	$(OBJ)
	gfortran -o tvguide $(OBJ)

clean :
	rm -f *.o sys.f ark_date.f ark_date.c

$(SITEOBJ):	./tvguide.inc
