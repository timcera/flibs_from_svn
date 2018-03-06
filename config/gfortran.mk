# Options for gfortran
#
# Maybe define LDOUTPUT as LDOUTPUT=-o $@
#
SPACE	=	\

SEP	=	/

FC	=	gfortran
FFLAGS_NORMAL	=	-c
FFLAGS_DEBUG	=	-c -g
FFLAGS_OPTIMISE	=	-c -O

CC	=	gcc
CFLAGS	=	-c

LD	=	gfortran
LDFLAGS_NORMAL	=	
LDFLAGS_DEBUG	=	-g
LDFLAGS_OPTIMISE	=	
#LDOUTPUT	=	-o$(SPACE)
LDOUTPUT	=	-o $@

LIB	=	ar r
LIBOUT	=

OBJEXT	=	.o
EXEEXT	=	
MODEXT	=	.mod
LIBEXT	=	.a

DELETE	=	rm -f
