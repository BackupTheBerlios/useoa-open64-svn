#	This Makefile creates a library containing the Fortran subroutine
#	timer(stime), where stime is a double precision variable containing
#	the time of day as returned by the system clock.  It works for the
#	following architectures: sun4, rs6000, iris4d and cray.  Users of
#	other systems should use their own timing routines.
#
#	Note: cray users may need to modify '$(FC)' below to 'cc'.


# FFLAGS      = -O -o32
# CFLAGS      = -D$(AD_OS) -O -o32
# CFLAGS      = -DSunOS5 -O
CFLAGS      = -D$(AD_OS) -O

#       The Unix command ranlib converts archives to random libraries.
#       However, ranlib is not installed on all systems.  Check to see if
#       your system has ranlib, if not declare it below as /bin/true.

#RANLIB       = /bin/true
RANLIB       = ranlib


TIMER_FILES = ctimer.o timer.o

timer: $(TIMER_FILES)
	ar ruv libTimer-$(AD_OS).a $(TIMER_FILES)
	$(RANLIB) libTimer-$(AD_OS).a

test: test_timer.o
	make timer
	$(FC) test_timer.o libTimer-$(AD_OS).a -o test_timer
	./test_timer

clean:
	- /bin/rm -f *.o test_timer *~
