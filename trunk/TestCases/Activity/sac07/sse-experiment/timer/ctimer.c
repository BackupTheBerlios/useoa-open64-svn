#include "ptime.h"

#ifdef __stdc__
#define _P(x) x
#else
#define _P(x) ()
#endif

#ifdef SunOS4
#define ctimer ctimer_
#elif AIX
#define ctimer ctimer
#elif SunOS5
#define ctimer ctimer_
#elif IRIX
#define ctimer ctimer_
#elif Linux86
#define ctimer ctimer_
#endif

void ctimer _P((double *vptr));

void ctimer(vptr)
double *vptr;
{
  PetscTime(*vptr);
}

