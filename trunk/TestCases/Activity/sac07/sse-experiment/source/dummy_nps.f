C234567890         0         0         0         0         0         012
C                                                                      C
C     Dummy function driver code for the original (Not Partially-      C
C     Separated) MINPACK-2 Minimal Surface Area (MSA) problem.         C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PROGRAM nps_msa_dummy_driver

      INTEGER maxnx
      PARAMETER(maxnx=300)
      INTEGER maxny
      PARAMETER(maxny=300)
      INTEGER nmax
      PARAMETER(nmax=maxnx*maxny)
      INTEGER nx
      INTEGER ny
      real x(nmax)
      real f
      real w(nmax)
      real fgrad(nmax)
      CHARACTER*6 task

      CALL dmsamain(nx,ny,x,f,fgrad,task,w)

      STOP
      END



