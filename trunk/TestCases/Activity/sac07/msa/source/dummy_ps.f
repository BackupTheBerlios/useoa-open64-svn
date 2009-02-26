C234567890         0         0         0         0         0         012
C                                                                      C
C     Dummy function driver code for the Partially-Separated           C
C     formulation of MINPACK-2's Minimal Surface Area (MSA) problem.   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PROGRAM ps_msa_dummy_driver

      INTEGER maxnx
      PARAMETER(maxnx=300)
      INTEGER maxny
      PARAMETER(maxny=300)
      INTEGER nmax
      PARAMETER(nmax=maxnx*maxny)
      INTEGER mmax
      PARAMETER(mmax=2*nmax+2*maxnx+2*maxny+2)
      INTEGER nx
      INTEGER ny
      INTEGER m
      DOUBLE PRECISION bottom(maxnx+2)
      DOUBLE PRECISION top(maxnx+2)
      DOUBLE PRECISION left(maxny+2)
      DOUBLE PRECISION right(maxny+2)
      DOUBLE PRECISION x(nmax)
      DOUBLE PRECISION fvec(mmax)

      CALL dmsasep(nx,ny,x,fvec,bottom,top,left,right,m)

      STOP
      END

