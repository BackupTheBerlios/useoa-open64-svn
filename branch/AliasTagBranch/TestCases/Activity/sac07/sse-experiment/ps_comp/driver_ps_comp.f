C234567890         0         0         0         0         0         012
C                                                                      C
C     Nonsparse derivative driver code for the nonsparse version of    C
C     the adifor-generated code for computing gradient of the          C
C     Partially-Separated formulation of MINPACK-2's Minimal           C
C     Surface Area (MSA) problem.                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PROGRAM ps_nonsparse_msa_driver


C     General MINPACK-2 Parameters & Variables
C     ****************************************

C     Max # of grid points in the 1st coordinate dimension; i.e.,
C     Max value of nx.
      INTEGER maxnx
      PARAMETER(maxnx=300)

C     Max # of grid points in the 2nd coordinate dimension; i.e.,
C     Max value of ny.
      INTEGER maxny
      PARAMETER(maxny=300)

C     Max gradient size; i.e., Max value of n.
      INTEGER nmax
      PARAMETER(nmax=maxnx*maxny)

C     Max # of element functions; i.e., Max value of m.
      INTEGER mmax
      PARAMETER(mmax=2*nmax+2*maxnx+2*maxny+2)

C     Runtime # of grid points in the 1st coordinate dimension.
      INTEGER nx

C     Runtime # of grid points in the 2nd coordinate dimension.
      INTEGER ny

C     Runtime gradient size; i.e., # of independent variables.
      INTEGER n

C     Runtime # of element functions; i.e., # of dependent variables.
      INTEGER m

C     MINPACK-2 action specifier variable.
      CHARACTER*6 task


C     Parameters and Variables for MINPACK-2's DSM Coloring Algorithm
C     ***************************************************************

C     Max # of nonzeroes in the final Jacobian; i.e.,
C     Max value of nnz
      INTEGER nnzmax
      PARAMETER(nnzmax=6*nmax)

C     DSM work array size.
      INTEGER liwa
      PARAMETER(liwa=6*nmax)

C     Needed as array dimension.
      INTEGER nmaxplus1
      PARAMETER(nmaxplus1=nmax+1)

C     Needed as array dimension.
      INTEGER mmaxplus1
      PARAMETER(mmaxplus1=mmax+1)

C     DSM input variable: runtime # of nonzeroes in the final Jacobian.
      INTEGER nnz

C     DSM input variable: the row indices of the nonzero elements of
C     the Jacobian.  The column indices can be recovered from the jpntr.
      INTEGER indrow(nnzmax)

C     DSM input variable: the column indices of the nonzero elements of
C     the Jacobian.  The row indices can be recovered from the ipntr.
      INTEGER indcol(nnzmax)

C     DSM output variable: # of groups in the partition of the
C     columns of the Jacobian; i.e., the chromatic number.
      INTEGER maxgrp

C     DSM output variable: lowerbound for the # of groups in any
C     consistent partition of the columns of the Jacobian.
      INTEGER mingrp

C     DSM output variable: specifies the partition of the columns of the
C     Jacobian. Column j of the original Jacobian maps to column ngrp(j)
C     of the compessed Jacobian.
      INTEGER ngrp(nmax)

C     DSM output variable: indices of ipntr are row indices of the
C     nonzeroes in the Jacobian.  ipntr(i) contains the first location
C     in indcol where the column indices of the nonzeroes in row i may
C     be found.  The entire set of column indices for row i are:
C            indcol(loc), where loc = ipntr(i),...,ipntr(i+1)-1.
C     A nonzero exists at each such [i,indcol(loc)] Jacobian entry.
      INTEGER ipntr(mmaxplus1)

C     DSM output variable: indices of jpntr are column indices of the
C     nonzeroes in the Jacobian.  jpntr(j) contains the first location
C     in indrow where the row indices of the nonzeroes in column j may
C     be found.  The entire set of row indices for column j are:
C            indrow(loc), where loc = jpntr(j),...,jpntr(j+1)-1.
C     A nonzero exists at each such [indrow(loc),j] Jacobian entry.
      INTEGER jpntr(nmaxplus1)

C     DSM work array.
      INTEGER iwa(liwa)

C     DSM termination flag: for normal termination, info = 1.
      INTEGER info


C     DMSA-Specific Variables
C     ***********************

C     Boundary values for the bottom boundary of the domain.
      DOUBLE PRECISION bottom(maxnx+2)

C     Boundary values for the top boundary of the domain.
      DOUBLE PRECISION top(maxnx+2)

C     Boundary values for the left boundary of the domain.
      DOUBLE PRECISION left(maxny+2)

C     Boundary values for the right boundary of the domain.
      DOUBLE PRECISION right(maxny+2)


C     Parameters and Variables Related to Derivative Computation
C     **********************************************************

C     Upper bound on the # of directions with respect to which
C     derivatives are computed.  Set to equal the chromatic number.
C     Used as first dimension of each derivative object.
      INTEGER pmax
      PARAMETER (pmax=3)

C     The independent variable.
      DOUBLE PRECISION x(nmax)

C     The scalar aggregate function computed in MINPACK-2.
      DOUBLE PRECISION f

C     The dependent variable; i.e.,
C     the partially-separated reformulation of f.
      DOUBLE PRECISION fvec(mmax)

C     MINPACK-2 work array.
      DOUBLE PRECISION w(nmax)

C     The seed matrix.
      DOUBLE PRECISION g_x(pmax,nmax)

C     The compressed Jacobian, transposed.
      DOUBLE PRECISION g_fvec(pmax,mmax)

C     MINPACK-2's analytically-computed gradient.
      DOUBLE PRECISION hc_g(nmax)

C     Gradient computed by ADIFOR.
      DOUBLE PRECISION ad_g(nmax)


C     Iteration Index Variables
C     *************************

C     Index used for averaging function time.
      INTEGER h

C     Index ranging over the m dependent variables, i.e.,
C     Jacobian row index.
      INTEGER i

C     Index ranging over the n independent variables, i.e.,
C     Jacobian column index.
      INTEGER j

C     Gradient computations counter.
      INTEGER k

C     Index ranging over the locations in indcol or indrow.
      INTEGER loc


C     Timing Variables
C     ****************

      DOUBLE PRECISION start_time
      DOUBLE PRECISION stop_time
      DOUBLE PRECISION F_compute_time
      DOUBLE PRECISION hc_G_compute_time
      DOUBLE PRECISION ad_J_compute_time
      DOUBLE PRECISION ad_G_compute_time
      DOUBLE PRECISION hc_G_to_F_time_ratio
      DOUBLE PRECISION ad_G_to_F_time_ratio
      DOUBLE PRECISION coloring_time


C     Differencing Variables
C     **********************

      DOUBLE PRECISION abs_diff
      DOUBLE PRECISION rel_diff
      DOUBLE PRECISION max_rel_diff
      DOUBLE PRECISION max_abs_diff



C     Begin Code
C     **********

      OPEN(unit=11,file='ps_nonsparse_msa.out',status='unknown')

      WRITE(*,101)
 101  FORMAT(/,'***',/,'MSA: Partially Separated; ',
     &       'Nonsparse ADIFOR',/,'***',//,
     &       '                           Func     HC_Grd    HC_G:F  ',
     &       ' Coloring   AD_Jac   AD_G_add   AD_G:F    AD Max  ',
     &       '  AD Max ',/,
     &       '    n       m      p       Time      Time      Ratio  ',
     &       '   Time      Time      Time      Ratio    Rel Err ',
     &       '  Abs Err',/,
     &       ' ======  ======  ======  ========  ========  ======== ',
     &       ' ========  ========  ========  ========  ======== ',
     &       ' ========')


C     Perform 6 Gradient Computations of Increasing Gradient Size, n 
C     **************************************************************

      DO k = 1, 6
         n = 2500 * k * k
      

C        Set Initial MINPACK-2 Parameter Values
C        **************************************

         nx   = sqrt(dble(n))
         ny   = nx


C        Initialize Independent Var., x
C        ******************************

         task = 'XS'
         CALL dmsamain(nx,ny,x,f,hc_g,task,w)


C        Average over 10 Runs the Time to Compute the Function, f
C        ********************************************************

         task = 'F'
         CALL timer(start_time)
         DO h = 1, 10
            CALL dmsabc(nx,ny,bottom,top,left,right)
            CALL dmsasep(nx,ny,x,fvec,bottom,top,left,right,m)
            f = 0.0d0
            DO i = 1, m
               f = f + fvec(i)
            ENDDO
         ENDDO
         CALL timer(stop_time)
         F_compute_time = (stop_time - start_time)/10.0d0


C        Time the Computation of MINPACK-2's Hand-coded Gradient, hc_g
C        *************************************************************

         task = 'G'
         CALL timer(start_time)
         CALL dmsamain(nx,ny,x,f,hc_g,task,w)
         CALL timer(stop_time)
         hc_G_compute_time = stop_time - start_time 
         hc_G_to_F_time_ratio = hc_G_compute_time/F_compute_time


C        Time the Sparsity Pattern and Coloring Computations
C        ***************************************************

         CALL timer(start_time)
         CALL dmsasps(nx,ny,indrow,indcol,nnz)
         CALL dsm(m,n,nnz,indrow,indcol,ngrp,maxgrp,mingrp,
     &        info,ipntr,jpntr,iwa,liwa)
         CALL timer(stop_time)
         coloring_time = stop_time - start_time


C        Initialize the Seed Matrix, g_x
C        *******************************

         DO i = 1, pmax
            DO j = 1, n
               g_x(i,j) = 0.0d0
            ENDDO
         ENDDO
         DO j = 1, n
            g_x(ngrp(j),j) = 1.0d0
         ENDDO


C        Time the Computation of the transposed Jacobian, g_fvec
C        *******************************************************

         CALL timer(start_time)
         CALL g_dmsasep(maxgrp, nx, ny, x, g_x, pmax,
     &                  fvec, g_fvec, pmax, bottom, top, left, right, m)
         CALL timer(stop_time)
         ad_J_compute_time = stop_time - start_time


C        Time the Accumulation of the Gradient, ad_g
C        *******************************************

         CALL timer(start_time)
         DO j = 1, n
            ad_g(j) = 0.0d0
            DO loc = jpntr(j), jpntr(j+1) - 1
                i = indrow(loc)
                ad_g(j) = ad_g(j) + g_fvec(ngrp(j),i)
            ENDDO
         ENDDO
         CALL timer(stop_time)
         ad_G_compute_time = stop_time - start_time
         ad_G_to_F_time_ratio =
     &      (ad_J_compute_time + ad_G_compute_time) / F_compute_time


C        Compute Absoulte and Relative Errors between hc_g and ad_g
C        **********************************************************

         max_rel_diff = 0.0D0
         max_abs_diff = 0.0D0
         DO j = 1, n
            abs_diff = abs(hc_g(j) - ad_g(j))
            IF (abs(hc_g(j)) .ge. 1.0e-15) THEN
               rel_diff = abs_diff / abs(hc_g(j))
            ELSE
               rel_diff = abs_diff
            ENDIF
            IF (rel_diff .gt. max_rel_diff) THEN
	       max_rel_diff = rel_diff
            ENDIF
            IF (abs_diff .gt. max_abs_diff) THEN
	       max_abs_diff = abs_diff
            ENDIF
         ENDDO


C        Print All Results
C        *****************

         WRITE(*,201) n,m,maxgrp,
     &                F_compute_time,
     &                hc_G_compute_time,
     &                hc_G_to_F_time_ratio,
     &                coloring_time,
     &                ad_J_compute_time,
     &                ad_G_compute_time,
     &                ad_G_to_F_time_ratio,
     &                max_rel_diff,
     &                max_abs_diff
         WRITE(11,201) n,m,maxgrp,
     &                F_compute_time,
     &                hc_G_compute_time,
     &                hc_G_to_F_time_ratio,
     &                coloring_time,
     &                ad_J_compute_time,
     &                ad_G_compute_time,
     &                ad_G_to_F_time_ratio,
     &                max_rel_diff,
     &                max_abs_diff
  201    FORMAT(1x,i6,2x,i6,i6,2x,9(2x,e8.3))


      ENDDO


C     Close File
C     **********
     
      CLOSE(11)
            
      STOP
      END

