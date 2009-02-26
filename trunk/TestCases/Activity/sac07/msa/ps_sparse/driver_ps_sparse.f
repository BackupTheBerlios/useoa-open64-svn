C234567890         0         0         0         0         0         012
C                                                                      C
C     Sparse derivative driver code for the sparse version of          C
C     the adifor-generated code for computing gradient of the          C
C     Partially-Separated formulation of MINPACK-2's Minimal           C
C     Surface Area (MSA) problem.                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PROGRAM ps_sparse_msa_driver


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


C     Variables Related to Derivative Computation
C     *******************************************

C     The independent variable.
      DOUBLE PRECISION x(nmax)

C     The scalar aggregate function computed in MINPACK-2.
      DOUBLE PRECISION f

C     The dependent variable; i.e.,
C     the partially-separated reformulation of f.
      DOUBLE PRECISION fvec(mmax)

C     MINPACK-2 work array.
      DOUBLE PRECISION w(nmax)

C     Array of pointers, pointing to derivatives of x.
C     The jth entry, s_x(j), points to the sparse representation
C     of the gradient vector dx(j)/dx.
C     s_x contains the seed matrix.
      INTEGER s_x(nmax)

C     Array of pointers, pointing to derivatives of fvec.
C     The ith entry, s_fvec(i), points to the sparse representation
C     of the gradient vector, dfvec(i)/dx.
C     s_fvec contains the final Jacobian of interest.
      INTEGER s_fvec(mmax)

C     MINPACK-2's analytically-computed gradient.
      DOUBLE PRECISION hc_g(nmax)

C     Gradient computed by ADIFOR/SparsLinC.
      DOUBLE PRECISION ad_g(nmax)


C     SparsLinC Variables
C     *******************

C     DSPXAQ output variable: largest index extracted.
      INTEGER out_len_xaq

C     DSPXAQ output variable: flag indicating whether the space
C     provided for the gradient (i.e., the dimension of ad_g) is
C     sufficient to extract all nonzeroes.
      INTEGER info_xaq

C     XSPMEM output variable: Kbytes of dynamic memory
C     allocated by SparsLinC.
      REAL used_kb


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


C     Differencing Variables
C     **********************

      DOUBLE PRECISION abs_diff
      DOUBLE PRECISION rel_diff
      DOUBLE PRECISION max_rel_diff
      DOUBLE PRECISION max_abs_diff



C     Begin Code
C     **********

      OPEN(unit=11,file='ps_sparse_msa.out',status='unknown')

      WRITE(*,101)
 101  FORMAT(/,'***',/,'MSA: Partially Separated; ',
     &       'ADIFOR/SparsLinC',/,'***',//,
     &       '                   Func     HC_Grd    HC_G:F  ',
     &       '  AD_Jac   AD_G_add   AD_G:F    AD Max  ',
     &       '  AD Max   Dynamic',/,
     &       '    n       m      Time      Time      Ratio  ',
     &       '   Time      Time      Ratio    Rel Err ',
     &       '  Abs Err   Memory',/,
     &       ' ======  ======  ========  ========  ======== ',
     &       ' ========  ========  ========  ======== ',
     &       ' ========  ========')


C     SparsLinC Initialization
C     ************************

      CALL XSPINI


C     Perform 6 Gradient Computations of Increasing Gradient Size, n 
C     **************************************************************

      DO k = 1, 6
         n = 2500 * k * k


C        Set Initial MINPACK-2 Parameter Values
C        **************************************

         nx = sqrt(dble(n))
         ny = nx


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


C        Initialize the Seed Matrix, s_x
C        *******************************

         DO j = 1, n
            CALL DSPSD(s_x(j),j,1.d0,1)
         ENDDO


C        Time the Computation of the Jacobian, s_fvec
C        ********************************************

         CALL timer(start_time)
         CALL s_dmsasep(nx, ny, x, s_x,
     &                  fvec, s_fvec, bottom, top, left, right, m)
         CALL timer(stop_time)
         ad_J_compute_time = stop_time - start_time


C        Time the Extraction (Accumulation) of the Gradient, ad_g
C        ********************************************************

         CALL timer(start_time)
         DO j = 1, n
            ad_g(j) = 0.0d0
         ENDDO

C        Since out_len_xaq can never be greater than n, info_xaq
C        is always 0, hence there is no need to check its value.
C        However, we do so for illustrative purposes.

         DO i = 1, m
            CALL DSPXAQ(ad_g(1),n,s_fvec(i),out_len_xaq,info_xaq)
            IF (info_xaq .ne. 0) THEN
               WRITE(*,151) n, out_len_xaq
 151           FORMAT('Space provided for extracting the gradient = ',
     &                i6,' was inadequate to accommodate',/,'the ',
     &                'largest index of the nonzero index set = ',i6)
               STOP
            ENDIF
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


C        Get Dynamic Memory Usage Information
C        ************************************

         CALL XSPMEM(used_kb)


C        Print Results to Screen and to File
C        ***********************************

         WRITE(*,201) n,m,
     &                F_compute_time,
     &                hc_G_compute_time,
     &                hc_G_to_F_time_ratio,
     &                ad_J_compute_time,
     &                ad_G_compute_time,
     &                ad_G_to_F_time_ratio,
     &                max_rel_diff,
     &                max_abs_diff,
     &                used_kb
         WRITE(11,201) n,m,
     &                F_compute_time,
     &                hc_G_compute_time,
     &                hc_G_to_F_time_ratio,
     &                ad_J_compute_time,
     &                ad_G_compute_time,
     &                ad_G_to_F_time_ratio,
     &                max_rel_diff,
     &                max_abs_diff,
     &                used_kb
 201     FORMAT(1x,i6,2x,i6,9(2x,e8.3))


      ENDDO


C     Free Dynamically Allocated Memory and Close File
C     ************************************************

      CALL XSPFRA
      CLOSE(11)

      STOP
      END

