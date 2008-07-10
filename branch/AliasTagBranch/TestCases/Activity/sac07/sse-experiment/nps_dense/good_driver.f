C234567890         0         0         0         0         0         012
C                                                                      C
C     Nonsparse derivative driver code for the nonsparse version of    C
C     the adifor-generated code for computing gradient of the          C
C     original (Not Partially-Separated) MINPACK-2 Minimal             C
C     Surface Area (MSA) problem.                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PROGRAM nps_nonsparse_msa_driver


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

C     Runtime # of grid points in the 1st coordinate dimension.
      INTEGER nx

C     Runtime # of grid points in the 2nd coordinate dimension.
      INTEGER ny

C     Runtime gradient size; i.e., # of independent variables.
      INTEGER n

C     MINPACK-2 action specifier variable.
      CHARACTER*6 task


C     Parameters and Variable for Stripmining of Derivative Computation
C     *****************************************************************

C     Max size of each strip or chunk.  Due to memory restrictions
C     it is not possible to differentiate with respect to all (nmax)
C     independent variables simultaneously.  Hence a reasonable value
C     is chosen, based on the memory requirements of the function
C     computation, and the resident memory of the platform on which
C     the derivative code will be run.
      INTEGER max_chunk_size
      PARAMETER (max_chunk_size=16)

C     Max # of chunks, i.e., Max value of n_chunk.
      INTEGER max_n_chunk
      PARAMETER (max_n_chunk=nmax/max_chunk_size)

C     Runtime # of chunks.
      INTEGER n_chunk

C     The actual size of each chunk.
C     chunk_size(i) is .le. max_chunk_size.
      INTEGER chunk_size(max_n_chunk)

C     Counter to keep count of the columns of the seed matrix for
C     which derivatives have been computed so far.
      INTEGER done_columns

C     Index ranging over the number of chunks.
      INTEGER i_chunk

C     Size of the last chunk.
C     last_chunk_size is .le. max_chunk_size.
      INTEGER last_chunk_size


C     Variables Related to Derivative Computation
C     *******************************************

C     Upper bound on the # of directions with respect to which
C     derivatives are computed.
C     Used as first dimension of each derivative object.
      INTEGER pmax
      PARAMETER (pmax=max_chunk_size)

C     The independent variable.
      DOUBLE PRECISION x(nmax)

C     The scalar aggregate function computed in MINPACK-2.
      DOUBLE PRECISION f

C     MINPACK-2 work array.
      DOUBLE PRECISION w(nmax)

C     The seed matrix.
      DOUBLE PRECISION g_x(pmax,nmax)

C     The derivatives of f with respect to a strip of size pmax of
C     the independent variables.
      DOUBLE PRECISION g_f(pmax)

C     MINPACK-2's analytically-computed gradient.
      DOUBLE PRECISION hc_g(nmax)

C     Gradient computed by ADIFOR.
      DOUBLE PRECISION ad_g(nmax)


C     Iteration Index Variables
C     *************************

C     Index used for averaging function time.
      INTEGER h

C     Index ranging over rows of the seed matrix.
      INTEGER i

C     Index ranging over the n independent variables.
      INTEGER j

C     Gradient computations counter.
      INTEGER k


C     Timing Variables
C     ****************

      DOUBLE PRECISION start_time
      DOUBLE PRECISION stop_time
      DOUBLE PRECISION F_compute_time
      DOUBLE PRECISION hc_G_compute_time
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

      OPEN(unit=11,file='nps_nonsparse_msa.out',status='unknown')

      WRITE(*,101)
 101  FORMAT(/,'***',/,'MSA: Not Partially Separated; ',
     &       'Nonsparse ADIFOR',/,'***',//,
     &       '           Func     HC_Grd    HC_G:F    AD_Grd  ',
     &       '  AD_G:F    AD Max    AD Max ',/,
     &       '    n      Time      Time      Ratio     Time   ',
     &       '   Ratio    Rel Err   Abs Err',/,
     &       ' ======  ========  ========  ========  ======== ',
     &       ' ========  ========  ========')


C     Perform 6 Gradient Computations of Increasing Gradient Size, n 
C     ***************************************************************

      DO k = 1, 6
         n = 100 * k * k


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
            CALL dmsamain(nx,ny,x,f,hc_g,task,w)
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


C        Setting up Uniformly-sized Chunks for Stripmining
C        *************************************************

         n_chunk =  n/max_chunk_size
         DO i_chunk = 1, n_chunk
            chunk_size(i_chunk) = max_chunk_size
         ENDDO
         last_chunk_size = n - (n_chunk * max_chunk_size)
         IF (last_chunk_size .ne. 0) THEN
            n_chunk = n_chunk + 1
            chunk_size(n_chunk) = last_chunk_size
         ENDIF


C        Perform Derivative Computation per Chunk
C        ****************************************

         ad_G_compute_time = 0.0
         done_columns = 0

         DO i_chunk = 1, n_chunk


C           Initialize the Seed Matrix, g_x
C           *******************************

            DO i = 1, pmax
               DO j = 1, n
                  g_x(i,j) = 0.0d0
               ENDDO
            ENDDO
            DO i = 1, chunk_size(i_chunk)
               g_x(i,i+done_columns) = 1.0d0
            ENDDO


C           Time the Computation of a Derivative Chunk, g_f
C           ***********************************************
C           And the Accumulation of the Gradient, ad_g
C           ******************************************

            task = 'F'
            CALL timer(start_time)
            CALL g_dmsamain(pmax, nx, ny, x, g_x, pmax,
     &                      f, g_f, pmax, hc_g, task, w)
            DO j = done_columns+1, done_columns+chunk_size(i_chunk)
               ad_g(j) = g_f(j-done_columns)
            ENDDO
            CALL timer(stop_time)
            ad_G_compute_time = ad_G_compute_time +
     &                          (stop_time - start_time)

            done_columns = done_columns + chunk_size(i_chunk)

         ENDDO

         ad_G_to_F_time_ratio = ad_G_compute_time / F_compute_time


C        Compute Absolute and Relative Errors between hc_g and ad_g
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


C        Print Results to Screen and to File
C        ***********************************

         WRITE(*,201) n,
     &                F_compute_time,
     &                hc_G_compute_time,
     &                hc_G_to_F_time_ratio,
     &                ad_G_compute_time,
     &                ad_G_to_F_time_ratio,
     &                max_rel_diff,
     &                max_abs_diff
         WRITE(11,201) n,
     &                F_compute_time,
     &                hc_G_compute_time,
     &                hc_G_to_F_time_ratio,
     &                ad_G_compute_time,
     &                ad_G_to_F_time_ratio,
     &                max_rel_diff,
     &                max_abs_diff
 201     FORMAT(1x,i6,7(2x,e8.3))


      ENDDO


C     Close File
C     **********

      CLOSE(11)
      
      STOP
      END

