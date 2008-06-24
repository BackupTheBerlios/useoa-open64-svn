C23456789012345678901234567890123456789012345678901234567890123456789012
C                                                                      C
C     PROGRAM driver_base                                              C
C                                                                      C
C     Driver for the nonsparse and sparse ADIFOR 2.0-generated         C
C     code for computing derivatives for the following MINPACK-2       C
C     Systems of Nonlinear Equations:                                  C
C                                                                      C
C             DFDC: Flow in a Driven Cavity                            C
C             DFIC: Flow In a Channel                                  C
C             DIER: Incompressible Elastic Rod                         C
C             DSFD: Swirling Flow between Disks                        C
C             DSFI: Solid Fuel Ignition                                C
C                                                                      C
C     Preprocessor Macros:                                             C
C       1     : Using SparsLinC-1.0 Library                      C
C       _NON_SPARSE : Using Nonsparse ADIFOR MODE                      C
C       _DFDC       : Code specific to DFDC                            C
C       _DFIC       : Code specific to DFIC                            C
C       _DIER       : Code specific to DIER                            C
C       1       : Code specific to DSFD                            C
C       _DSFI       : Code specific to DSFI                            C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      PROGRAM dsfd_driver



C     Parameters & Variables for MINPACK-2 and DSM (Coloring) Routines
C     ****************************************************************

      INTEGER nmax                          
      PARAMETER(nmax=160000)

      INTEGER mmax                          
      PARAMETER(mmax=nmax)

      INTEGER n                             

      INTEGER m                             





      CHARACTER*6 task                      


C     MINPACK-2 Variables Related to Derivative Objects
C     *************************************************
      
      DOUBLE PRECISION x(nmax)              

      DOUBLE PRECISION fvec(mmax)           

      DOUBLE PRECISION fjac(1,1)            

      DOUBLE PRECISION ad_g(nmax)           




C     Parameters & Variables for Derivative Objects
C     *********************************************

      INTEGER pmax                          



      PARAMETER (pmax=14)



      INTEGER s_x(nmax)                     
      INTEGER s_fvec(mmax)                  




C      DOUBLE PRECISION gf_val(pmax,mmax)
C                                            


C      INTEGER gf_ind(pmax,mmax)
C                                            


      INTEGER rowlen                        



      INTEGER info_xs                       



      REAL usedkb                           







C     Local Parameters & Variables
C     ****************************

      INTEGER NNVAL                         
      PARAMETER(NNVAL=8)

      INTEGER nval(NNVAL)                   


      INTEGER timeunit                      
      PARAMETER(timeunit=11)

      INTEGER derivunit                    

      PARAMETER(derivunit=12)

      INTEGER i                             

      INTEGER j                             
      INTEGER iouter                        


CCC         INTEGER k
CCC                                            




      DOUBLE PRECISION stime                
      DOUBLE PRECISION ftime                
      DOUBLE PRECISION F_compute_time       
      DOUBLE PRECISION ad_J_compute_time    




      DOUBLE PRECISION ad_J_unravel_time    

      DOUBLE PRECISION ad_G_compute_time    


      DOUBLE PRECISION ad_G_to_F_ratio      




 


C     Random Number Generator
C     ***********************

      REAL RAN0

C     Data  statement with  NNVAL  values for  n  given the
C     problem-dependent constraints on the values n can take
C     ******************************************************
C           DFDC: n = nx*ny
C           DFIC: n = nint*8
C           DIER: n = nint*15+3
C           DSFD: n = nint*14
C           DSFI: n = nx*ny


      DATA nval/2492, 9996,22498,39998,62496,89992,122500,159992/



C     Begin Code
C     **********




      OPEN(unit=timeunit,file='dsfd_sparse.time',status='unknown')
      OPEN(unit=derivunit,file='dsfd_sparse.deriv',status='unknown')




      WRITE(*,100)




 100  FORMAT(/,'****',/,'DSFD: ADIFOR/SparsLinC',/,'****')




      WRITE(*,101)

 101  FORMAT(/,
     &       '                   Func   ',
     &       '  AD_Jac    AD_Unr   AD_G_add   AD_G:F  ',
     &       ' Dynamic',/,
     &       '    n       m      Time   ',
     &       '   Time      Time      Time      Ratio  ',
     &       '  Memory',/,
     &       ' ======  ======  ======== ',
     &       ' ========  ========  ========  ======== ',
     &       ' ========')



      
C     SP initialize: SSBuckets=pmax; CSBuckets=2; Switch=pmax+1 (never)
C     *****************************************************************



      CALL XSPCNF ( 1, pmax )
      CALL XSPCNF ( 2, 2 )
      CALL XSPCNF ( 3, pmax+1 )





      CALL XSPINI


      
C     Compute Gradients d(f)/d(x) for Increasing Gradient Sizes, n 
C     ************************************************************


      DO iouter = 1, NNVAL


C        Set Initial MINPACK-2 Parameter Values
C        **************************************

         n = nval(iouter)
         m = n




C        Get Starting Point from MINPACK-2 and Add Noise
C        ***********************************************

         task = 'XS'

         call dsfdfj(n,x,fvec,fjac,1,task,0.1d0,n/14)


         DO i = 1, n
            x(i) = x(i) + DBLE(RAN0(12345) * 1.0)
         ENDDO


C        Measure Time to Compute the Function
C        ************************************

         CALL timer(stime)
         task = 'F'
         DO i = 1, 5

         call dsfdfj(n,x,fvec,fjac,1,task,0.1d0,n/14)

         ENDDO
         CALL timer(ftime)
         F_compute_time = (ftime - stime)/5.0d0






C***********************************************************************
C                                                                      *
C                       AUTOMATIC DIFFERENTIATION                      *
C                                                                      *
C***********************************************************************


C        Initialize the Seed Matrix
C        **************************


         DO j = 1, n
            CALL DSPSD(s_x(j),j,1.d0,1)
         ENDDO



C        Compute Compressed Jacobian (& Time the Computation)
C        ****************************************************

         CALL timer(stime)
         task = 'F'




         CALL g_dsfdfj(n,x,s_x,fvec,s_fvec,
     &                 fjac,1,task,0.1d0,n/14)




         CALL timer(ftime)
         ad_J_compute_time = ftime - stime


C        Extract & Unravel Compressed Jacobian
C        *************************************

         CALL timer(stime)


         DO j = 1, n
            ad_g(j) = 0.0d0
         ENDDO
         DO i = 1, m
           CALL DSPXAQ(ad_g(1),n,
     &           s_fvec(i),rowlen,info_xs)
         ENDDO
C         DO i = 1, m
C           CALL DSPXSQ(gf_ind(1,i),gf_val(1,i),pmax,
C     &           s_fvec(i),rowlen,info_xs)
C           IF (info_xs .ne. 0) THEN
C              PRINT*,'m,info_xs:',m,info_xs
C           ENDIF
C         ENDDO

CCC         k = 1
CCC            DO i = 1, m
CCC               DO j = 1, rowlen(i)
C               indrow(k) = i
CCC                  indcol(k) = gf_ind(j,i)
CCC                  ad_fjacval(k) = gf_val(j,i)
CCC                  k = k + 1
CCC               ENDDO
CCC            ENDDO
CCC            nnz = k - 1
C         PRINT*, 'nnz = ',nnz
C         DO i = 1, nnz
C            PRINT*, indrow(i),indcol(i),ad_fjacval(i)
C         ENDDO



         CALL timer(ftime)
         ad_J_unravel_time = ftime - stime

            
C        Compute (Accumulate) Gradient
C        *****************************

         CALL timer(stime)


CCC         DO j = 1, n
CCC           ad_g(j) = 0.0d0
CCC         ENDDO
CCC         DO j = 1, nnz
CCC            ad_g(indcol(j)) = ad_g(indcol(j)) + ad_fjacval(j)
CCC         ENDDO


C         PRINT*, 'COMPUTED GRADIENT:', (ad_g(i),i=1,n)


         CALL timer(ftime)
         ad_G_compute_time = ftime - stime
         ad_G_to_F_ratio = (ad_J_compute_time + ad_J_unravel_time +
     &                      ad_G_compute_time) /
     &                     F_compute_time




C        Get Memory Usage Information
C        ****************************
           

         CALL XSPMEM(usedkb)




C        Print All Results
C        *****************



         WRITE(*,200) n,m,
     &                F_compute_time,
     &                ad_J_compute_time,
     &                ad_J_unravel_time,
     &                ad_G_compute_time,
     &                ad_G_to_F_ratio,
     &                usedkb
  200    FORMAT(1x,i6,2x,i6,6(2x,e8.3))

         WRITE(timeunit,201) n,m,
     &                F_compute_time,
     &                ad_J_compute_time,
     &                ad_J_unravel_time,
     &                ad_G_compute_time,
     &                ad_G_to_F_ratio,
     &                usedkb
  201    FORMAT(1x,i6,2x,i6,6(2x,e8.3))



      ENDDO

C     Write to file ad_g for largest n
C     ********************************

C      DO j = 1, nval(NNVAL)
      DO j = n/2, n/2 + 1000
         WRITE(derivunit,999) ad_g(j)
  999    FORMAT(e18.12)
      ENDDO


C     Free Dynamically Allocated Memory and Close Files
C     *************************************************


      CALL XSPFRA

     
      CLOSE(timeunit)
      CLOSE(derivunit)
            
      STOP
      END


