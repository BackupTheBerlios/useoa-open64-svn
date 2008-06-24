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
C       _SPARSE     : Using SparsLinC-1.0 Library                      C
C       1 : Using Nonsparse ADIFOR MODE                      C
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




      INTEGER nnzmax                        


      PARAMETER (nnzmax=12*nmax)


      INTEGER liwa                          
      PARAMETER(liwa=6*nmax)

      INTEGER nmaxplus1                     
      PARAMETER(nmaxplus1=nmax+1)

      INTEGER mmaxplus1                     
      PARAMETER(mmaxplus1=mmax+1)

      INTEGER nnz                           


      INTEGER maxgrp                        


      INTEGER mingrp                        




      INTEGER indrow(nnzmax)                















      INTEGER indcol(nnzmax)                















      INTEGER ngrp(nmax)                    





      INTEGER ipntr(mmaxplus1)              












      INTEGER jpntr(nmaxplus1)              












      INTEGER iwa(liwa)                     
      INTEGER info                          


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



      DOUBLE PRECISION g_x(pmax,nmax)
      DOUBLE PRECISION g_fvec(pmax,mmax)





C      DOUBLE PRECISION ad_fjacval(nnzmax)  





C     Variables for FDJS
C     ******************

C      INTEGER numgrp
C      DOUBLE PRECISION eta(nmax)           
C
C      DOUBLE PRECISION fjacd(mmax)         





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

      INTEGER ip                            

      INTEGER j                             
      INTEGER iouter                        



      DOUBLE PRECISION stime                
      DOUBLE PRECISION ftime                
      DOUBLE PRECISION F_compute_time       
      DOUBLE PRECISION ad_J_compute_time    




      DOUBLE PRECISION ad_J_unravel_time    

      DOUBLE PRECISION ad_G_compute_time    


      DOUBLE PRECISION ad_G_to_F_ratio      




      DOUBLE PRECISION dsm_time             

 


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




      OPEN(unit=timeunit,file='dsfd_nonsparse.time',status='unknown')
      OPEN(unit=derivunit,file='dsfd_nonsparse.deriv',status='unknown')




      WRITE(*,100)




 100  FORMAT(/,'****',/,'DSFD: Nonsparse ADIFOR',/,'****')




      WRITE(*,101)

 101  FORMAT(/,
     &       '                           Func   ',
     &       ' Coloring ',
     &       '  AD_Jac    AD_Unr   AD_G_add   AD_G:F ',/,
     &       '    n       m    maxgrp    Time   ',
     &       '   Time     ',
     &       '   Time      Time      Time      Ratio ',/,
     &       ' ======  ======  ======  ======== ',
     &       ' ======== ',
     &       ' ========  ========  ========  ========')



      
C     SP initialize: SSBuckets=pmax; CSBuckets=2; Switch=pmax+1 (never)
C     *****************************************************************



      
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





C        Get the sparsity pattern
C        ************************

         CALL timer(stime)

         CALL dsfdsp(n,n/14,nnz,indrow,indcol)


C         PRINT*, 'n = ',n,' nnz = ',nnz


C        Get a permutation via Coleman/More'
C        ***********************************

         CALL dsm(m,n,nnz,indrow,indcol,ngrp,maxgrp,mingrp,
     &            info,ipntr,jpntr,iwa,liwa)
         CALL timer(ftime)
         dsm_time = ftime - stime
C         Print*, 'maxgrp = ',maxgrp




C***********************************************************************
C                                                                      *
C                       AUTOMATIC DIFFERENTIATION                      *
C                                                                      *
C***********************************************************************


C        Initialize the Seed Matrix
C        **************************


         DO i = 1, pmax
            DO j = 1, n
               g_x(i,j) = 0.0d0
            ENDDO
         ENDDO
         DO i = 1, n
            g_x(ngrp(i),i) = 1.0d0
C            WRITE(*,*) i,ngrp(i)
         ENDDO



C        Compute Compressed Jacobian (& Time the Computation)
C        ****************************************************

         CALL timer(stime)
         task = 'F'




         CALL g_dsfdfj(pmax,n,x,g_x,pmax,fvec,g_fvec,pmax,
     &                 fjac,1,task,0.1d0,n/14)




         CALL timer(ftime)
         ad_J_compute_time = ftime - stime


C        Extract & Unravel Compressed Jacobian
C        *************************************

         CALL timer(stime)


C         DO i = 1, n
C            eta(i) = 1.0D0
C         ENDDO
C         DO numgrp = 1, maxgrp 	
C            DO j = 1, m
C               fjacd(j) = g_fvec(numgrp,j) 
C            ENDDO	
C            CALL fdjs(m,n,.true.,indrow,jpntr,ngrp,numgrp,eta,
C     &                fjacd,ad_fjacval)
C         ENDDO



         CALL timer(ftime)
         ad_J_unravel_time = ftime - stime

            
C        Compute (Accumulate) Gradient
C        *****************************

         CALL timer(stime)


CCC         DO j = 1, n
CCC            ad_g(j) = 0.0d0
CCC            DO i = jpntr(j), jpntr(j+1) - 1
CCC               ad_g(j) = ad_g(j) + ad_fjacval(i)
C               PRINT*,indrow(i),j,fjacad(i),fjacdd(i)
CCC            ENDDO
CCC         ENDDO


         DO j = 1, n
            ad_g(j) = 0.0d0
            DO ip = jpntr(j), jpntr(j+1) - 1
                i = indrow(ip)
                ad_g(j) = ad_g(j) + g_fvec(ngrp(j),i)
            ENDDO
         ENDDO

C         DO j = 1, n
C            ad_g(j) = 0.0d0
C         ENDDO
C         DO i = 1, n
C            DO ip = ipntr(i), ipntr(i+1) - 1
C               j = indcol(ip)
C               ad_g(j) = ad_g(j) + g_fvec(ngrp(j),i)
C            ENDDO
C         ENDDO


C         PRINT*, 'COMPUTED GRADIENT:', (ad_g(i),i=1,n)


         CALL timer(ftime)
         ad_G_compute_time = ftime - stime
         ad_G_to_F_ratio = (ad_J_compute_time + ad_J_unravel_time +
     &                      ad_G_compute_time) /
     &                     F_compute_time




C        Get Memory Usage Information
C        ****************************
           




C        Print All Results
C        *****************



         WRITE(*,200) n,m,maxgrp,
     &                F_compute_time,
     &                dsm_time,
     &                ad_J_compute_time,
     &                ad_J_unravel_time,
     &                ad_G_compute_time,
     &                ad_G_to_F_ratio
  200    FORMAT(1x,i6,2x,i6,i6,2x,6(2x,e8.3))

         WRITE(timeunit,201) n,m,maxgrp,
     &                F_compute_time,
     &                dsm_time,
     &                ad_J_compute_time,
     &                ad_J_unravel_time,
     &                ad_G_compute_time,
     &                ad_G_to_F_ratio
  201    FORMAT(1x,i6,2x,i6,i6,2x,6(2x,e8.3))



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


     
      CLOSE(timeunit)
      CLOSE(derivunit)
            
      STOP
      END


