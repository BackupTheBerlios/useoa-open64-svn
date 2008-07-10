      program admain
      integer m,i,j
      integer PMAX
      parameter (PMAX = 5)
      double precision x, y(20), yp(20)
      double precision g_y(PMAX, 20), g_yp(PMAX, 20)

C     read in input values for m, x and y
      read (*,*) m
      read (*,*) x
      read (*,*) (y(j), j = 1, m)

C     Loop through the (m div 5) slices 

      do 50 i = 1, (m/5)

C     initialize gradient object for the independent variable
C     so that columns 1-5 and rows 1-5 of slice i form a 5x5
C     identity matrix (see section 3.2 of the manual)
 
        do j = 1, 5
           do k = 1, m
              g_y(j,k) = 0.0
           enddo
           g_y(j,5*(i-1)+j) = 1.0
        enddo

      call g_fcn2(5, m, x, y, g_y, PMAX, yp, g_yp, PMAX)

      call ehrpt

C     print out transpose of g_yp
      do 30 k = 1, 5
         write (*, 40) (g_yp(k, j), j = 1, m)
         print *
 30   continue

 40   format ((20e16.8))

 50   continue
      end
