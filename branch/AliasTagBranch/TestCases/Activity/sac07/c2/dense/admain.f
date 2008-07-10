      program admain
      integer m,i,j
      integer PMAX
      parameter (PMAX = 20)
      double precision x, y(20), yp(20)
      double precision g_y(PMAX, 20), g_yp(PMAX, 20)

C     read in input values for m, x and y
      read (*,*) m
      read (*,*) x
      read (*,*) (y(j), j = 1, m)

C     initialize gradient object for the independent variable
C     to the m x m identity matrix (seed matrix g_y = I)
      do i = 1, m
         do j = 1, m
            g_y(i,j) = 0.0
         enddo
         g_y(i,i) = 1.0
      enddo

      call g_fcn2(20, m, x, y, g_y, PMAX, yp, g_yp, PMAX)

      call ehrpt

C     print out transpose of g_yp
      do 30 i = 1, m
         write (*, 40) (g_yp(i, j), j = 1, m)
         print *
 30   continue

 40   format ((20e16.8))
      end
