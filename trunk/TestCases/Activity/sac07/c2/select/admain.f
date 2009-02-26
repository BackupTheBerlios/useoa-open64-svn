      program admain
      integer m,i,j
      integer PMAX
      parameter (PMAX = 3)
      double precision x, y(20), yp(20)
      double precision g_y(PMAX, 20), g_yp(PMAX, 20)

C     read in input values for m, x and y
      read (*,*) m
      read (*,*) x
      read (*,*) (y(j), j = 1, m)

C     initialize gradient object for the independent variable
C     so that rows 3, 8, and 17 and columns 1-3 of the seed matrix
C     (columns 3, 8, 17 and rows 1-3 of g_y) form a 3 x 3
C     identity matrix (see section 3.2.2 of the manual)

        do j = 1, 3
           do k = 1, m
              g_y(j,k) = 0.0
           enddo
        enddo

        g_y(1,3) = 1.0
        g_y(2,8) = 1.0
        g_y(3,17) = 1.0

        call g_fcn2(3, m, x, y, g_y, PMAX, yp, g_yp, PMAX)

        call ehrpt

C     print out transpose of g_yp
      do 30 i = 1, 3
         write (*, 40) (g_yp(i, j), j = 1, m)
         print *
 30   continue

 40   format ((20e16.8))
      end
