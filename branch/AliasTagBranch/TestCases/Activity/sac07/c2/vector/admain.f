      program admain
      integer m,j
      double precision x, y(20), yp(20)
      double precision v(20), g_y(20), g_yp(20)

C     read in input values for m, x, y, and v
      read (*,*) m
      read (*,*) x
      read (*,*) (y(j), j = 1, m)
      read (*,*) (v(j), j = 1, m)

C     initialize gradient object for the independent variable
C     so that g_y = v (on exit g_yp will equal Jv)

      do j = 1, m
         g_y(j) = v(j)
      enddo

      call g_fcn2(1, m, x, y, g_y, 1, yp, g_yp, 1)

      call ehrpt

C     print out transpose of g_yp
         write (*, 40) (g_yp(j), j = 1, m)

 40   format ((20e16.8))
      end
