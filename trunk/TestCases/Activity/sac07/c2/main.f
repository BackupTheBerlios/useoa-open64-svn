      program main
      integer m,j
      double precision x, y(20), yp(20)

      integer        id, iwt
      double precision w(20)
      common         /stcom5/w, iwt, n, id

C     read in input values for m, x and y
      read (*,*) m
      read (*,*) x
      read (*,*) (y(j), j = 1, m)

      call fcn2(m, x, y, yp)

      write (*, *) (yp(j), j = 1, m)

      end
