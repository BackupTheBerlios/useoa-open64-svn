!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! A simple program with scalar operations
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine head(x1, x2, y1, y2) 
        double precision t1, t2, x1, x2, y1, y2
        
        t1 = x1*x2
        t2=x1*sin(t1)
        y1=cos(t2)
        y2=t2*x2
      end subroutine
      
      program simple1

      double precision :: x, y
      x = 10
      y = 100
      call head(x, y)
      write(*,*) y

      end program
