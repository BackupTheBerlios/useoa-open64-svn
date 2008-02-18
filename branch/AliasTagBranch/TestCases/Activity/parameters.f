
      subroutine foo
           double precision :: x,y
           y=x
           call bar(x,y)
      end subroutine

      subroutine bar(xx,yy)
           double precision :: xx,yy
           t=yy
           xx=t
      end subroutine
