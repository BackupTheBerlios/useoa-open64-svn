
      subroutine foo
          double precision :: x,a,b
          call bar(x,a,b)
          call bar(x,a,b)
      end subroutine

      subroutine bar(f,b,g)
          double precision :: f,b,g
          b=f
          g=b
      end subroutine
