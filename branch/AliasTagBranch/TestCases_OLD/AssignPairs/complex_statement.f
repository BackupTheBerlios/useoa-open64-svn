

      subroutine bar( x,y,z)
         integer x,y,x
         x = y+z
      end subroutine

      subroutine foo()
         integer a,b,c,d
         call bar( c, a+b, d)
      end subroutine
