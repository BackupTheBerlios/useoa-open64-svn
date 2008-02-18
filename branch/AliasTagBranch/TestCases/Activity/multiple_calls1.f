
      subroutine foo()
          double precision ::x,a,y
          call bar(x,a,y)
          call bar(x,a,y)
      end subroutine


      subroutine bar(f,b,g)
          double precision :: f,b,g
          g=b
          b=f
      end subroutine
      
