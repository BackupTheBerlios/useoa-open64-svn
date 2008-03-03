
      subroutine foo()
          double precision ::x,a,y
c$openad INDEPENDENT(x)
          call bar(x,a,y)
          call bar(x,a,y)
c$openad DEPENDENT(y)
      end subroutine


      subroutine bar(f,b,g)
          double precision :: f,b,g
          g=b
          b=f
      end subroutine
      
