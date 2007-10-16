
      subroutine foo()
        integer a,b
        call bar(a+b)
      end subroutine

      subroutine bar(c)
        integer c
        c = 10
      end subroutine
      
