
      subroutine head()
       double precision, dimension(10) :: x
       common /cpad/ x 

       double precision t2

       t2 = x(2:4)
      end subroutine


