       module globals

        double precision, dimension(2) :: x

      end module

      subroutine head()
       use globals

       double precision t2

       t2 = x(1)
      end subroutine


