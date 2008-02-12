       module globals

        double precision, dimension(2) :: x

      end module

      subroutine head()
       use globals

       
       double precision, dimension(2) :: y

       x = y
      end subroutine


