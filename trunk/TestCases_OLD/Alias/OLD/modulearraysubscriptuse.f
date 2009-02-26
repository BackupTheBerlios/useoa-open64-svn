       module globals

        double precision, dimension(2) :: x

      end module

      subroutine head()
       use globals

       double precision t2
       integer j

       j=1
       t2 = x(j)
      end subroutine


