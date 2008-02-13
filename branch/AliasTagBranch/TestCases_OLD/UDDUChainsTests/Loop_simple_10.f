        subroutine foo(x,y,a,j)
          double precision, dimension(1), intent(in) :: x
          double precision, dimension(1), intent(out) :: y
    	  integer a(2,2)
          integer i,j
          y(1)=x(1)
          do i=1,2
            if (i.ne.0) then
              y(1)=y(1)*x(1)*a(i,j)
            else
              y(1)=y(1)-x(1)
            end if
          end do
        end subroutine


