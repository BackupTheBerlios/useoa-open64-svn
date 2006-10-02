       subroutine head(x,y)
         double precision, dimension(2), intent(inout) :: x
         double precision, dimension(2), intent(inout) :: y
         double precision t


         t=x(1)*x(2)
         y(1)=sin(t)
         y(2)=cos(t)

       end subroutine
