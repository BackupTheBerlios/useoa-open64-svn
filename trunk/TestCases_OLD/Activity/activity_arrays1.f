        subroutine head(x,y)
        double precision, dimension(3), intent(in) :: x
        double precision, dimension(3), intent(out) :: y

c$openad INDEPENDENT(x)
        y(1)=2. 
        if (x(1)>0.) then
          y(1)=x(1)
        end if
        y(1)=y(1)*y(1)
c$openad DEPENDENT(y)

        end subroutine

