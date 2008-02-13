       subroutine head()
         double precision x(2)
         double precision y(2)
         integer i
c$openad INDEPENDENT(x)
         x(1) = 5
         x(2) = 7
         i=1
         do while (i<3)
           if (i<2) then
             y(2)=x(1)
           else
             y(1)=x(2)
           end if
           i=i+1
         end do
         y(2)=y(1)*y(2)
c$openad DEPENDENT(y)
       end subroutine

