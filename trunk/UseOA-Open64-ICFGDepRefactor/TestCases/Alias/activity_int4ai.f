       subroutine head()
         double precision x(2)
         double precision y(2)
         integer i
c$openad INDEPENDENT(x)

         i = 1
         if (i<2) then
           y(2)=sin(x(1))
         else
           y(1)=sin(x(2))
         end if

         y(2)=y(1)*y(2)

c$openad DEPENDENT(y)
       end subroutine

