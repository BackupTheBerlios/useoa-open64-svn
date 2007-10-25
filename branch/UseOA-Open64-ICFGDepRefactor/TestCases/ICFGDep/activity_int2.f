! for testing some intermediates between x and y

       subroutine head(x,y)
         double precision x(2),y(2)
         double precision t1, t2
         integer i
c$openad INDEPENDENT(x)
         i=1
         do while (i<3)
           t1 = x(1)
           if (i<2) then
             y(2)=sin(t1)
           else
             y(1)=cos(t2)
           end if
           i=i+1
         end do
         y(2)=y(1)*y(2)
c$openad DEPENDENT(y)
       end subroutine

