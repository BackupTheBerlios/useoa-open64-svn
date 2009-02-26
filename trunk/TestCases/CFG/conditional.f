! Reference:
! http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap03/grade-1.html


       program smallest_of_three
         double precision :: a,b,c
         double precision :: res
         a=5
         b=2
         c=3
         if ( a < b ) then
             if ( a < c ) then
                 res = a
             else 
                 res = c
             end if    
         else
             if ( b < c ) then
                 res = b
             else 
                 res = c
             end if
         end if
       end program

