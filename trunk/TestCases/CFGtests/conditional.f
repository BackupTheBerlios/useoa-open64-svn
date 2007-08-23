
       program reachdefs_if
         double precision :: x, y, z
         x = 2
         if ( x .ge. 0. ) then
             y = 5
         else
             y = 3
         endif
         z = y
       end program

