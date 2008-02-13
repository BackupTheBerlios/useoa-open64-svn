


        program arraypointer
         double precision, dimension(3), target :: x = (/1.0,2.0,3.0/)
         double precision, dimension(:), pointer :: p
         allocate(p(1:3))
        end program

