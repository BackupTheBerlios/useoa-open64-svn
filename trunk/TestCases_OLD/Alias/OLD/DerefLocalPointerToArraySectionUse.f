

       subroutine arraypointer() 
         double precision, dimension(3), target :: x = (/1.0,2.0,3.0/) 
         double precision, dimension(:), pointer :: p 
         p=>x ! now we lost the name for the  space allocated to 3 
         x(1:2) = p(2:3)
       end subroutine 