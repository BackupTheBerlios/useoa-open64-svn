

       subroutine arraypointer(p) 
         double precision, dimension(3), target :: x = (/1.0,2.0,3.0/) 
         double precision, dimension(:), pointer :: p 
         p=>x  
       end subroutine 
