
       subroutine arraypointer() 
         double precision, dimension(:), pointer :: p 
         double precision, dimension(5), target :: x 
         p=>x 
         call bar(p) 
       end subroutine 

