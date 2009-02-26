
       subroutine arraypointer() 
         double precision, dimension(:), pointer :: p 
         double precision, dimension(3), target :: x 
         p=>x 
         call bar(p(1)) 
       end subroutine 
