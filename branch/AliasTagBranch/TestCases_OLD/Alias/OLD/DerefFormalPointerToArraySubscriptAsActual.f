
       subroutine arraypointer(p) 
         double precision, dimension(:), pointer :: p 
         call bar(p(1)) 
       end subroutine 
