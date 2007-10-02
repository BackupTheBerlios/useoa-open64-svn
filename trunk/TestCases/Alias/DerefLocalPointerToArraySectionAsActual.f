
       subroutine arraypointer() 
         double precision, dimension(:), pointer :: p 
         call bar(p(1:2)) 
       end subroutine 


