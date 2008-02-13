

       subroutine arraypointer() 
         double precision, target :: x 
         double precision, pointer :: p 
         p=>x  
       end subroutine 
