

       subroutine arraypointer(p) 
         double precision, target :: x 
         double precision, pointer :: p 
         double precision t 
         p=>x 
         p=t 
       end subroutine 
