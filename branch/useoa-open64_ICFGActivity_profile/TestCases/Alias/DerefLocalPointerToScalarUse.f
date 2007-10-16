


      subroutine arraypointer() 
         double precision, target :: x 
         double precision, pointer :: p 
         double precision t 
         p=>x 
         t=p 
       end subroutine 
