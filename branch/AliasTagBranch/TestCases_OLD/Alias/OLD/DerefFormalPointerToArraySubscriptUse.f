


       subroutine arraypointer(p) 
         double precision, dimension(3), target :: x = (/1.0,2.0,3.0/) 
         double precision, dimension(:), pointer :: p 
         double precision t 
         integer j 
         p=>x ! now we lost the name for the  space allocated to 3 
         t=p(j) 
       end subroutine 

