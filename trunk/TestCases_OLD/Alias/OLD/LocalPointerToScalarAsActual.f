
       subroutine foo() 
           double precision, pointer :: p 
           double precision, target :: x 
           p=>x 
           call bar(p) 
      end subroutine 
 
      subroutine bar(q) 
           double precision, pointer :: q 
      end subroutine 
  
