

      module myModule
          type repair_bill 
            real parts(20)  
            real labor     
            real pointer insurance  
          end type repair_bill 
      end module 

      subroutine foo
          use myModule
          type(repair_bill), target :: first, second          
          type(repair_bill), target :: third(4), four(4)   
          type(repair_bill), pointer:: firstPtr, secondPtr   

 
          call bar(third)
      end subroutine 


      subroutine bar(x)
          use myModule
          type(repair_bill), target :: x(4)
      end subroutine
