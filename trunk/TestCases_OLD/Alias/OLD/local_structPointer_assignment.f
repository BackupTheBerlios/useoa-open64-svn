

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

 
          firstPtr=secondPtr
      end subroutine 
