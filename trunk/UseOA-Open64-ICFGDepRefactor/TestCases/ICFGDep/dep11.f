


         module myModule
            type repair_bill
                 real parts(20)
                 real labor
                 real, pointer :: insurance
            end type repair_bill
         end module

         subroutine foo(first)
            use myModule
            type (repair_bill), target :: first
            type (repair_bill), pointer :: firstPtr
            real x
            firstPtr=>first
            x = firstPtr%labor
         end subroutine

 
