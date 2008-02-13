


         module myModule
            type repair_bill
                 real parts(20)
                 real labor
                 real, pointer :: insurance
            end type repair_bill
         end module

         subroutine foo
            use myModule
            type (repair_bill), pointer :: firstPtr
            type (repair_bill), target  :: first
            real x
            firstPtr=>first
            firstPtr%labor = x
         end subroutine

