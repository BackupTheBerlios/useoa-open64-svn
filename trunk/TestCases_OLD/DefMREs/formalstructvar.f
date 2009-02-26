


         module myModule
            type repair_bill
                 real parts(20)
                 real labor
                 real, pointer :: insurance
            end type repair_bill
         end module

         subroutine foo(first)
            use myModule
            type (repair_bill) first
            real x
            first%labor = x
         end subroutine

