

         module myModule
            type repair_bill
                 real parts(20)
                 real labor
                 real, pointer :: insurance
            end type repair_bill
         end module

         subroutine foo
            use myModule
            type (repair_bill) first
            type (repair_bill) for_array(6)
            type (repair_bill), pointer :: carPtr

            real x(20)
            integer i

            x = first%parts
         end subroutine


