


         module myModule
            type repair_bill
                 real parts(20)
                 real labor
                 real, pointer :: insurance
            end type repair_bill
         end module

         subroutine foo
            use myModule
            type (repair_bill), target :: first
            type (repair_bill) for_array(6)
            type (repair_bill), pointer :: carPtr

            real, target :: x
            real :: y
            carPtr=>first
            y = carPtr%labor
         end subroutine

