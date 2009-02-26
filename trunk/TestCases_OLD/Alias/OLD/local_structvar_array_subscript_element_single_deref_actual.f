

         module myModule
            type repair_bill
                 real parts(20)
                 real labor
                 real, pointer :: insurance
            end type repair_bill
         end module

         subroutine foo
            use myModule
            type(repair_bill) red_ferrari 
            type(repair_bill) black_ford(20) 

            integer i
            call bar(red_ferrari%parts(i))
         end subroutine

         subroutine bar(x)
            real x
         end subroutine
         


