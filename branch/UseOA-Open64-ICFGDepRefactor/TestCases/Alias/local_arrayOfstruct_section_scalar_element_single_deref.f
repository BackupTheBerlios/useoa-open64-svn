

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
            type(repair_bill), pointer :: carPtr  

            real x(20)
            integer i,j
            x = black_ford(i:j)%labor
         end subroutine


