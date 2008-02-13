

         module myModule
            type repair_bill
                 real parts(20)
                 real labor
                 real, pointer :: insurance
            end type repair_bill


            type vehicle 
                 integer mileage 
                 type(repair_bill) cost   
                 type(repair_bill), pointer :: costPtr 
            end type vehicle 
         end module

         subroutine foo
            use myModule
            type(vehicle) red_ferrari  
            type(vehicle) black_ford(20)    
            type(vehicle), pointer :: carPtr  

            real x(20)
            integer i,j

            x = carPtr%cost%labor
         end subroutine


