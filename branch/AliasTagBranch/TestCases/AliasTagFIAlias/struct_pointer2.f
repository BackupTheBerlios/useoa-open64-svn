! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Aliasing in the presence of pointer to the structure
!
! AliasPairs : (*carPtr, first)
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


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
            carPtr=>first      ! AliasTag("first")  => ({1,2},MUST)
                               ! AliasTag("carPtr") => (3,MUST)
             
            
            y = carPtr%labor   ! AliasTag("(*carPtr)%labor") => (2,MAY)
                               ! AliasTag("*carPtr")         => ({1,2},MUST)
                               ! AliasTag("y")               => (4,MUST)
            
         end subroutine

