! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Aliasing for structure member variable of type pointer.
!
! AliasPairs : (*(first%insurance), x)
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
            type (repair_bill) first
            type (repair_bill) for_array(6)
            type (repair_bill), pointer :: carPtr

            real, target :: x
            real :: y
            first%insurance=>x     ! AliasTag("first") => ({1,2},MUST)  
                                   ! AliasTag("x")     => (3,MUST)

            y = first%insurance    ! AliasTag("first%insurance")    => (2,MAY)
                                   ! AliasTag("*(first%insurance)") => (3,MUST)
                                   ! AliasTag("y")                  => (4,MUST)

         end subroutine

