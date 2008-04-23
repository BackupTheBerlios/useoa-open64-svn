! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Aliasing for structure member variable of type pointer.
!
! AliasPairs : (*(first%insurance), x)
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Problem: April 22nd 2008
! First is strictlyLocal NamedRef but gets May ?
! =====================================


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

            first%insurance=>x  
            y = first%insurance 

         end subroutine



! Results of AliasTagFIAlias Analysis
! ===================================

! MemRefHandle                  => AliasTags
! MemRefHandle(first%insurance) => (7),   Must=0
! MemRefHandle(y)               => (6),   Must=1
! MemRefHandle(first%insurance) => (3,4), Must=0

! MemRefExprs                                    => AliasTags
! NamedRef(first)                                => (1,2,7), Must=0, Local=1
! NamedRef(x)                                    => (4),     Must=1, Local=1
! NamedRef(y)                                    => (6),     Must=1, Local=1
! Deref(FieldAccess(NamedRef((first)insurance))) => (3,4),   Must=0, Local=1
! FieldAccess(NamedRef((first)insurance))        => (7),     Must=0, Local=1 



