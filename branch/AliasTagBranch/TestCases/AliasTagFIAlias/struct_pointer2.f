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

            carPtr=>first   
            y = carPtr%labor  
            
         end subroutine


! AliasTagFIAlias Output
! =======================

! MemRefHandle => AliasTags
! =========================
! MemRefHandle                => AliasTags
! MemRefHandle(carPtr)        => (2), must
! MemRefHandle(y)             => (6), Must
! MemRefHandle(carPtr%labor)  => (7), May

! MemRefExpr => AliasTags
! =======================
! MemRefExprs                        => AliasTags
! NamedRef(carPtr)                   => (2),     Must, Local
! NamedRef(First)                    => (3,4,7), May,  Local
! NamedRef(Y)                        => (6),     Must, Local
! Deref(NamedRef(carPtr))            => (3,4,7), May,  Local
! FieldAccess(Deref(carPtr),labor)   => (7),     May,  Local


