! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. UnnamedRef for the actual parameter as expression
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status : Should UnnamedRef has multiple virtual Addresses ?
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Example :
! ==========


        subroutine foo
             double precision :: a,b
             call bar(a+b)    
        end subroutine
 
        subroutine bar(c)
             double precision :: c
             c=5
        end subroutine



! Analysis :
! =========


          ! ========= AliasTagFIAlias Results =========
          !
          !   [ MemRefExpr  =>  AliasTag ]
          !   ============================
          ! NamedRef("a")           => (1, Must)
          ! NamedRef("b")           => (2, Must)
          ! UnnamedRef("a+b")       => (3, May)
          ! NamedRef("c")           => (4, Must)
          !
          !
          !   [ MemRefHandle  =>  AliasTag]
          !   =============================
          ! MemRefHandle("a")       => (1, Must)
          ! MemRefHandle("b")       => (2, Must)
          ! MemRefHandle("a+b")     => (3, May)
          ! MemRefHandle("c")       => (4, Must)




          ! ======== AliasMapXAIF Results ================
          !
          !   [  MemRefHandle => SetId ]
          !   ===========================
          ! MemRefHandle("a")        => SetId(1)
          ! MemRefHandle("b")        => SetId(2)
          ! MemRefHandle("a+b")      => SetId(3)
          ! MemRefHandle("c")        => SetId(4) 
          !
          !
          !   [  SetId  =>  Virtual Address ]
          ! ==================================
          ! SetId(1) => { LocTuple(1:1, Must) }
          ! SetId(2) => { LocTuple(2:2, Must) }
          ! SetId(3) => { LocTuple(3:3, May) }
          ! SetId(4) => { LocTuple(4:4, Must)}

         
