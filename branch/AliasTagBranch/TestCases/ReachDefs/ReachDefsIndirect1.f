! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ReachDefs
!                   ==========
! Features:
!           1. ReachDefs through pointer dereference.
!              - The exact location that pointer is pointing at is
!                unknwon statically.
!
! Testing :
!          [X] ReachDefs[Stmt]
!
! Status /Issues:  
!
! Note:  1. Intraprocedural ReachDefs as of today April 9th 2008.
!        2. StmtHandle(0) indicate may-ReachDefs. Except strictlyLocal
!           all other AliasSets are May and therefore, we get 
!           StmtHandle(0). [explaining some cases of StmtHandle(0)]
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Example:
! --------
      
       subroutine foo
            double precision, pointer :: p
            double precision, target  :: t
            double precision, target  :: a
            double precision :: m, n
            integer i

            if( i < 5 ) then    
                p=>t           
            else   
                p=>a           
            end if

            p=5                 ! (p=>t), (p=>a)
            m=t                 ! (p=>t), (p=>a), (p=5)
            a=3                 ! (p=>t), (p=>a), (p=5), (m=t)
            n=a                 ! (p=>t), (p=>a), (p=5),
                                ! (m=t), (a=3)

! Exit Reaching Definitions : (p=>t), (p=>a), (p=5), (m=t), (a=3), (n=a)


       end subroutine




