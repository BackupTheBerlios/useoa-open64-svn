! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Trying to Tests Pointer assignment as Reaching definition
! to the program point where pointer is derefernced.
! 
! Test: [X] pointer Use at a=*p
!       [X] pointer Assignment Reaching Definition at a=*p
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        subroutine foo
             double precision, pointer :: p
             double precision, target :: t
             double precision :: a
             p=>t 
             a=p            ! Stmt(p=>t), StmtHandle(0)
             return         ! Stmt(p=>t), StmtHandle(0)
        end subroutine
