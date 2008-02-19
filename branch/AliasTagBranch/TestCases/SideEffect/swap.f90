!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Swap Function.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Definitions of different abstractions used in SideEffect Analysis
! Reference: Optimizing Compilers and Modern Architectures by Kennedy
!
! MOD(s)     = May be modified as a side effect of the call site 's'
!
! REF(s)     = May be referenced as a side effect at call site 's'
!
! USE(s)     = upward exposed use
!
! DEF(s)     = Must defined on every path through the procedure p
!
! LMOD(stmt) = May be modified at statement s locally
!
! LDEF(stmt) = Must Defined variables at statement s locally.
!              Always MustDef.
!
! LUSE(stmt) = uses in the procedure.
!              e.g. &x => x is not in the LUse set
!
! LREF(stmt) = reference at statement s locally.
!              e.g. &x => x is in the LREF set.
      
      

        subroutine swap(v,i,j)
          integer v(5), i, j
          integer temp

          temp = v(i)

!          MOD()  = (swap::temp),       LMOD() = (swap::temp)
!          REF()  = (),       LREF() = (v)
!          USE()  = (),       LUSE() = (v(),i)
!          DEF()  = (),       LDEF() = ()          
          
          v(i)=v(j)

!          MOD()  = (),       LMOD() = (v())
!          REF()  = (),       LREF() = (v)
!          USE()  = (),       LUSE() = (v(),i,j)
!          DEF()  = (),       LDEF() = ()

          v(j)=temp

!          MOD()  = (),       LMOD() = (v())
!          REF()  = (),       LREF() = (v)
!          USE()  = (),       LUSE() = (temp.j)
!          DEF()  = (),       LDEF() = ()

        end subroutine swap

        program main
          integer :: a(5)
          integer :: i, j

          do i=1,4

!          MOD() = (main::i),    LMOD() = (main::i)
!          DEF() = (main::i),    LDEF() = (main::i)          
!          REF() = (main::i),    LREF() = (main::i)
!          USE() = (main::i),    LUSE() = (main::i)


            call swap(a,i,i+1)

! MOD() = (main::a(), swap::v()),                           LMOD() = ()
! DEF() = (swap::temp),         ,                           LDEF() = (swap::temp)            
! REF() = (main::i, main::a, swap::i, swap::j, swap::v),    LREF() = (a,i)
! USE() = (main::i, main::a, swap::i, swap::j, swap::v),    LUSE() = (a,i)


          end do
        end program main


