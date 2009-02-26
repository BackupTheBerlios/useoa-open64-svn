! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!           SideEffect Analysis
!
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
!
! Note: Temporary UnnamedRef(i+1) is in the LMOD and MOD set of 
!       procedure head because t=i+1 modelling.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      

        subroutine swap(v,i,j)
          integer v(5), i, j
          integer temp

          temp = v(i)
          v(i)=v(j)
          v(j)=temp

        end subroutine swap

        program main
          integer :: a(5)
          integer :: i, j

          do i=1,4
            call swap(a,i,i+1)
          end do

        end program main


! =====================================================
! Interprocedural SideEffect Analysis
! =====================================================
!
! Procedure swap
!
! LMOD = temp, *v, *v()
! MOD  = temp, *v, *v()
! LDEF = temp
! DEF  = temp
! LUSE = temp, *v, *v(), *i, *j
! USE  = temp, *v, *v(), *i, *j
! LREF = temp, *v, *v(), *i, *j
! REF  = temp, *v, *v(), *i, *j
!
!
! Procedure main
!
! LMOD = i, Unnamed(i+1)
! MOD  = i, Unnamed(i+1), a
! LDEF = i
! DEF  = i
! LUSE = i
! USE  = i, Unnamed(i+1), a
! LREF = i
! REF  = i, Unnamed(i+1), a


