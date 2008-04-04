! =====================================================================
!                    Testing Global Variables
!
!
! 1. NamedRef(MemRefType, SymHandle, StrictlyLocal)
!             MemRefType    => def/use
!             strictlyLocal => { 0  | NonLocal to the procedure P}
!                              { 1  | Local to the procedure P}
!
! 2. FieldAccess(NamedRef(..., structure_variable,...), field)
! =================================================================

      module global
          double precision :: module_a

          ! Global structure
           type repair_bill
              real parts(20)
              real labor
              real pointer insurance
          end type repair_bill

      end module

      program main
           use global
           
           double precision :: global_c
           common /c/global_c

      end program
      
      subroutine foo
           use global
           common /c/global_c
      
           type(repair_bill), target :: first
           real k
     
           module_a = 10      ! NamedRef(def, module_a, 0)
           
           global_c=20        ! NamedRef(def, global_c, 0)

           k = first%labor    ! FieldAccess(NamedRef(use,first,1), labor)
                              ! NamedRef(def, k, 1)
      end subroutine
