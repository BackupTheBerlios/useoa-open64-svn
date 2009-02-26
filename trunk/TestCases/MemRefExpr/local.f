! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                       MemRefExpr Testcase
!                       ====================
! 
! Features:  Testing Local scalars and pointers, formal scalars
!
!            1. NamedRef(MemRefType, SymHandle, StrictlyLocal)
!               MemRefType    => def/use
!               strictlyLocal => { 0  | NonLocal to the procedure P}
!                                { 1  | Local to the procedure P}
!
!
! Testing :
!          [X] MemRefExpr Type (Def/Use)
!          [X] StrictlyLocal = 1
!
! Status : No Issues
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      subroutine foo(f)

          ! ======== Declarations ========
          double precision, target :: a
          double precision, dimension(5) :: b
          double precision, pointer :: p
          double precision, target :: f

          type repair_bill  
              real parts(20)                    
              real labor                       
              real pointer insurance            
          end type repair_bill  

          type(repair_bill), target :: first, second
          


          ! ===== Scalar =====

          a=10           ! NamedRef(def, foo::a, 1)

 
          ! ===== Array =====

          b(1)=1         ! SubSetRef(NamedRef(def, foo::b, 1), )


          ! ===== Pointer to local =====

          p=>a           ! NamedRef(def, foo::p, 1)
                         ! NamedRef(use, foo::a, 1) 

          a=p            ! NamedRef(def, foo::a, 1)
                         ! Deref(NamedRef(use, foo::p, 1), 1)


          ! ===== Pointer to formal =====

          p=>f           ! NamedRef(def, foo::p, 1)
                         ! NamedRef(use, dummy::f, 1), because f is
                         ! fomal, therefore &, * cancel out.


          f=p            ! NamedRef(def, foo::f, 0)
                         ! Deref(NamedRef(use, foo::p, 1), 1)
          

         ! ===== Structure variable =====

         first=second    ! NamedRef(use, foo::second, 1)
                         ! NamedRef(def, foo::first, 1)


      end subroutine
