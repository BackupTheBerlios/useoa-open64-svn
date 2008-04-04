! ================================================================
!                 Testing Local Variables
! 
!
! There are 2 types of Memory Reference Expressions below:
!
! 1. NamedRef(MemRefType, SymHandle, StrictlyLocal)
!             MemRefType    => def/use
!             strictlyLocal => { 0  | NonLocal to the procedure P}
!                              { 1  | Local to the procedure P}
!
! 2. Deref((MemRefType, SymHandle, StrictlyLocal), numberOfDeref)
!              numberOfDeref => Number of Dereference Wrappers
!
! ================================================================

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
          
          ! ======= Program ========

          ! Scalar
          a=10           ! NamedRef(def, a, 1)

 
          ! Array 
          b(1)=1         ! SubSetRef(NamedRef(def, b, 1), )


          ! Pointer to local
          p=>a           ! NamedRef(def, p, 1)
                         ! NamedRef(use, a, 1) 

          a=p            ! NamedRef(def, a, 1)
                         ! Deref(NamedRef(use, p, 1), 1)


          ! Pointer to formal
          p=>f           ! NamedRef(def, p, 1)
                         ! NamedRef(use, f, 1)

          f=p            ! NamedRef(def, f, 0)
                         ! Deref(NamedRef(use, p, 1), 1)
          

         ! Structure variable
         first=second    ! NamedRef(use, second, 1)
                         ! NamedRef(def, first, 1)

      end subroutine
