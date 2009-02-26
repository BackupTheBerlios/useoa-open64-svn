! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Below Different types of expressions are used to show the the
! ICFGDep Results.  Results are showed using following abstractions
!
! Note: No Defs for pointer assignment because we only consider
!       Differentiable Memory References.
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     

      module myTypeModule

        implicit none
        private
        public :: myType

        type myType
        sequence
          double precision :: field1
          end type myType
      end module

      
      subroutine foo
          use myTypeModule
          integer :: a,b,c,k
          double precision, dimension(5) :: array,brray
          logical :: t,f
          double precision, pointer :: p
          double precision, target  :: m
          double precision:: d,s,x
          CHARACTER STRING1*20, STRING2*20, STRING3*20, STRING4*20
          type(myType) :: typed_y

          ! 1. Declaration ???
          integer::e=3

          a=b+c
          a=a+b
          array(k) = brray(k) + 10
          typed_y%field1=x




          ! 2. Simple ArithMatic Expression          

          a=b+c

c                        mUses           => mDefs
c                        ==============================
c                        [2: foo::b]     => [1: foo::a]
c                        [3: foo::c]     => [1: foo::a]       
c
c                        ImplicitRemoves:
c                        ================
c                        [1: foo::a ]









          ! 3. ArithMatic Expression where LHS and RHS are same

          ! The reflexive pairs (eg. <a,a>) are implicitly assumed
          ! to be dependent. 


          a=a+b

c                        mUses           => mDefs
c                        ==============================
c                        [2: foo::b]     => [1: foo::a]
c
c                        ImplicitRemoves:
c                        ================









          ! 4. Arrays, do not get killed. Thus, no ImpliciteRemoves.
          
          array(k) = brray(k) + 10

c                        mUses                => mDefs
c                        ==============================
c                        [4: foo::brray(),    => [5: foo::array(),
c                            foo::brray]                  array]
c
c                        ImplicitRemoves:
c                        ================
c









          ! 5. FieldAccess do not get killed. Thus, no ImpliciteRemoved

          typed_y%field1=x


c                        mUses           => mDefs
c                        ==============================
c                        [6: foo::x]     => [7: foo::typed_y%field1,
c                                               foo::typed_y]
c    
c                        ImplicitRemoves:
c                        ================
c 






            
          ! 6. Intrinsic Expression. Used as an operator not callsites
          
          d=sin(s+10)


c                        mUses          => mDefs
c                        ==============================
c                        [8: foo::s]    => [9: foo::d]
c
c                        ImplicitRemoves:
c                        ================
c                        [9: foo::d]
c






          

          ! 7. Pointer Assingment Expression

          ! Pointer Statement is model as p=&a. Address MREs are not used MemRefs
          ! Thus No ICFGDep information.
          
          p=>m







          ! 8. String Concatenation Expression 
          
          STRING4 = STRING1 // STRING2 // STRING3 

c                        mUses                => mDefs
c                        ==============================
c                        [10: foo::STRING1,    => [13: foo::STRING4,
c                             foo::STRING1()]          foo::STRING4()]
c
c                        [11: foo::STRING2,    => [13: foo::STRING4,
c                             foo::STRING2()]          foo::STRING4()]
c
c                        [12: foo::STRING3,    => [13: foo::STRING4,
c                             foo::STRING3()]          foo::STRING4()]
c
c                        ImplicitRemoves:
c                        ================
c




      end subroutine


