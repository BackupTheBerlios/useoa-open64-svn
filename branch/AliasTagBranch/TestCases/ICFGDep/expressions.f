! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Below Different types of expressions are used to show the the
! ICFGDep Results.  Results are showed using following abstractions
!
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
          integer :: a,b,c
          double precision, dimension(5) :: array,brray
          integer k
          logical :: t,f
          double precision, pointer :: p
          double precision, target  :: m
          double precision:: d,s
          CHARACTER STRING1*20, STRING2*20, STRING3*20, STRING4*20
          double precision  x
          type(myType) :: typed_y

          ! 1. Declaration ???
          integer::e=3

          ! =========================================================


          ! 2. Simple ArithMatic Expression          

          a=b+c

          ! mUses       => mDefs 
          ! NamedRef(b) => NamedRef(a)
          ! NamedRef(c) => NamedRef(a) 

          ! mDefs       => mUses
          ! NamedRef(a) => NamedRef(b)
          !             => NamedRef(c)

          ! ImplicitRemoves: NamedRef(a)

          
          
          ! ============================================================



          ! 3. ArithMatic Expression where LHS and RHS are same

          ! The reflexive pairs (eg. <a,a>) are implicitly assumed
          ! to be dependent. For the example below, 'a' still depends 
          ! on itself because it is on the rhs and lhs.


          a=a+b

          ! mUses       => mDefs 
          ! NamedRef(b) => NamedRef(a)

          ! mDefs       => mUses 
          ! NamedRef(a) => NamedRef(b)

          ! ImplicitRemoves: {}


          
          ! ============================================================


          ! 4. Arrays, do not get killed. Thus, no ImpliciteRemoves.
          
          array(k) = brray(k) + 10

          ! mUses                      => mDefs
          ! SubSetRef(NamedRef(brray)) => SubSetRef(NamedRef(array))
          
          ! mDefs                      => mUses
          ! SubSetRef(NamedRef(array)) => SubSetRef(NamedRef(brray))

          ! mImplicitRemoves : ()



          ! ============================================================


          ! 5. FieldAccess do not get killed. Thus, no ImpliciteRemoved

          typed_y%field1=x

          ! mUses       => mDefs  
          ! NamedRef(x) => FieldAccess(NamedRef(y),field1)

          ! mDefs                           => mUse   
          ! FieldAccess(NamedRef(y),field1) => NamedRef(x)

          ! ImplicitRemoves :



          ! ==============================================================

            
          ! 6. Intrinsic Expression. Used as an operator not callsites
          
          d=sin(s+10)

          ! mUses       => mDefs   
          ! NamedRef(s) => NamedRef(d)

          ! mDefs       => mUses  
          ! NamedRef(d) => NamedRef(s)

          ! mImplicitRemoves : NamedRef(d)

          
          ! ==============================================================
          

          ! 7. Pointer Assingment Expression

          ! Pointer Statement is model as p=&a. Address MREs are not used MemRefs
          ! Thus No ICFGDep information.
          
          p=>m

          ! mUses => mDefs   : {}
          
          ! mDefs => mUses   : {}

          ! ImplicitRemoves  : {}


          ! ============================================================


          ! 8. String Concatenation Expression 
          
          STRING4 = STRING1 // STRING2 // STRING3 
 
          ! mUses                        => mDefs
          ! SubSetRef(NamedRef(STRING1)) => SubSetRef(NamedRef(STRING4))
          ! SubSetRef(NamedRef(STRING2)) => SubSetRef(NamedRef(STRING4))
          ! SubSetRef(NamedRef(STRING3)) => SubSetRef(NamedRef(STRING4))

          ! mDefs             => mUses
          ! NamedRef(STRING4) => SubSetRef(NamedRef(STRING1)),
          !                   => SubSetRef(NamedRef(STRING2)),
          !                   => SubSetRef(NamedRef(STRING3))
 
          ! ImplicitRemoves  : SubSetRef(NamedRef(STRING4))

          

          ! =========================================================
          

      end subroutine


