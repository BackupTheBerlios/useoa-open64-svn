


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
          integer a,b,c
          logical t,f
          double precision, pointer :: p
          double precision, target  :: m
          double precision:: d,s
          CHARACTER STRING1*20, STRING2*20, STRING3*20, STRING4*20
          double precision  x
          type(myType) :: typed_y

          ! Declaration ???
          integer::e=3

          ! ArithMatic Expression          
          a=b+c
          ! Expression: b+c
          !    ExprTree: + (OpNode)
          !                => b (MemRefNode)
          !                => c (MemRefNode)

          ! precedence 
          a = (b+10)*c
          ! Expression: (b+10)*c
          !    ExprTree: * (OpNode)
          !                => + (OpNode)
          !                     => b (MemRefNode)
          !                     => 10 (ConstValNode)
          !                => c (MemRefNode)
            

          ! Intrinsic Expression
          ! FIXME:
          ! ConstValBasic does not accept double precision constants
          ! ConstValBasic at this point, can only accept Int constants.
          !d=sin(s+10)
          ! Expression: sin(s+10)
          !    ExprTree: sin (OpNode)
          !                => + (OpNode)
          !                   => s (MemRefNode)
          !                   => 10 (ConstNode) 

          ! Logical Expression
          t = .TRUE. .AND. f
          ! Expression: .TRUE. .AND. f
          !    ExprTree: .AND. (OpNode)
          !                => .TRUE. (ConstNode)
          !                => f      (MemRefNode)  

          ! Logical Unary expression
          f = .NOT. f

          ! Pointer Assingment Expression
          p=>m
          ! Expression:
          !    ExprTree: m (MemRefNode)

          ! Structure pointer assignment
          typed_y%field1=x

          ! String Concatenation Expression 
          STRING4 = STRING1 // STRING2 // STRING3 
          ! Expression: STRING1 // STRING2 // STRING3
          !    ExprTree:  // (OpNode)
          !                  => STRING1 (MemRefNode)
          !                  => STRING2 (MemRefNode)
          !                  => STRING3 (MemRefNode)
 
      end subroutine


