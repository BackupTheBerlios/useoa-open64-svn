! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Expression Trees for different tyoes of expressions in F90
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



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

c         Declaration ???
          integer::e=3




c         ArithMatic Expression          
          a=b+c
          
c                  ExprHandle: b+c
c                      ExprTree: OpNode(+)
c                                         => MemRefNode(b)
c                                         => MemRefNode(c)


          
c         precedence 
          a = (b+10)*c
         
c                  ExprHandle: (b+10)*c
c                      ExprTree: OpNode(*)
c                                         => OpNode(+)
c                                                     => MemRefNode(b)
c                                                     => ConstValNode(10)
c                                         => MemRefNode(c)
            



            
c          Intrinsic Expression
c          FIXME:
c          ConstValBasic does not accept double precision constants
c          ConstValBasic at this point, can only accept Int constants.
c          d=sin(s+10)
c           Expression: sin(s+10)
c              ExprTree: OpNode(sin)
c                           => OpNode(+)
c                                 => MemRefNode(s)
c                                 => ConstValNode(10)




          
c         Logical Expression
          t = .TRUE. .AND. f
          
c                  ExprHandle: .TRUE. .AND. f
c                      ExprTree: OpNode(.AND.)
c                                             => ConstValNode(.TRUE.)
c                                             => MemRefNode(f)




          
c         Logical Unary expression
          f = .NOT. f
          
c                  ExprHandle: .NOT. f
c                      ExprTree: OpNode(+)
c                                         => MemRefNode(f)



          
          
c         Pointer Assingment Expression
          p=>m
          
c                  ExprHandle:
c                      ExprTree: MemRefNode(m)



          
          
c         Structure pointer assignment
          x=typed_y%field1
         
c                  ExprHandle : typed_y%field1 
c                      ExprTree: MemRefNode(typed_y%field1)

          

          
          
c         String Concatenation Expression 
          STRING4 = STRING1 // STRING2 // STRING3 
          
c                  ExprHandle: STRING1 // STRING2 // STRING3
c                      ExprTree:  OpNode(//)
c                                           => MemRefNode(STRING1)
c                                           => MemRefNode(STRING2)
c                                           => MemRefNode(STRING3)
 
      end subroutine


