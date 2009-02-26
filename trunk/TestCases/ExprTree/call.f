! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   
!  Expression Trees for different types of Call Statements
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     
      
      subroutine foo
          integer :: a,b,c
          REAL, ALLOCATABLE :: d(:)
     
          
c         Subroutine Call
          call bar(a, b+c, 3)
          
c                     ExprHandle: &a
c                         ExprTree: MemRefNode(&a)
c        
c                     ExprHandle: &(b+c)
c                         ExprTree: MemRefNode(&(b+c))
c
c                     ExprHandle: &3
c                         ExprTree: MemRefNode(&3)
c
c                     ExprHandle: b+c
c                         ExprTree: OpNode( + )
c                                             => MemRefNode (b)
c                                             => MemRefNode (c)
c
c                     ExprHandle: 3
c                         ExprTree: ConstValNode(3)
         

         

         
c         allocate
          allocate(d(1:3))

c                     ExprHandle: d(1:3)
c                         ExprTree: MemRefNode(d(1:3))
         

         

         

c         Function Calls
          t = func(a)

c                     ExprHandle: func(a)
c                         ExprTree: CallNode(func(a))
          
c                     ExprHandle: &a
c                         ExprTree: MemRefNode(&a)
         


         


          ! Function calls in the expression
          a = ((a*10)+func(b-c))
          
c                     ExprHandle: ((a*10)+func(b-c))
c                         ExprTree: OpNode(+)
c                                            => OpNode(*)
c                                                        => MemRefNode(a)
c                                                        => ConstValNode(10)
c
c                                            => CallNode(func(b-c))
c
c
c                     ExprHandle: b-c
c                         ExprTree: OpNode(-)
c                                            => MemRefNode(b)
c                                            => MemRefNode(c)
c
c                     ExprHandle: &(b-c)
c                         ExprTree: MemRefNode(&(b-c))
         
         


      end subroutine

      subroutine bar(p,q,r)
          double precision :: p,q,r
      end subroutine

      function func(n) result(res)
          double precision res
          double precision n
             res = n
             
c                     ExprHandle: res = n
c                         ExprTree: MemRefNode(n)
             
      end function func

