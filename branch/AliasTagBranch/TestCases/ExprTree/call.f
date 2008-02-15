
      subroutine foo
          integer :: a,b,c
          REAL, ALLOCATABLE :: d(:)
          
          call bar(a, b+c, 3)
          ! ExprHandle: &a
          !   ExprTree: MemRefNode(&a)
         
          ! ExprHandle: &(b+c)
          !   ExprTree: MemRefNode(&(b+c))

          ! ExprHandle: &3
          !   ExprTree: MemRefNode(&3)

          ! ExprHandle: b+c
          !   ExprTree: OpNode( + )
          !                 => MemRefNode (b)
          !                 => MemRefNode (c)
          
          ! ExprHandle: 3
          !   ExprTree: ConstValNode(3)
          
          allocate(d(1:3))
          ! ExprHandle: d(1:3)
          !   ExprTree: MemRefNode(d(1:3))
          
          
          t = func(a)
          ! ExprHandle: func(a)
          !   ExprTree: CallNode(func(a))
          
          ! ExprHandle: &a
          !   ExprTree: MemRefNode(&a)
          
          
          a = ((a*10)+func(b-c))
          ! ExprHandle: ((a*10)+func(b-c))
          !   ExprTree: OpNode(+)
          !                 => OpNode(*)
          !                        => MemRefNode(a)
          !                        => ConstValNode(10)
          !                 => CallNode(func(b-c))

          ! ExprHandle: b-c
          !   ExprTree: OpNode(-)
          !                 => MemRefNode(b)
          !                 => MemRefNode(c)

          ! ExprHandle: &(b-c)
          !   ExprTree: MemRefNode(&(b-c))
          
      end subroutine

      subroutine bar(p,q,r)
          double precision :: p,q,r
      end subroutine

      function func(n) result(res)
          double precision res
          double precision n
             res = n
             ! ExprHandle: res = n
             !   ExprTree: MemRefNode(n)
             
      end function func

