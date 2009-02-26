
      subroutine foo
          integer :: a,b,c
          REAL, ALLOCATABLE :: d(:)
          
          call bar(a, b+c, 3)
          ! ExprHandle: a
          !   ExprTree: MemRefHandle(a)
          
          ! ExprHandle: b+c
          !   ExprTree: + (OpNode)
          !               => b (MemRefNode)
          !               => c (MemRefNode)
          
          ! ExprHandle: &(b+c)
          !   ExprTree: &(b+c) (MemRefNode)
          
          ! ExprHandle: 3
          !   ExprTree:   => 3 (ConstValNode)
          
          ! ExprHandle: &3
          !   ExprTree: &3     (MemRefNode)
          
          
          allocate(d(1:3))
          ! ExprHandle: d(1:3)
          !   ExprTree: d(1:3) (MemRefNode)
          
          
          t = func(a)
          ! ExprHandle: func(a)
          !   ExprTree: func(a) (ConstValNode)
          
          ! ExprHandle: &a
          !   ExprTree: &a (MemRefNode)
          
          
          a = ((a*10)+func(b-c))
          ! ExprHandle: ((a*10)+func(b-c))
          !   ExprTree: + (OpNode)
          !               => * (OpNode)
          !                    => a (MemRefNode)
          !                    => 10 (ConstValNode)
          !               => func(b-c) (CallNode)

          ! ExprHandle: b-c
          !   ExprTree: - (OpNode)
          !               => b (MemRefNode)
          !               => c (MemRefNode)

          ! ExprHandle: &(b-c)
          !   ExprTree: &(b-c) (MemRefNode)
          
      end subroutine

      subroutine bar(p,q,r)
          double precision :: p,q,r
      end subroutine

      function func(n) result(res)
          double precision res
          double precision n
             res = n
             ! ExprHandle: res = n
             !   ExprTree: n (MemRefNode)
             
      end function func

