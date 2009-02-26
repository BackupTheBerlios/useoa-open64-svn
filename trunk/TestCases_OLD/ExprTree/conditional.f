
      ! This example simulates
      ! 1. Loop initialization
      ! 2. Loop conditions (generally arithmatic),
      ! 3. Loop Increament
      ! 4. Logical Conditions
      
      subroutine foo
          integer :: a
          integer i
          logical :: t,f
          
          t = .TRUE.
          ! Expression t=.TRUE.
          !     ExprTree: .TRUE.
          
          f = .FALSE.
          ! Expression f=.FALSE.
          !     ExprTree: .FALSE.
          
          do i=1,10
          ! Expression: i=1
          !     ExprTree:  ConstValNode(1)
          
          ! Expression: i < = 10
          !     ExprTree:   < = (OpNode)
          !                   => i (MemRefNode)
          !                   => 10 (ConstValNode)
          
          ! Expression: i=i+1
          !     ExprTree:   + (OpNode) 
          !                   => i (MemRefNode)
          !                   => 1 (ConstValNode) 
          
             if(t .AND. f) then
             ! Expression: t .AND. f
             !    ExpreTree: .AND.  (OpNode)
             !                  => t (MemRefNode)
             !                  => f (MemRefNode)
             
                a=a+1
                ! Expression: a=a+1
                !    ExprTree: + (OpNode)
                !                => a (MemRefNode)
                !                => 1 (ConstValNode)
                
             else     
                f = .NOT. f 
                ! Expression: f=.NOT. f
                !    ExprTree: .NOT. (OpNode)
                !                 => f (MemRefNode)
                
             end if   
          end do
      end subroutine
