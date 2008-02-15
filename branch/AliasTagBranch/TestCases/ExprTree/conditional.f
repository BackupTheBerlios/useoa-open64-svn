
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
          !     ExprTree:  ConstValNode(.TRUE.)
          
          f = .FALSE.
          ! Expression f=.FALSE.
          !     ExprTree:  ConstValNode(.FALSE.)
          
          do i=1,10
          ! Expression: i=1
          !     ExprTree:  ConstValNode(1)
          
          ! Expression: i < = 10
          !     ExprTree: OpNode(<=)
          !                       => MemRefNode(i)
          !                       => ConstValNode(10)
          
          ! Expression: i=i+1
          !     ExprTree: OpNode(+)
          !                       => MemRefNode(i)
          !                       => ConstValNode(1)
          
             if(t .AND. f) then
             ! Expression: t .AND. f
             !    ExpreTree: OpNode(.AND.)
             !                        => MemRefNode(t)
             !                        => MemRefNode(f)
             
                a=a+1
                ! Expression: a=a+1
                !    ExprTree: OpNode(+)
                !                     => MemRefNode(a)
                !                     => ConstValNode(1)
                
             else     
                f = .NOT. f 
                ! Expression: f=.NOT. f
                !    ExprTree: OpNode(.NOT.)
                !                     => MemRefNode(f)
                
             end if   
          end do
      end subroutine
