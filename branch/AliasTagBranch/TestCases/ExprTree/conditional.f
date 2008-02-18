! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Expression Trees for Loops and Conditionals
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      
      subroutine foo
          integer :: a
          integer i
          logical :: t,f
          
          t = .TRUE.

c                       ExprHandle: t=.TRUE.
c                           ExprTree:  ConstValNode(.TRUE.)
          
          f = .FALSE.

c                       ExprHandle f=.FALSE.
c                           ExprTree:  ConstValNode(.FALSE.)
          
          do i=1,10

c                       ExprHandle: i=1
c                           ExprTree:  ConstValNode(1)
c          
c                       ExprHandle: i < = 10
c                           ExprTree: OpNode( < = )
c                                                  => MemRefNode(i)
c                                                  => ConstValNode(10)
c          
c                       ExprHandle: i=i+1
c                           ExprTree: OpNode(+)
c                                              => MemRefNode(i)
c                                              => ConstValNode(1)
c          
             if(t .AND. f) then

c                       ExprHandle: t .AND. f
c                           ExpreTree: OpNode(.AND.)
c                                                   => MemRefNode(t)
c                                                   => MemRefNode(f)
             
                a=a+1

c                       ExprHandle: a=a+1
c                           ExprTree: OpNode(+)
c                                               => MemRefNode(a)
c                                               => ConstValNode(1)
                
             else     
                f = .NOT. f 

c                       ExprHandle: f=.NOT. f
c                           ExprTree: OpNode(.NOT.)
c                                                  => MemRefNode(f)

                
             end if   
          end do
      end subroutine
