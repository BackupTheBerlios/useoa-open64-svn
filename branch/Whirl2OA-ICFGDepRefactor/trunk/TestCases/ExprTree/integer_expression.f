
      subroutine foo
         integer a,b,c

         ! AssignmentPair
         !       | - MemRefHandle a
         !       | - ExprHandle b

         a = b + c
      end subroutine
