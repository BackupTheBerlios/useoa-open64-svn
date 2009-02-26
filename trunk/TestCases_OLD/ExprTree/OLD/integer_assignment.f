
      subroutine foo
         integer a,b

         ! AssignmentPair
         !       | - MemRefHandle a
         !       | - ExprHandle b

         a = b 
      end subroutine
