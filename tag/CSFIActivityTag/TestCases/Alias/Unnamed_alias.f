       subroutine foo(a1)
         integer a1
         print *, a1
       end subroutine

       subroutine bar(a2)
         integer a2
         print *, a2
       end subroutine

       subroutine myproc(a1,a2)
         integer a1,a2
         print *, a2
       end subroutine
       
       program head
         integer i,j
         i=1
         j=4
         call foo(i)
         call foo(2)
         call bar(j)
         call bar(3)
       end
