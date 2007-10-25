       subroutine myproc(a1,a2)
         integer a1,a2
         print *, a2
       end subroutine
       
       program head(i,j)
         integer i,j
         call myproc(i,j)
         call myproc(i+j,2)
       end
