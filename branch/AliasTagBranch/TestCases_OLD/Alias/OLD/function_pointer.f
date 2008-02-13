



        recursive function pntmin(ain) result (themin) ! return a pointer
!         use numz
!         use galapagos
!         use sort_mod ! contains the < operator for thefit type
         implicit none
         type (thefit),pointer:: themin,t1,t2
         integer n,right
         type (thefit) ,dimension(:),target :: ain
         n=size(ain)
         if(n == 1)then
             themin=>ain(1) !this is how we do pointer assignment
             return
         else
             right=n/2
             t1=>pntmin(ain(1:right))
             t2=>pntmin(ain(right+1:n))
             if(t1 < t2)then; themin=>t1; else; themin=>t2; endif
         endif
        end function

