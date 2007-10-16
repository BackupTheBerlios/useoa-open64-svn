      subroutine fnc(x,fn)
      integer m,np,nq
      parameter (np=8,m=2,nq=2)
      integer i
      double precision x(np+m)
      double precision fn(nq)
      double precision beta(np),xplusd(m)

      do 10 i=1,np
         beta(i) = x(i)
   10 continue
      do 20 i=1,m
         xplusd(i) = x(np+i)
   20 continue

c  compute first of multi-response observations

	   fn(1) =   beta(1)
     +                    + beta(2)*xplusd(1)
     +                    + beta(3)*xplusd(2)
     +                    + beta(4)*xplusd(1)*xplusd(2)

c  compute second of multi-response observations

	   fn(2) =   beta(5)
     +                    + beta(6)*xplusd(1)
     +                    + beta(7)*xplusd(2)
     +                    + beta(8)*xplusd(1)*xplusd(2)

      return
      end
