c**********************************************************************
      subroutine history(ittot)
      implicit double precision(a-h,o-z)
      common //
     1 u(190),eq(190),eqold(190),a(190,95),eqplus(190),
     2 b(10),bth(10),ff(3,6,10),gg(3,6,10),hh(3,6,10),
     3 mc,nc,dx,dy,ddx,ddy,ddx2,ddy2,mc1,nc1,mc2,mc3,
     4 theta1,gamma,gm1,xmach,ncf,mcncf,
     5 qinc,dinc,pinc,einc,ainc,
     6 htot,swall,dstag,emaxs(200),eqmaxs(200),delta,
     7 kdiag,kblk,ksize,kblk2,kblk3
     8 ,itmax,sstol,alpha,nprint,emax,eqmax
     9 ,kprint,dtheta,icont,xend,xincr
c
c         print out iteration history
c
      write(6,100)
      write(6,101)
      do 200 i=1,ittot
      eratio=0.
      if(i .ge. 2)eratio=emaxs(i)/emaxs(i-1)**2
200   write(6,102)i,emaxs(i),eqmaxs(i),eratio
100   format(1h1," iteration history",5(/))
101   format(" it",6x,"error max",6x,"eqn error",6x,
     1 "erconv ratio",//)
102   format(i5,1p3e15.4,/)
      return
      end
