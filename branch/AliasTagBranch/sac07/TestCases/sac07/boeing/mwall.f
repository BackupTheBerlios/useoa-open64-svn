c**********************************************************************
      subroutine mwall(m,mf,mb)
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
c        determine y differencing indices for wall and shock
c
      if(m .eq. mc)go to 10
      if(m .eq. 1)go to 5
      mf=m+1 
      mb=m-1
      return
10    mf=mc 
      mb=mc-1 
      return
5     mf=2 
      mb=1 
      return
      end