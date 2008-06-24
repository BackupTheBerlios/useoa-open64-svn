c**********************************************************************
      subroutine var(n,m,dm,um,vm,em,pm)
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
c        subroutine returns density, r-velocity,theta-velocity,
c        specific energy, total energy, and pressure at point(n,m)
c
      mm=(m-1)*ncf+(n-1)*3
      dm=u(mm+1)
      um=u(mm+2)
      vm=u(mm+3)
      em=(htot-(um**2+vm**2)/2.)/gamma
      pm=gm1*em*dm
      return 
      end
