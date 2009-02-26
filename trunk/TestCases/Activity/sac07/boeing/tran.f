c**********************************************************************
      subroutine tran(n,m,xr,xth,yr,yth,ajac)
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
c        subroutine determines transformation parameters xr,xth,
c        yr,yth and jacobian ajac at mesh point (n,m)
c
c      sm=s(m)
      call subs(m,dummy)
      sm = dummy
      bm=b(m)
      sb=sm-bm
c      sthm=sth(m)
      call substh(m,dummy)
      sthm = dummy
      bthm=bth(m)
      call rth(n,m,r,theta)
      xr=1./sb
      xth=((r-sm)*bthm-(r-bm)*sthm)/sb**2
      yr=0.
      yth=1./theta1
      ajac=theta1*sb
      return 
      end
