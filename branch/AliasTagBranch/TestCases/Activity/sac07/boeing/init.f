c**********************************************************************
      subroutine init
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
c         initialize flowfield from tape 11,
c         and define needed factors
c
      namelist/in/alpha,itmax,sstol,nprint,delta,kprint
     1 ,icont,xend,xincr
      namelist/outv/xmach,theta1,gamma,mc,nc,qinc,dinc,pinc,
     1 einc,htot,swall,dstag,dx,dy
      read(5,in)
      write(6,in)
c         get data from tape 11
      read(11,*)theta1,gamma,mc,nc,qinc,dinc,pinc,einc,htot,swall,dstag
      ncf=3*nc+1
      mcncf=mc*ncf
      kblk=ncf
      kblk2=2*kblk
      kblk3=3*kblk
      ksize=5*kblk
      kdiag=kblk3+1
      read(11,*)(u(i),i=1,mcncf)
      read(11,*)(b(i),i=1,mc)
      read(11,*)(bth(i),i=1,mc)
      nc1=nc-1
      mc1=mc-1
      mc2=mc-2
      mc3=mc-3
      dx=1./nc1
      dy=1./mc1
      ddx=nc1
      ddy=mc1
      ddx2=.5*ddx
      ddy2=.5*ddy
      dtheta=theta1/mc1
      gm1=gamma-1.
      ainc=dsqrt(gamma*gm1*einc)
      xmach=qinc/ainc
      write(6,outv)
c     call out(0)
      return
      end
