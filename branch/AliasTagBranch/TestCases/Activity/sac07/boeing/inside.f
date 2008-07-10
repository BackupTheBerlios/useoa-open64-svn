c**********************************************************************
      subroutine inside
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
c
c         evaluates interior equations
c           centered differences except outflow m=mc
c           and symmetry line m=1
c
      do 10 n=1,nc
      iflag=0
      do 20 m=2,mc
      call eval(n,m,4,iflag,ff(1,n,m),gg(1,n,m),hh(1,n,m))
20    continue
      iflag=1
      m=1
      call eval(n,m,4,iflag,ff(1,n,m),gg(1,n,m),hh(1,n,m))
10    continue
      do 30 m=1,mc
      call mmm(m,mf,mb)
      deltay=(mf-mb)*dy
      do 40 n=2,nc1
      call nnn(n,nf,nb)
      deltax=(nf-nb)*dx
      do 50 i=1,3
      index=(m-1)*ncf+3*(n-1)+i
      eq(index)=(ff(i,nf,m)-ff(i,nb,m))/deltax+hh(i,n,m)
      if(m .eq. mc)go to 45
      if(m .ne. 1)eq(index)=eq(index)+(gg(i,n,mf)-gg(i,n,mb))/deltay
      go to 50
c         special case for second order outflow at m=mc
45    continue
      eq(index)=eq(index)
     1 +(gg(i,n,mc2)-4.*gg(i,n,mc1)+3.*gg(i,n,mc))*ddy2
50    continue
40    continue
30    continue
      return
      end
