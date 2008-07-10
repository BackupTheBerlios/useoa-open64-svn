c**********************************************************************
      subroutine single(n,m,eqp)
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
c         evaluates single interior eqn at n,m
c
c      dimension eqp(1),ffplus(3,2),ggplus(3,3),hhplus(3)
      dimension ffplus(3,2),ggplus(3,3),hhplus(3)
      double precision eqp(*)
      iflag=0
      if(m .eq. 1)iflag=1
      if(m .eq. 1)go to 25
      call mmm(m,mf,mb)
      deltay=(mf-mb)*dy
      call eval(n,mf,2,iflag,dum1,ggplus(1,2),dum2)
      call eval(n,mb,2,iflag,dum1,ggplus(1,1),dum2)
      if(m .eq. mc)call eval(n,mc2,2,iflag,dum1,ggplus(1,3),dum2)
25    call nnn(n,nf,nb)
      deltax=(nf-nb)*dx
      call eval(nf,m,1,iflag,ffplus(1,2),dum1,dum2)
      call eval(nb,m,1,iflag,ffplus(1,1),dum1,dum2)
      call eval(n,m,3,iflag,dum1,dum2,hhplus(1))
      do 100 i=1,3
      eqp(i)=(ffplus(i,2)-ffplus(i,1))/deltax+hhplus(i)
      if(m .eq. 1)go to 100
      if(m .ne. mc)eqp(i)=eqp(i)+(ggplus(i,2)-ggplus(i,1))/deltay
      if(m .eq. mc)eqp(i)=eqp(i)+(ggplus(i,3)-4.*ggplus(i,1)
     1 +3.*ggplus(i,2))*ddy2
100   continue
      return
      end
