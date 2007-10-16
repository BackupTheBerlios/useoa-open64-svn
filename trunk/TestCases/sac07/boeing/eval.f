c**********************************************************************
      subroutine eval(n,m,k,iflag,f,g,h)
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
      dimension f(3),g(3),h(3)
c
c     subroutine returns flux vectors f,g and inhom. term h at (n,m)
c        special case iflag=1 for symmetry line m=1
c         k=1 f only, k=2 g only, k=3 h only, k=4 all
c
      call tran(n,m,xr,xth,yr,yth,ajac)
      call rth(n,m,r,theta)
      call var(n,m,dm,um,vm,em,pm)
      xir=ajac*r*r
      if(k .eq. 2)go to 20
      if(k .eq. 3)go to 30
10    continue
      aa=um*xr+vm*xth/r
c        flux vector f
      cf1=dm*aa*xir
      f(1)=cf1
      f(2)=um*cf1+xir*xr*pm
      f(3)=vm*cf1+xir*xth*pm/r
      if(k .ne. 4)return
20    continue
c        flux vector g
      bb=um*yr+vm*yth/r
      cg1=dm*bb*xir
      g(1)=cg1
      g(2)=um*cg1+xir*yr*pm
      g(3)=vm*cg1+xir*yth*pm/r
      if(k .ne. 4)return
30    continue
c        inhomogeneous term h
      if(iflag .eq. 1)go to 40
      costh=cos(theta) 
      sinth=sin(theta)
      h1=ajac*costh*r*dm*vm/sinth
      h(1)=h1
      h(2)=h1*um-ajac*r*(2.*pm+dm*vm*vm)
      h(3)=h1*vm+ajac*r*dm*um*vm
       return
40    continue
      call var(n,2,dum1,dum2,vf,dum3,dum4)
      vy=vf/dy
      vth=vy/theta1
      h(1)=2.*r*vth*dm*ajac
      h(2)=h(1)*um-ajac*2.*r*pm
      h(3)=h(1)*vm
      return 
      end
