c**********************************************************************
      subroutine wall(eqns,m)
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
c      dimension eqns(1)
      double precision eqns(*)
c
c        evaluates eqns at wall
c
      mm=(m-1)*ncf
      call var(1,m,dm,um,vm,em,pm)
      if(m .eq. 1)go to 100
      bm=b(m) 
      bm2=bm*bm
      bthm=bth(m)
      bthb=bthm/bm
      dee2=1.+bthb**2
      dee=dsqrt(dee2)
      qtan=(um*bthb+vm)/dee
      call tran(1,m,xr,xth,yr,yth,ajac)
      beta=um*yr+vm*yth/bm
      call mwall(m,mf,mb)
      call var(1,mf,dum1,uf,vf,dum2,pf)
      call var(1,mb,dum1,ub,vb,dum2,pb)
      deltax=2.*dx
      deltay=(mf-mb)*dy
c        boundary condition
      value=um-vm*bthb
      eqns(mm+1)=value
c        entropy on wall=constant
      eqns(mm+2)=dlog(pm/pinc)-gamma*dlog(dm/dinc)-swall
c        l4 compat.
      call rth(1,m,dum,theta)
      cotth=1./dtan(theta)
      z2=gamma*gm1*em 
      z=dsqrt(z2)
      cpx=-z*xr*dee
      cux=dm*z2*xr
      cvx=dm*z2*xth/bm
      cpy=beta+z*(-yr+bthb*yth/bm)/dee
      cuy=dm*z*(z*yr-beta/dee)
      cvy=dm*z*(z*yth+bthm*beta/dee)/bm
      cinhom=dm*z*(z*(2.*um+vm*cotth)+vm*qtan)/bm
c        determine spatial differences
      py=(pf-pb)/deltay
      uy=(uf-ub)/deltay
      vy=(vf-vb)/deltay
      if(m .ne. mc)go to 50
      call var(1,mc-2,dum1,ubb,vbb,dum2,pbb)
      deltay=2.*dy
      py=(pbb-4.*pb+3.*pf)/deltay
      uy=(ubb-4.*ub+3.*uf)/deltay
      vy=(vbb-4.*vb+3.*vf)/deltay
50    continue
      call var(2,m,dum1,uf,vf,dum2,pf)
      call var(3,m,dum1,uff,vff,dum2,pff)
      px=(-3.*pm+4.*pf-pff)/deltax
      ux=(-3.*um+4.*uf-uff)/deltax
      vx=(-3.*vm+4.*vf-vff)/deltax
c        evaluate eqn
      value=cpx*px+cux*ux+cvx*vx+cpy*py+cuy*uy+cvy*vy+cinhom
      eqns(mm+3)=value
      return
100   continue
      eqns(mm+1)=dm-dstag
      eqns(mm+2)=um
      eqns(mm+3)=vm
      return 
      end
