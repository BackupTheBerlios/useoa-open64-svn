c**********************************************************************
      subroutine shock(eqns,m)
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
c        subroutine evaluates r-h eqns and compatibility
c
      mm=(m-1)*ncf+(nc-1)*3
c      sm=s(m) 
      call subs(m,dummy)
      sm = dummy
      sm2=sm*sm
c      sthm=sth(m)
      call substh(m,dummy)
      sthm = dummy
      sths=sthm/sm
      ds2=1.+sths**2
      ds=dsqrt(ds2)
      call rth(nc,m,r,theta) 
      sinth=sin(theta) 
      costh=cos(theta)
      capqb=-qinc*(costh+sths*sinth)/ds
      call var(nc,m,dm,um,vm,em,pm)
      capqa=(um-sths*vm)/ds
c        r-h mass
      value=dm*capqa-dinc*capqb
      eqns(mm+1)=value
c        r-h momentum
      value=pm+dm*capqa**2-pinc-dinc*capqb**2
      eqns(mm+2)=value
c        tangential velocity
      qtangb=qinc*(-sths*costh+sinth)/ds
      qtanga=(um*sths+vm)/ds
      value=qtanga-qtangb
      eqns(mm+3)=value
c        compatibility
      z2=gamma*gm1*em 
      z=dsqrt(z2)
      sig=-ds*z
      call tran(nc,m,xr,xth,yr,yth,ajac)
      aa=um*xr+vm*xth/sm
      bb=um*yr+vm*yth/sm
      alam2=1. 
      alam3=-sthm
c        determine coeffs of various terms
      cpx=-sig*aa+z2*(alam2*xr+alam3*xth/sm2)
      cux=dm*z2*(-sig*xr+alam2*aa)
      cvx=dm*z2*(-sig*xth+alam3*aa)/sm
      if(m .eq. 1)go to 20
      cpy=-sig*bb+z2*(alam2*yr+alam3*yth/sm2)
      cuy=dm*z2*(-sig*yr+alam2*bb)
      cvy=dm*z2*(-sig*yth+alam3*bb)/sm
      cinhom=-dm*z2*(sig*(2.*um+vm*costh/sinth)+vm*(alam2*vm
     1 -alam3*um/sm))/sm
      go to 25
20    continue
c        special for shock pt on sym line
      cpy=0.
      cuy=0.
      cvy=-2.*dm*z2*sig*yth/sm
      cinhom=-2.*dm*z2*sig*um/sm
25    continue
c        determine spatial differences
      call var(nc-1,m,dum1,ub,vb,dum2,pb)
      call var(nc-2,m,dum1,ubb,vbb,dum2,pbb)
      deltax=2.*dx
      px=(pbb-4.*pb+3.*pm)/deltax
      ux=(ubb-4.*ub+3.*um)/deltax
      vx=(vbb-4.*vb+3.*vm)/deltax
      call mwall(m,mf,mb)
      call var(nc,mf,dum1,uf,vf,dum2,pf)
      call var(nc,mb,dum1,ub,vb,dum2,pb)
      deltay=(mf-mb)*dy
      py=(pf-pb)/deltay
      uy=(uf-ub)/deltay
      vy=(vf-vb)/deltay
      if(m .ne. mc)go to 50
      deltay=2.*dy
      call var(nc,mc-2,dum1,ubb,vbb,dum2,pbb)
      py=(pbb-4.*pb+3.*pf)/deltay
      uy=(ubb-4.*ub+3.*uf)/deltay
      vy=(vbb-4.*vb+3.*vf)/deltay
50    continue
c        evaluate eqn
      value=cpx*px+cux*ux+cvx*vx+cpy*py+cuy*uy+cvy*vy+cinhom
      eqns(mm+4)=value
100   continue
      return 
      end
