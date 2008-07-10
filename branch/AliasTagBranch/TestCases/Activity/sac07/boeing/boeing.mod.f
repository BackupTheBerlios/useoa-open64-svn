      module all_globals_mod

       double precision u(190),eq(190),eqold(190),a(190,95),eqplus(190),
     2 b(10),bth(10),ff(3,6,10),gg(3,6,10),hh(3,6,10),
     3 dx,dy,ddx,ddy,ddx2,ddy2,
     4 theta1,gamma,gm1,xmach,
     5 qinc,dinc,pinc,einc,ainc,
     6 htot,swall,dstag,emaxs(200),eqmaxs(200),delta,
     8 sstol,alpha,emax,eqmax
     9 ,dtheta,xend,xincr
       integer mc,nc,mc1,nc1,mc2,mc3,ncf,mcncf,itmax,kprint,icont,
     7 kdiag,kblk,ksize,kblk2,kblk3,nprint
      end module

c**********************************************************************
      subroutine wall(eqns,m)
      use all_globals_mod
      implicit double precision(a-h,o-z)
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
c**********************************************************************
      subroutine var(n,m,dm,um,vm,em,pm)
      use all_globals_mod
      implicit double precision(a-h,o-z)
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
c**********************************************************************
      subroutine tran(n,m,xr,xth,yr,yth,ajac)
      use all_globals_mod
      implicit double precision(a-h,o-z)
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
c**********************************************************************
      subroutine substh(m,sthout)
      use all_globals_mod
      implicit double precision(a-h,o-z)
c Added variables smp1,sm,smm1,smm2,smm3 to hold values 
c from calls to subs() - PDH
      double precision smp1, sm, smm1, smm2, smm3
      if(m .ne. 1)go to 10
      sthout=0.
      return
10    continue
c      if(m .eq. 2)sy=(2.*s(m)-3.*s(m-1)+s(m+1))/(3.*dy)
      if(m .eq. 2) then
        call subs(m,sm)
        call subs(m-1,smm1)
        call subs(m+1,smp1)
        sy=(2.*sm-3.*smm1+smp1)/(3.*dy)
      endif
       
c      if(m .ge. 3 .and. m .le. (mc-1))sy=(s(m-2)-6.*s(m-1)
c     1 +3.*s(m)+2.*s(m+1))/(6.*dy)
      if(m .ge. 3 .and. m .le. (mc-1)) then 
        call subs(m+1,smp1)
        call subs(m,sm)
        call subs(m-1,smm1)
        call subs(m-2,smm2)
        sy=(smm2-6.*smm1+3.*sm+2.*smp1)/(6.*dy)
      endif

c      if(m .eq. mc)sy=(-2.*s(m-3)+9.*s(m-2)-18.*s(m-1)+11.*s(m))
c     1 /(6.*dy)
      if(m .eq. mc) then
        call subs(m,sm)
        call subs(m-1,smm1)
        call subs(m-2,smm2)
        call subs(m-3,smm3)
        sy=(-2.*smm3+9.*smm2-18.*smm1+11.*sm)/(6.*dy)
      endif

      sthout=sy/theta1
      return 
      end
c**********************************************************************
      subroutine shock(eqns,m)
      use all_globals_mod
      implicit double precision(a-h,o-z)
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
c**********************************************************************
      subroutine subs(m,sout)
      use all_globals_mod
      implicit double precision(a-h,o-z)
      sout=u(m*ncf)
      return 
      end
c**********************************************************************
      subroutine rth(n,m,r,theta)
      use all_globals_mod
      implicit double precision(a-h,o-z)
c
c        subroutine returns values of r and theta at point (n,m)
c
c      sm=s(m) 
      call subs(m,dummy)
      sm = dummy
      bm=b(m)
      x=(n-1)*dx
      r=x*(sm-bm)+bm
      y=(m-1)*dy
      theta=y*theta1
      return 
      end
c**********************************************************************
      subroutine nnn(n,nf,nb)
      implicit double precision(a-h,o-z)
c
c        subroutine determines forward index nf, back index nb
c        for interior x difference at point n
c
      nf=n+1
      nb=n-1
      return 
      end
c**********************************************************************
      subroutine mwall(m,mf,mb)
      use all_globals_mod
      implicit double precision(a-h,o-z)
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
c**********************************************************************
      subroutine mmm(m,mf,mb)
      use all_globals_mod
      implicit double precision(a-h,o-z)
c
c        subroutine determines forward index mf, back index mb
c        for y-differences at m
c
      mf=m+1 
      mb=m-1
      if(m .ne. 1 .and. m .ne. mc)return
      mf=mc 
      mb=mc-1
      if(m .ne. 1)return
      mf=2 
      mb=1
      return 
      end
c**********************************************************************
      subroutine inside
      use all_globals_mod
      implicit double precision(a-h,o-z)
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
c**********************************************************************
      subroutine formeq(it)
      use all_globals_mod
      implicit double precision(a-h,o-z)
      real timvec(2),etime,dummy
      external etime
c
c         forms equation vector eq
c         for each m, m=1 to mc
c           3 equations at wall n=1
c           3 interior equations, n=2 to nc1
c           4 equations at shock, n=nc
c
c$openad INDEPENDENT(u)
c      dummy=etime(timvec)
c      t1=timvec(1)
      do 100 m=1,mc
      call wall(eq,m)
      call shock(eq,m)
100   continue
      call inside
      eqmax=0.
      do 200 i=1,mcncf
c      eqmax=amax1(eqmax,dabs(eq(i)))
      if (dabs(eq(i)) .gt. eqmax) eqmax = dabs(eq(i))
      eqold(i)=eq(i)
200   continue
c      dummy=etime(timvec)
c      t2=timvec(1)
c      write(6,222)t2-t1
222   format(/," formeq time in seconds=",f10.4)
      eqmaxs(it)=eqmax
c$openad DEPENDENT(eq)
      return
      end
c**********************************************************************
      subroutine eval(n,m,k,iflag,f,g,h)
      use all_globals_mod
      implicit double precision(a-h,o-z)
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
