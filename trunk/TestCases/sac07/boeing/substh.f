c**********************************************************************
      subroutine substh(m,sthout)
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
