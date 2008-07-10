      program newtbb

C This parameter specifies the maximum number of rows in ADIFOR gradient
C objects (rows in ADIFOR g.o. are columns in the Jacobian).

      integer g$pmax$
      parameter (g$pmax$ = 190)

C This parameter liwa specifies the length of work array iwa used for
C coloring.  It should be at least 6*pmax
      integer liwa
      parameter (liwa = 1330)

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

C These variables are used by the ADIFOR-generated subroutine g$formeq$2

        double precision g$u(g$pmax$, 190)
        double precision g$eq(g$pmax$, 190)
        double precision g$eqold(g$pmax$, 190)
        double precision g$a(g$pmax$, 190, 95)
        double precision g$eqplus(g$pmax$, 190)
        double precision g$b(g$pmax$, 10)
        double precision g$bth(g$pmax$, 10)
        double precision g$ff(g$pmax$, 3, 6, 10)
        double precision g$gg(g$pmax$, 3, 6, 10)
        double precision g$hh(g$pmax$, 3, 6, 10)
        integer g$mc(g$pmax$)
        integer g$nc(g$pmax$)
        double precision g$dx(g$pmax$)
        double precision g$dy(g$pmax$)
        double precision g$ddx(g$pmax$)
        double precision g$ddy(g$pmax$)
        double precision g$ddx2(g$pmax$)
        double precision g$ddy2(g$pmax$)
        integer g$mc1(g$pmax$)
        integer g$nc1(g$pmax$)
        integer g$mc2(g$pmax$)
        integer g$mc3(g$pmax$)
        double precision g$theta1(g$pmax$)
        double precision g$gamma(g$pmax$)
        double precision g$gm1(g$pmax$)
        double precision g$xmach(g$pmax$)
        integer g$ncf(g$pmax$)
        integer g$mcncf(g$pmax$)
        double precision g$qinc(g$pmax$)
        double precision g$dinc(g$pmax$)
        double precision g$pinc(g$pmax$)
        double precision g$einc(g$pmax$)
        double precision g$ainc(g$pmax$)
        double precision g$htot(g$pmax$)
        double precision g$swall(g$pmax$)
        double precision g$dstag(g$pmax$)
        double precision g$emaxs(g$pmax$, 200)
        double precision g$eqmaxs(g$pmax$, 200)
        double precision g$delta(g$pmax$)
        integer g$kdiag(g$pmax$)
        integer g$kblk(g$pmax$)
        integer g$ksize(g$pmax$)
        integer g$kblk2(g$pmax$)
        integer g$kblk3(g$pmax$)
        integer g$itmax(g$pmax$)
        double precision g$sstol(g$pmax$)
        double precision g$alpha(g$pmax$)
        integer g$nprint(g$pmax$)
        double precision g$emax(g$pmax$)
        double precision g$eqmax(g$pmax$)
        integer g$kprint(g$pmax$)
        double precision g$dtheta(g$pmax$)
        integer g$icont(g$pmax$)
        double precision g$xend(g$pmax$)
        double precision g$xincr(g$pmax$)

        common /g$/ g$u, g$eq, g$eqold, g$a, g$eqplus, g$b, g$bth, g$ff,
     * g$gg, g$hh, g$mc, g$nc, g$dx, g$dy, g$ddx, g$ddy, g$ddx2, g$ddy2,
     * g$mc1, g$nc1, g$mc2, g$mc3, g$theta1, g$gamma, g$gm1, g$xmach, g$
     *ncf, g$mcncf, g$qinc, g$dinc, g$pinc, g$einc, g$ainc, g$htot, g$sw
     *all, g$dstag, g$emaxs, g$eqmaxs, g$delta, g$kdiag, g$kblk, g$ksize
     *, g$kblk2, g$kblk3, g$itmax, g$sstol, g$alpha, g$nprint, g$emax, g
     *$eqmax, g$kprint, g$dtheta, g$icont, g$xend, g$xincr

c This variable indicates how many columns of the Jacobian the
c ADIFOR-generated subroutine g$formeq$2 should evaluate (this is
c much less than the actual number of columns when compression is
c used.
        
      integer g$p$

c These variables used for coloring routine

      integer indrow(15000),indcol(15000),ngrp(190),iwa(liwa)
      integer ipntr(191),jpntr(191)
      integer count,maxgrp,mingrp

c Needed to know how to compress
      logical nonzero(190,190)
c
c         program to solve blunt body problem using steady
c         shock tracking and newtons method
c
c         implements algorithm reported in 
c         "Steady Shock Tracking, Newton's Method, and
c         the Supersonic Blunt Body Problem," by
c         G.R. Shubin, A.B. Stephens, H.M. Glaz, 
c         A.B. Wardlaw, and L.B. Hackerman,
c
c         SIAM Journal on Scientific and Statistical Computing,
c         Vol. 3, n. 2, June 1982, pp. 127-144.
c
c

C  Initialization for ADIFOR
C  Also, initialize nonzero array, so that we know how to compress things.
      do 43 i=1, 190
        do 44 j=1, 190
          g$u(j,i)=0.0
          nonzero(j,i) = .false.
 44     continue
        g$u(i,i) = 1.0d0
 43   continue

      g$p$ = 190
      ndim=190
      call init
C  For first two iterations, use g$p =190 and evaluate the Jacobian in dense
C  form.
      do 200 it=1, 2
      ittot= it

C Call the ADIFOR-generated subroutine.  This computes both eq and
C g$eq, the gradient object containing the Jacobian.

      call g$formeq$2(g$p$,it)

C Copy the Jacobian into a (icband expects a special structure)
      do 141 i=1, 190
        do 142 j=1, 95
          a(i,j) = 0.0
 142    continue
 141  continue
      count = 0
      do 143 i=1, 190
        do 144 j=1, 190
          if (g$eq(j,i) .ne. 0.0)  then
            nonzero(j,i) = .true.
            count = count + 1
            a(i,j-i+kdiag) = g$eq(j,i)
            indrow(count) = i
            indcol(count) = j
          endif
 144    continue
 143  continue

C Call icband, et al
      call icband(a,ndim,mcncf,1,kblk3,kblk2-1,eq)
      call update(it)
      call out(it)
      if(emax .lt. sstol)go to 105
 200  continue

C Do coloring, so that we can use a compressed form of the Jacobian

      call dsm(190,190,count,indrow,indcol,ngrp,maxgrp,mingrp,
     *         info,ipntr,jpntr,iwa,liwa)

C Initialize g$u and g$p accordingly
      g$p$ = maxgrp
      do 191 i = 1, 190
        do 192 j =1, g$p$
          g$u(j,i) = 0.0
 192    continue
        g$u(ngrp(i),i) = 1.0
 191  continue
c
c         main newton loop
c

C from iteration 3 on, we use a compressed Jacobian.
      write(27,*) g$p$
10    continue
      do 100 it=3,itmax
      ittot=it
C Call the ADIFOR-generated subroutine.  This computes both eq and
C g$eq, the gradient object containing the Jacobian.

      call g$formeq$2(g$p$,it)

C Copy the Jacobian into a (icband expects a special structure)
      do 241 i=1, 190
        do 242 j=1, 95
          a(i,j) = 0.0
 242    continue
 241  continue
      do 243 i=1, 190
        do 244 j=1, 190
          if (nonzero(j,i)) then
            a(i,j-i+kdiag) = g$eq(ngrp(j),i)
          endif
 244    continue
 243  continue

C Call icband, et al 
      call icband(a,ndim,mcncf,1,kblk3,kblk2-1,eq)
      call update(it)
      call out(it)
      if(emax .lt. sstol)go to 105
100   continue
c
c         end main newton loop
c
      write(6,1000)
1000  format(5(/)," steady state not reached")
      call endit(ittot)
      stop
105   continue
      write(6,1001)
1001  format(5(/)," steady state reached")
      call endit(ittot)
      if(icont .eq. 0 .or. xmach .le. xend)stop
      call contin
      go to 10
      end
