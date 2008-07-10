c**********************************************************************
      subroutine formeq(it)
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
      real timvec(2),etime,dummy
      external etime
c
c         forms equation vector eq
c         for each m, m=1 to mc
c           3 equations at wall n=1
c           3 interior equations, n=2 to nc1
c           4 equations at shock, n=nc
c
      dummy=etime(timvec)
      t1=timvec(1)
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
      dummy=etime(timvec)
      t2=timvec(1)
      write(6,222)t2-t1
222   format(/," formeq time in seconds=",f10.4)
      eqmaxs(it)=eqmax
      return
      end
