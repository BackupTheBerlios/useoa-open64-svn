      program newtbb
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
      ndim=190
      call init
c
c         main newton loop
c
10    continue
      do 100 it=1,itmax
      ittot=it
      call formeq(it)
      call numjac
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
