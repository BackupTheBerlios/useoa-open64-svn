c**********************************************************************
      subroutine jacob(n,m,kbeg,kend)
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
c         evaluates part of numerical jacobian at wall or shock
c         for variables kbeg to kend
c
      do 100 k=kbeg,kend
      usave=u(k)
      u(k)=u(k)+delta
      if(n .eq. nc)go to 30
c         at wall
      call wall(eqplus,m)
      iimax=3
      go to 40
c         at shock
30    call shock(eqplus,m)
      iimax=4
40    do 50 ii=1,iimax
      idiag=(m-1)*ncf+(n-1)*3+ii
      index=k-idiag+kblk3+1
      a(idiag,index)=(eqplus(idiag)-eq(idiag))/delta
50    continue
      u(k)=usave
100   continue
      return
      end
