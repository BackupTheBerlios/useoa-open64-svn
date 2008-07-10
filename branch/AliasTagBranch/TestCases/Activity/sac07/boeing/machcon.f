c**********************************************************************
      subroutine machcon
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
c         compute changes in parameters due to changed
c         gamma or mach number--perfect gas
c
      gm1=gamma-1.
      gp1=gamma+1.
      gm12=gm1/2.
      einc=pinc/(gm1*dinc)
      a2inc=gamma*gm1*einc
      ainc=dsqrt(a2inc)
      qinc=xmach*ainc
      htot=qinc**2/2.+gamma*einc
      xmach2=xmach**2
      xmshk2=(1.+gm12*xmach2)/(gamma*xmach2-gm12)
      dshk=dinc*gp1*xmach2/(gm1*xmach2+2.)
      factor=(1.+gm12*xmshk2)**(1./gm1)
      dstag=dshk*factor
      pshk=pinc*(1.+2.*gamma*(xmach2-1.)/gp1)
      pstag=pshk*(factor**gamma)
      swall=dlog(pshk/pinc)-gamma*dlog(dshk/dinc)
      return
      end
