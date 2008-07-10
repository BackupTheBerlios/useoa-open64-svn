c**********************************************************************
      subroutine out(iter)
      implicit double precision(a-h,o-z)
c
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
      write (6 , 1001) iter,alpha
      if(mod(iter,nprint) .ne. 0)go to 111
      write (6 , 1002)
c
c        output flow variables
      do 10 m=1,mc
      write(6,1012)
      do 10 n=1,nc
      call rth(n,m,r,theta)
      call var(n,m,dm,um,vm,em,pm)
      amach=dsqrt((um*um+vm*vm)/(gamma*gm1*em))
      write(6,1003)n,m,r,theta,dm,um,vm,em,pm,amach
10    continue
c
c
      write (6 , 1004)
      write (6 , 1005)
      do 110 i=1,mc
         angle = (i - 1) * dtheta
      write(6,1006)angle,s(i),b(i)
  110 continue
      if(kprint .eq. 0)go to 111
c
c        print iteration errors and equation values
c
      write(6,1009)
      do 201 m=1,mc
      do 200 n=1,nc
      ind=(m-1)*ncf+(n-1)*3+1
      write(6,1013)n,m,eq(ind),eq(ind+1),eq(ind+2),
     1 eqold(ind),eqold(ind+1),eqold(ind+2)
200   continue
      write(6,1014)eq(m*ncf),eqold(m*ncf)
      write(6,1012)
201   continue
  111 continue
      write (6 , 1007) emax,eqmax
c
c
c
1001  format(10(/)," iteration number=",i5,10x,
     1 "with alpha=",f10.4,/)
1002  format("  n  m",7x,"r",12x,"theta",9x,"density",9x,
     1 "u-vel",10x,"v-vel",10x,
     1 "energy",8x,"pressure",8x,"mach no.")
1003  format(2i3,1p8e15.6)
 1004 format (//, " shock and wall radii ")
 1005 format (/,6x, "theta", 12x, "s",14x,"b")
 1006 format (1p3e15.6)
 1007 format (//, " maximum iteration error = ", 1p1e15.6,
     1 10x,"max equation value=",1pe15.6)
1009  format(//,"  n  m",10x,"iteration errors",40x,
     1 "equation values from previous iterate",/)
 1012 format (/)
1013  format(2i3,1p3e12.4,27x,1p3e12.4)
 1014 format (6x, 1p1e12.4, 51x, 1p1e12.4)
c
      return
      end
