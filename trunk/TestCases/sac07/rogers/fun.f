      SUBROUTINE fun(n,m,np,nq,
     +               beta,xplusd,ldxpd,
     +               f,ldf)

c  Subroutine Arguments
c      ==> n        number of observations
c      ==> m        number of columns in independent variable
c      ==> np       number of parameters
c      ==> nq       number of responses per observation
c      ==> beta     current values of parameters
c      ==> xplusd   current value of independent variable
c      ==> ldxpd    leading dimension of xplusd
c     <==  f        predicted function values
c      ==> ldf      leading dimension of f

c  Variable Declarations
      INTEGER          i,j,k,ldf,ldxpd,m,n,np,nq,numpars
      INTEGER          ia, ib
      DOUBLE PRECISION beta(np),f(ldf,nq),xplusd(ldxpd,m)

      double precision par(20),fn(2)

      do 10 k=1,np
         par(k) = beta(k)
   10 continue
   
      do 100 i=1,n
         do 20 j=1,m
            par(np+j) = xplusd(i,j)
   20    continue

c  compute function values (fn) given parameters (par)
         call fnc(par,fn)

         f(i,1) = fn(1)
         f(i,2) = fn(2)

  100 continue
      return
      end
