      PROGRAM schiller

c  ODRPACK Argument Definitions (listed alphabetically)
c      ==> fun      name of the user supplied function subroutine
c      ==> jac      name of the user supplied jacobian subroutine
c      ==> n        number of observations 
c      ==> m        columns of data in the independent variable
c      ==> np       number of parameters
c      ==> nq       number of responses per observation
c     <==> beta     function parameters
c      ==> y        dependent variable
c      ==> ldy      leading dimension of array y
c      ==> x        independent variable
c      ==> ldx      leading dimension of array x
c      ==> we       "epsilon" weights
c      ==> ldwe     leading dimension of array we
c      ==> ld2we    second dimension of array we
c      ==> wd       "delta" weights
c      ==> ldwd     leading dimension of array wd
c      ==> ld2wd    second dimension of array wd
c      ==> ifixb    indicators for "fixing" parameters (beta)
c      ==> ifixx    indicators for "fixing" independent variable (x)
c      ==> ldifx    leading dimension of array ifixx
c      ==> job      task to be performed 
c      ==> ndigit   good digits in subroutine fun results
c      ==> taufac   trust region initialization factor
c      ==> sstol    sum of squares convergence criterion
c      ==> partol   parameter convergence criterion
c      ==> maxit    maximum number of iterations
c      ==> iprint   print control 
c      ==> lunerr   logical unit for error reports 
c      ==> lunrpt   logical unit for computation reports 
c      ==> stpb     step sizes for finite difference derivatives wrt beta
c      ==> stpd     step sizes for finite difference derivatives wrt delta
c      ==> ldstpd   leading dimension of array stpd
c      ==> sclb     scale values for parameters beta
c      ==> scld     scale values for errors delta in independent variable 
c      ==> ldscld   leading dimension of array scld
c     <==> work     DOUBLE PRECISION work vector
c      ==> lwork    dimension of vector work
c     <==  iwork    INTEGER work vector
c      ==> liwork   dimension of vector iwork
c     <==  info     stopping condition 
 
c  Parameters specifying maximum problem sizes handled by this driver
c     maxn          maximum number of observations 
c     maxm          maximum number of columns in independent variable
c     maxnp         maximum number of function parameters
c     maxnq         maximum number of responses per observation

c  Parameter Declarations and Specifications
      INTEGER    ldifx,ldscld,ldstpd,ldwd,ldwe,ldx,ldy,ld2wd,ld2we,
     +           liwork,lwork,maxm,maxn,maxnp,maxnq
      PARAMETER (maxm=2,maxn=70,maxnp=10,maxnq=2,
     +           ldy=maxn,ldx=maxn,
     +           ldwe=1,ld2we=1,ldwd=1,ld2wd=1,
     +           ldifx=1,ldstpd=1,ldscld=maxn,
     +           lwork=72000,
     +           liwork=300)

c  Variable Declarations 
      INTEGER          i,info,iprint,j,job,lunerr,lunrpt,m,maxit,n,
     +                 ndigit,np,nq
      INTEGER          ifixb(maxnp),ifixx(ldifx,maxm),iwork(liwork)
      INTEGER          idisk,iplot
      INTEGER          istopf
      DOUBLE PRECISION partol,sstol,taufac
      DOUBLE PRECISION beta(maxnp),sclb(maxnp),scld(ldscld,maxm),
     +                 stpb(maxnp),stpd(ldstpd,maxm),
     +                 wd(ldwd,ld2wd,maxm),we(ldwe,ld2we,maxnq),
     +                 work(lwork),x(ldx,maxm),y(ldy,maxnq),
     +                 f(maxn,maxm),xplusd(maxn,maxm)
      DOUBLE PRECISION dumb
      REAL             t0,t1,timer
      EXTERNAL         fun,jac

c common block temporarily needed to get jacobian 
c from subroutine fun to subroutine jac
      double precision jsave
      common /savejac/ jsave(300,20,2)
 

c  Set up ODRPACK report files
      lunerr  =   9
      lunrpt  =   9
c      OPEN (unit=9,file='report')

c  Set up data file and read problem data
      np = 8
      nq = 2
      m = 2

      beta(1) = -7.
      beta(2) = 14.
      beta(3) =   .8
      beta(4) =   .7
      beta(5) =  7.
      beta(6) = -6.
      beta(7) =  4.
      beta(8) = -4.

      OPEN (unit=4,file='data.dat')
	i = 1
100	   Read (4,*,end = 101,err=101) 
     +         idisk, iplot, x(i,1), dumb, x(i,2), (y(i,j),j=1,2)
           x(i,1) = x(i,1) / 100.0d0
  	   y(i,1) = y(i,1) / 100.0d0
  	   y(i,2) = y(i,2) / 100.0d0
	   i = i + 1
           if (i.eq.64) go to 101
	goto 100
101	n = i-1

c  Compute solution
      t0 = timer()
      do 43 i =1, n
        do 143 j = 1 , m
          xplusd(i,j) = x(i,j)
  143   continue
   43 continue
      call fun(n,m,np,nq,beta,xplusd,maxn,f,maxn,istopf)
      t1 = timer()
      END
