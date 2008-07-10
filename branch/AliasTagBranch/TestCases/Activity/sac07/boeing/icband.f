c**********************************************************************

      subroutine icband (a, ndim, neq, nrhs, nld, nud, b)
      implicit double precision(a-h,o-z)
c
c   this subroutine is an in core matrix solver.
c   written by john bell.
c
c      dimension a(ndim,1), b(ndim,1)
      double precision a(ndim,*), b(ndim,*)
      real timvec(2),etime,dummy
      external etime
c
      dummy=etime(timvec)
      t1=timvec(1)
      nlc = nld + 1
      nbw = nlc + nud
      nm1 = neq - 1
      do 10 i = 1 , nld
         ind = nlc - i + 1
         do 5 j = ind , nbw
            a(i,j-ind+1) = a(i,j)
    5    continue
         ind = i + nud + 1
         do 6 j = ind , nbw
            a(i,j) = 0.0
    6    continue
   10 continue
c
c   select pivot.
c
      do 300 n = 1 , nm1
         k = n
         apm = dabs (a(n,1))
         nd = min0 (nld, neq-n)
         do 110 j = 1 , nd
            if (apm .gt. dabs (a(n+j,1))) go to 110
            apm = dabs (a(n+j,1))
            k = n + j
  110    continue
c
c   check threshhold.
c
         if (apm .lt. 1.e-14) go to 1000
         if (dabs (a(n,1)) .gt. 0.1 * apm) go to 150
c
c   pivot.
c
         do 130 j = 1 , nbw
            scr = a(k,j)
            a(k,j) = a(n,j)
            a(n,j) = scr
  130    continue
         do 140 j = 1 , nrhs
            scr = b(k,j)
            b(k,j) = b(n,j)
            b(n,j) = scr
  140    continue
c
c   eliminate.
c
  150    continue
         nd = min0 (nld, neq-n)
         do 200 i = 1 , nd
            amult = a(n+i,1) / a(n,1)
            if(dabs(amult) .lt. 1.e-14)go to 175
            do 160 j = 2 , nbw
               a(n+i,j-1) = a(n+i,j) - amult * a(n,j)
  160       continue
            a(n+i,nbw) = 0.0
            do 170 j = 1 , nrhs
               b(n+i,j) = b(n+i,j) - amult * b(n,j)
  170       continue
      go to 200
175   do 180 j=2,nbw
180   a(n+i,j-1)=a(n+i,j)
      a(n+i,nbw)=0.
  200    continue
  300 continue
      if (dabs (a(neq,1)) .lt. 1.e-14) go to 1000
c
c   backsolve.
c
      do 325 j = 1 , nrhs
         b(neq,j) = b(neq,j) / a(neq,1)
  325 continue
      do 400 l = 1 , nm1
         n = neq - l
         nd = min0 (nbw, l+1)
         do 390 j = 1 , nrhs
            do 350 i = 2 , nd
               b(n,j) = b(n,j) - a(n,i) * b(n+i-1,j)
  350       continue
               b(n,j) = b(n,j) / a(n,1)
  390    continue
  400 continue
      dummy=etime(timvec)
      t2=timvec(1)
      write(6,5000)t2-t1
5000  format(/," icband time in seconds=",f10.4)
c
      return
 1000 stop " singular matrix found in icband "
      end
