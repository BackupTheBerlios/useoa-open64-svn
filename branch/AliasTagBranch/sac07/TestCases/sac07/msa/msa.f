      subroutine dmsamain(nx,ny,x,f,g,task,w)
      character*(*) task
      integer ny,nx
      double precision f
      double precision x(nx*ny),g(nx*ny),w(nx*ny)
c     *********
c
c     Subroutine dmsamain
c
c     This subroutine is adopted from the subroutine dfmins,
C     the subroutine which selects the minimization problem from 
c     the MINPACK-2 test problem collection.  The reason we needed
c     to add dmsamain was so that a top level subroutine (dmsamain)
c     which accesses all dmsa routines could be called from ADIFOR.
c
c     Peyvand Khademi December 1994

c     The subroutine statement (based on documentation for dfmins) is
c
c       subroutine dsmamain(nx,ny,x,f,g,task,w)
c
c     where
c
c       nx is an integer variable.
c         On entry nx is the number of grid points in the first
c            coordinate direction.
c         On exit nx is unchanged.
c
c       ny is an integer variable.
c         On entry ny is the number of grid points in the second
c            coordinate direction. If the problem is formulated in
c            one spatial dimension, ny = 1.
c         On exit ny is unchanged.
c
c       x is a double precision array of dimension n.
c         On entry x specifies the vector x if the function
c            or the gradient need to be evaluated. Otherwise 
c            x need not be specified.
c         On exit x is unchanged if the function or the gradient
c            need to be evaluated.
c            Otherwise x is set according to task.
c
c       f is a double precision variable.
c         On entry f need not be specified.
c         On exit f is set to the function evaluated at x if 
c            task = 'F' or 'FG'..
c
c       g is a double precision array of dimension n.
c         On entry g need not be specified.
c         On exit g contains the gradient evaluated at x if 
c            task = 'G' or 'FG'.
c
c       task is a character variable of length atleast 42.
c         On entry task specifies the action of the subroutine:
c
c            task               action
c            ----               ------
c             'F'     Evaluate the function at x.
c             'G'     Evaluate the gradient vector at x.
c             'FG'    Evaluate the function and the gradient at x.
c             'XS'    Set x to the standard starting point xs.
c             'XL'    Set x to the lower bound xl.
c             'XU'    Set x to the upper bound xu.
c
c         On exit task is set to 'ERROR' if prob is not an acceptable
c            problem name. Oterwise task is unchanged.
c
c       w is a double precision array of dimension 4*(nx+1)*(ny+1).
c
c     MINPACK-2 Project. September 1993.
c     Argonne National Laboratory.
c     Brett M. Averick and Jorge J. More'.
c
c     **********

c$openad INDEPENDENT(x)
      call dmsabc(nx,ny,w(1),w(nx+3),w(2*nx+5),w(2*nx+ny+7))
      call dmsafg(nx,ny,x,f,g,task,w(1),w(nx+3),w(2*nx+5),w(2*nx+ny+7))
c$openad DEPENDENT(f)

      return
      end
      subroutine dmsafg(nx,ny,x,f,fgrad,task,bottom,top,left,right)
      character*(*) task
      integer nx, ny
      double precision f
      double precision x(nx*ny), fgrad(nx*ny), bottom(nx+2), top(nx+2),
     +                 left(ny+2), right(ny+2)
c     **********
c
c     Subroutine dmsafg
c
c     This subroutine computes the function and gradient of the
c     minimal surface area problem.
c
c     The subroutine statement is
c
c       subroutine dmsafg(nx,ny,x,f,fgrad,task,bottom,top,left,right)
c
c     where
c
c       nx is an integer variable.
c         On entry nx is the number of grid points in the first
c            coordinate direction.
c         On exit nx is unchanged.
c
c       ny is an integer variable.
c         On entry ny is the number of grid points in the second
c            coordinate direction.
c         On exit ny is unchanged.
c
c       x is a double precision array of dimension nx*ny.
c         On entry x specifies the vector x if task = 'F', 'G', or 'FG'.
c            Otherwise x need not be specified.
c         On exit x is unchanged if task = 'F', 'G', or 'FG'. Otherwise
c            x is set according to task.
c
c       f is a double precision variable.
c         On entry f need not be specified.
c         On exit f is set to the function evaluated at x if task = 'F'
c            or 'FG'.
c
c       fgrad is a double precision array of dimension nx*ny.
c         On entry fgrad need not be specified.
c         On exit fgrad contains the gradient evaluated at x if
c            task = 'G' or 'FG'.
c
c       task is a character variable.
c         On entry task specifies the action of the subroutine:
c
c            task               action
c            ----               ------
c             'F'     Evaluate the function at x.
c             'G'     Evaluate the gradient at x.
c             'FG'    Evaluate the function and the gradient at x.
c             'XS'    Set x to the standard starting point xs.
c
c         On exit task is unchanged.
c
c       bottom is a double precision array of dimension nx + 2.
c         On entry bottom must contain boundary data beginning
c            with the lower left corner of the domain.
c         On exit bottom is unchanged.
c
c       top is a double precision array of dimension nx + 2.
c         On entry top must contain boundary data beginning with
c            the upper left corner of the domain.
c         On exit top is unchanged.
c
c       left is a double precision array of dimension ny + 2.
c         On entry left must contain boundary data beginning with
c            the lower left corner of the domain.
c         On exit left is unchanged.
c
c       right is a double precision array of dimension ny + 2.
c         On entry right must contain boundary data beginning with
c            the lower right corner of the domain.
c         On exit right is unchanged.
c
c     MINPACK-2 Project. November 1993.
c     Argonne National Laboratory and University of Minnesota.
c     Brett M. Averick.
c
c     **********
      double precision one, p5, two, zero
      parameter (zero=0.0d0,p5=0.5d0,one=1.0d0,two=2.0d0)

      logical feval, geval
      integer i, j, k
      double precision alphaj, area, betai, dvdx, dvdy, fl, fu, hx, hy,
     +                 v, vb, vl, vr, vt, xline, yline

c     Initialize.

      hx = one/dble(nx+1)
      hy = one/dble(ny+1)
      area = p5*hx*hy

c     Compute the standard starting point if task = 'XS'.

      if (task .eq. 'XS') then
         do 20 j = 1, ny
            alphaj = dble(j)*hy
            do 10 i = 1, nx
               k = nx*(j-1) + i
               betai = dble(i)*hx
               yline = alphaj*top(i+1) + (one-alphaj)*bottom(i+1)
               xline = betai*right(j+1) + (one-betai)*left(j+1)
               x(k) = (yline+xline)/two
   10       continue
   20    continue

         return

      end if

      if (task .eq. 'F' .or. task .eq. 'FG') then
         feval = .true.
      else
         feval = .false.
      end if
      if (task .eq. 'G' .or. task .eq. 'FG') then
         geval = .true.
      else
         geval = .false.
      end if

c     Evaluate the function if task = 'F', the gradient if task = 'G',
c     or both if task = 'FG'.

      if (feval) f = zero
      if (geval) then
         do 30 k = 1, nx*ny
            fgrad(k) = zero
   30    continue
      end if

c     Computation of the function and gradient over the lower
c     triangular elements.

      do 50 j = 0, ny
         do 40 i = 0, nx
            k = nx*(j-1) + i
            if (i .ge. 1 .and. j .ge. 1) then
               v = x(k)
            else
               if (j .eq. 0) v = bottom(i+1)
               if (i .eq. 0) v = left(j+1)
            end if
            if (i .lt. nx .and. j .gt. 0) then
               vr = x(k+1)
            else
               if (i .eq. nx) vr = right(j+1)
               if (j .eq. 0) vr = bottom(i+2)
            end if
            if (i .gt. 0 .and. j .lt. ny) then
               vt = x(k+nx)
            else
               if (i .eq. 0) vt = left(j+2)
               if (j .eq. ny) vt = top(i+1)
            end if
            dvdx = (vr-v)/hx
            dvdy = (vt-v)/hy
            fl = sqrt(one+dvdx**2+dvdy**2)
            if (feval) f = f + fl
            if (geval) then
               if (i .ge. 1 .and. j .ge. 1)
     +             fgrad(k) = fgrad(k) - (dvdx/hx+dvdy/hy)/fl
               if (i .lt. nx .and. j .gt. 0)
     +             fgrad(k+1) = fgrad(k+1) + (dvdx/hx)/fl
               if (i .gt. 0 .and. j .lt. ny)
     +             fgrad(k+nx) = fgrad(k+nx) + (dvdy/hy)/fl
            end if
   40    continue
   50 continue

c     Computation of the function and the gradient over the upper
c     triangular elements.

      do 70 j = 1, ny + 1
         do 60 i = 1, nx + 1
            k = nx*(j-1) + i
            if (i .le. nx .and. j .gt. 1) then
               vb = x(k-nx)
            else
               if (j .eq. 1) vb = bottom(i+1)
               if (i .eq. nx+1) vb = right(j)
            end if
            if (i .gt. 1 .and. j .le. ny) then
               vl = x(k-1)
            else
               if (j .eq. ny+1) vl = top(i)
               if (i .eq. 1) vl = left(j+1)
            end if
            if (i .le. nx .and. j .le. ny) then
               v = x(k)
            else
               if (i .eq. nx+1) v = right(j+1)
               if (j .eq. ny+1) v = top(i+1)
            end if
            dvdx = (v-vl)/hx
            dvdy = (v-vb)/hy
            fu = sqrt(one+dvdx**2+dvdy**2)
            if (feval) f = f + fu
            if (geval) then
               if (i .le. nx .and. j .gt. 1)
     +             fgrad(k-nx) = fgrad(k-nx) - (dvdy/hy)/fu
               if (i .gt. 1 .and. j .le. ny)
     +             fgrad(k-1) = fgrad(k-1) - (dvdx/hx)/fu
               if (i .le. nx .and. j .le. ny)
     +             fgrad(k) = fgrad(k) + (dvdx/hx+dvdy/hy)/fu
            end if
   60    continue
   70 continue

c     Scale the function and the gradient.

      if (feval) f = area*f
      if (geval) then
         do 80 k = 1, nx*ny
            fgrad(k) = area*fgrad(k)
   80    continue
      end if

      end
      subroutine dmsabc(nx,ny,bottom,top,left,right)
      integer nx, ny
      double precision bottom(nx+2), top(nx+2), left(ny+2), right(ny+2)
c     **********
c
c     Subroutine dmsabc
c
c     This subroutine computes Enneper's boundary conditions for the
c     minimal surface area problem on the unit square centered at the
c     origin.
c
c     The subroutine statement is
c
c       subroutine dmsabc(nx,ny,hx,hy,bottom,top,left,right)
c
c     where
c
c       nx is an integer variable.
c         On entry nx is the number of grid points in the first
c            coordinate direction.
c         On exit nx is unchanged.
c
c       ny is an integer variable.
c         On entry ny is the number of grid points in the second
c            coordinate direction.
c         On exit ny is unchanged.
c
c       bottom is a double precision array of dimension nx + 2.
c         On entry bottom need not be specified.
c         On exit bottom contains boundary values for the bottom
c            boundary of the domain.
c
c       top is a double precision array of dimension nx + 2.
c         On entry top need not be specified.
c         On exit top contains boundary values for the top boundary of
c            the domain.
c       left is a double precision array of dimension ny + 2.
c         On entry left need not be specified.
c         On exit left contains boundary values for the left boundary
c            of the domain.
c
c       right is a double precision array of dimension ny + 2.
c         On entry right need not be specified.
c         On exit right contains boundary values for the right boundary
c            of the domain.
c
c     MINPACK-2 Project. November 1993.
c     Argonne National Laboratory and University of Minnesota.
c     Brett M. Averick.
c
c     **********
      integer maxit
      double precision b, l, one, r, t, three, tol, two
      parameter (one=1.0d0,two=2.0d0,three=3.0d0)
      parameter (maxit=5,tol=1.0d-10)
      parameter (b=-.50d0,t=.50d0,l=-.50d0,r=.50d0)

      integer i, j, k, limit
      double precision det, fnorm, hx, hy, xt, yt
      double precision nf(2), njac(2,2), u(2)

c     Compute Enneper's boundary conditions: bottom, top, left, then
c     right.  Enneper's boundary values are obtained by defining
c     bv(x,y) = u**2 - v**2 where u and v are the unique solutions of
c     x = u + u*(v**2) - (u**3)/3, y = -v - (u**2)*v + (v**3)/3.

      hx = (r-l)/dble(nx+1)
      hy = (t-b)/dble(ny+1)

      do 40 j = 1, 4
         if (j .eq. 1) then
            yt = b
            xt = l
            limit = nx + 2
         else if (j .eq. 2) then
            yt = t
            xt = l
            limit = nx + 2
         else if (j .eq. 3) then
            yt = b
            xt = l
            limit = ny + 2
         else if (j .eq. 4) then
            yt = b
            xt = r
            limit = ny + 2
         end if

c        Use Newton's method to solve xt = u + u*(v**2) - (u**3)/3,
c        yt = -v - (u**2)*v + (v**3)/3.

         do 30 i = 1, limit
            u(1) = xt
            u(2) = -yt
            do 10 k = 1, maxit
               nf(1) = u(1) + u(1)*u(2)**2 - u(1)**3/three - xt
               nf(2) = -u(2) - u(1)**2*u(2) + u(2)**3/three - yt
               fnorm = sqrt(nf(1)*nf(1)+nf(2)*nf(2))
               if (fnorm .le. tol) go to 20
               njac(1,1) = one + u(2)**2 - u(1)**2
               njac(1,2) = two*u(1)*u(2)
               njac(2,1) = -two*u(1)*u(2)
               njac(2,2) = -one - u(1)**2 + u(2)**2
               det = njac(1,1)*njac(2,2) - njac(1,2)*njac(2,1)
               u(1) = u(1) - (njac(2,2)*nf(1)-njac(1,2)*nf(2))/det
               u(2) = u(2) - (njac(1,1)*nf(2)-njac(2,1)*nf(1))/det
   10       continue
   20       continue

            if (j .eq. 1) then
               bottom(i) = u(1)*u(1) - u(2)*u(2)
               xt = xt + hx
            else if (j .eq. 2) then
               top(i) = u(1)*u(1) - u(2)*u(2)
               xt = xt + hx
            else if (j .eq. 3) then
               left(i) = u(1)*u(1) - u(2)*u(2)
               yt = yt + hy
            else if (j .eq. 4) then
               right(i) = u(1)*u(1) - u(2)*u(2)
               yt = yt + hy
            end if
   30    continue
   40 continue

      end
C234567890         0         0         0         0         0         012
C                                                                      C
C     Dummy function driver code for the original (Not Partially-      C
C     Separated) MINPACK-2 Minimal Surface Area (MSA) problem.         C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PROGRAM nps_msa_dummy_driver

      INTEGER maxnx
      PARAMETER(maxnx=300)
      INTEGER maxny
      PARAMETER(maxny=300)
      INTEGER nmax
      PARAMETER(nmax=maxnx*maxny)
      INTEGER nx
      INTEGER ny
      DOUBLE PRECISION x(nmax)
      DOUBLE PRECISION f
      DOUBLE PRECISION w(nmax)
      DOUBLE PRECISION fgrad(nmax)
      CHARACTER*6 task

      CALL dmsamain(nx,ny,x,f,fgrad,task,w)

      STOP
      END



