c  File:  HEART              14-AUG-1991

      program HeartDv

c Purpose:  Test driver for Heart example.
c           Test ADIFOR

c     The convenction adhered to is that
c      <object>1 denotes hand-coded derivatives
c      <object>2 denotes adifor-generated derivatives

      integer nxmax, ldfjac, i, j, k
      parameter (nxmax = 8)
      parameter (ldfjac = nxmax)
      double precision x(nxmax),fvec1(nxmax),fjac1(ldfjac,nxmax)

      character*6 prob(5)
      data prob / 'ddgv1', 'ddgv2', 'ddgv3', 'ddgv4', 'ddgv5' /

      do 20 k = 1, 5
      write (6, 1010) prob(k)
 1010 format (// 'Human Heart Dipole problem ', A6, '.')

c     Evaluate function and gradient:
c             info = -1       Set x to the standard starting point x0.
      call ddgv (nxmax, x, fvec1, ldfjac, fjac1, -1, prob(k))
      write (6, 1020)
 1020 format ('Standard starting point:')
      write (6, 1030) (x(i), i = 1, nxmax)
 1030 format (5d15.7)
c             info = 3       Evaluate the function and the Jacobian at x.
      call ddgv (nxmax, x, fvec1, ldfjac, fjac1, 1, prob(k))
      call ddgv (nxmax, x, fvec1, ldfjac, fjac1, 2, prob(k))
      write (6, 1040)
 1040 format ('Function:')
      write (6, 1030) (fvec1(i), i = 1, nxmax)
      write (6, 1050)
 1050 format ('Jacobian:')
      do 10 j = 1, nxmax
         write (6, 1060) j
 1060    format ('Row', i4)
         write (6, 1030) (fjac1(j,i), i = 1, nxmax)

   10 continue
   20 continue
     
      stop
      end
