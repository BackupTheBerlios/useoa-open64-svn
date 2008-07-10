      subroutine dmsamain(nx,ny,x,f,g,task,w)
      character*(*) task
      integer ny,nx
      real f
      real x(nx*ny),g(nx*ny),w(nx*ny)
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
c       x is a real array of dimension n.
c         On entry x specifies the vector x if the function
c            or the gradient need to be evaluated. Otherwise 
c            x need not be specified.
c         On exit x is unchanged if the function or the gradient
c            need to be evaluated.
c            Otherwise x is set according to task.
c
c       f is a real variable.
c         On entry f need not be specified.
c         On exit f is set to the function evaluated at x if 
c            task = 'F' or 'FG'..
c
c       g is a real array of dimension n.
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
c       w is a real array of dimension 4*(nx+1)*(ny+1).
c
c     MINPACK-2 Project. September 1993.
c     Argonne National Laboratory.
c     Brett M. Averick and Jorge J. More'.
c
c     **********

      call dmsabc(nx,ny,w(1),w(nx+3),w(2*nx+5),w(2*nx+ny+7))
      call dmsafg(nx,ny,x,f,g,task,w(1),w(nx+3),w(2*nx+5),w(2*nx+ny+7))

      return
      end
