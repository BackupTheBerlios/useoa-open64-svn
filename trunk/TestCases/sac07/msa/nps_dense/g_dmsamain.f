C                           DISCLAIMER
C
C   This file was generated on 03/05/02 by the version of
C   ADIFOR compiled on June, 1998.
C
C   ADIFOR was prepared as an account of work sponsored by an
C   agency of the United States Government, Rice University, and
C   the University of Chicago.  NEITHER THE AUTHOR(S), THE UNITED
C   STATES GOVERNMENT NOR ANY AGENCY THEREOF, NOR RICE UNIVERSITY,
C   NOR THE UNIVERSITY OF CHICAGO, INCLUDING ANY OF THEIR EMPLOYEES
C   OR OFFICERS, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES
C   ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETE-
C   NESS, OR USEFULNESS OF ANY INFORMATION OR PROCESS DISCLOSED, OR
C   REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
C
      subroutine g_dmsamain(g_p_, nx, ny, x, g_x, ldg_x, f, g_f, ldg_f, 
     *g, task, w)
        character*(*) task
        integer ny, nx
        double precision f
        double precision x(nx * ny), g(nx * ny), w(nx * ny)
C     *********
C
C     Subroutine dmsamain
C
C     This subroutine is adopted from the subroutine dfmins,
C     the subroutine which selects the minimization problem from 
C     the MINPACK-2 test problem collection.  The reason we needed
C     to add dmsamain was so that a top level subroutine (dmsamain)
C     which accesses all dmsa routines could be called from ADIFOR.
C
C     Peyvand Khademi December 1994
C
C     The subroutine statement (based on documentation for dfmins) is
C
C       subroutine dsmamain(nx,ny,x,f,g,task,w)
C
C     where
C
C       nx is an integer variable.
C         On entry nx is the number of grid points in the first
C            coordinate direction.
C         On exit nx is unchanged.
C
C       ny is an integer variable.
C         On entry ny is the number of grid points in the second
C            coordinate direction. If the problem is formulated in
C            one spatial dimension, ny = 1.
C         On exit ny is unchanged.
C
C       x is a double precision array of dimension n.
C         On entry x specifies the vector x if the function
C            or the gradient need to be evaluated. Otherwise 
C            x need not be specified.
C         On exit x is unchanged if the function or the gradient
C            need to be evaluated.
C            Otherwise x is set according to task.
C
C       f is a double precision variable.
C         On entry f need not be specified.
C         On exit f is set to the function evaluated at x if 
C            task = 'F' or 'FG'..
C
C       g is a double precision array of dimension n.
C         On entry g need not be specified.
C         On exit g contains the gradient evaluated at x if 
C            task = 'G' or 'FG'.
C
C       task is a character variable of length atleast 42.
C         On entry task specifies the action of the subroutine:
C
C            task               action
C            ----               ------
C             'F'     Evaluate the function at x.
C             'G'     Evaluate the gradient vector at x.
C             'FG'    Evaluate the function and the gradient at x.
C             'XS'    Set x to the standard starting point xs.
C             'XL'    Set x to the lower bound xl.
C             'XU'    Set x to the upper bound xu.
C
C         On exit task is set to 'ERROR' if prob is not an acceptable
C            problem name. Oterwise task is unchanged.
C
C       w is a double precision array of dimension 4*(nx+1)*(ny+1).
C
C     MINPACK-2 Project. September 1993.
C     Argonne National Laboratory.
C     Brett M. Averick and Jorge J. More'.
C
C     **********
C
        integer g_pmax_
        parameter (g_pmax_ = 16)
        integer g_p_, ldg_x, ldg_f
        double precision g_x(ldg_x, nx * ny), g_f(ldg_f)

        external g_dmsafg

C

C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        call dmsabc(nx, ny, w(1), w(nx + 3), w(2 * nx + 5), w(2 * nx + n
     *y + 7))
        call g_dmsafg(g_p_, nx, ny, x, g_x, ldg_x, f, g_f, ldg_f, g, tas
     *k, w(1), w(nx + 3), w(2 * nx + 5), w(2 * nx + ny + 7))
C
        return
      end