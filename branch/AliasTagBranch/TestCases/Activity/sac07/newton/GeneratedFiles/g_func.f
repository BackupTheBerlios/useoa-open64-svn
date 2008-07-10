C                           DISCLAIMER
C
C   This file was generated on 11/12/98 by the version of
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
      subroutine g_func(g_p_, x, g_x, ldg_x, y, g_y, ldg_y)
        double precision x(2), y(2)
C
        integer g_pmax_
        parameter (g_pmax_ = 2)
        integer g_i_, g_p_, ldg_y, ldg_x
        double precision d5_b, d2_b, g_y(ldg_y, 2), g_x(ldg_x, 2)
        integer g_ehfid
        intrinsic dble
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'func','g_func.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        d2_b = dble(10.0)
        d5_b = (-d2_b) * x(1) + (-d2_b) * x(1)
        do g_i_ = 1, g_p_
          g_y(g_i_, 1) = d5_b * g_x(g_i_, 1) + d2_b * g_x(g_i_, 2)
        enddo
        y(1) = dble(10.0) * (x(2) - x(1) * x(1))
C--------
        do g_i_ = 1, g_p_
          g_y(g_i_, 2) = -g_x(g_i_, 1)
        enddo
        y(2) = 1.0d0 - x(1)
C--------
        return
      end
