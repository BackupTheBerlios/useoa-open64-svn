C                           DISCLAIMER
C
C   This file was generated on 06/29/00 by the version of
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
      subroutine g_dmsafg(g_p_, nx, ny, x, g_x, ldg_x, f, g_f, ldg_f, fg
     *rad, task, bottom, top, left, right)
        character*(*) task
        integer nx, ny
        real f
        real x(nx * ny), fgrad(nx * ny), bottom(nx + 2), top(nx + 2), le
     *ft(ny + 2), right(ny + 2)
C     **********
C
C     Subroutine dmsafg
C
C     This subroutine computes the function and gradient of the
C     minimal surface area problem.
C
C     The subroutine statement is
C
C       subroutine dmsafg(nx,ny,x,f,fgrad,task,bottom,top,left,right)
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
C            coordinate direction.
C         On exit ny is unchanged.
C
C       x is a real array of dimension nx*ny.
C         On entry x specifies the vector x if task = 'F', 'G', or 'FG'.
C            Otherwise x need not be specified.
C         On exit x is unchanged if task = 'F', 'G', or 'FG'. Otherwise
C            x is set according to task.
C
C       f is a real variable.
C         On entry f need not be specified.
C         On exit f is set to the function evaluated at x if task = 'F'
C            or 'FG'.
C
C       fgrad is a real array of dimension nx*ny.
C         On entry fgrad need not be specified.
C         On exit fgrad contains the gradient evaluated at x if
C            task = 'G' or 'FG'.
C
C       task is a character variable.
C         On entry task specifies the action of the subroutine:
C
C            task               action
C            ----               ------
C             'F'     Evaluate the function at x.
C             'G'     Evaluate the gradient at x.
C             'FG'    Evaluate the function and the gradient at x.
C             'XS'    Set x to the standard starting point xs.
C
C         On exit task is unchanged.
C
C       bottom is a real array of dimension nx + 2.
C         On entry bottom must contain boundary data beginning
C            with the lower left corner of the domain.
C         On exit bottom is unchanged.
C
C       top is a real array of dimension nx + 2.
C         On entry top must contain boundary data beginning with
C            the upper left corner of the domain.
C         On exit top is unchanged.
C
C       left is a real array of dimension ny + 2.
C         On entry left must contain boundary data beginning with
C            the lower left corner of the domain.
C         On exit left is unchanged.
C
C       right is a real array of dimension ny + 2.
C         On entry right must contain boundary data beginning with
C            the lower right corner of the domain.
C         On exit right is unchanged.
C
C     MINPACK-2 Project. November 1993.
C     Argonne National Laboratory and University of Minnesota.
C     Brett M. Averick.
C
C     **********
        real one, p5, two, zero
        parameter (zero = 0.0d0, p5 = 0.5d0, one = 1.0d0, two = 2.0d0)
C
        logical feval, geval
        integer i, j, k
        real alphaj, area, betai, dvdx, dvdy, fl, fu, hx, hy, v, vb, vl,
     * vr, vt, xline, yline
C
C     Initialize.
C
        integer g_pmax_
        parameter (g_pmax_ = 16)
        integer g_i_, g_p_, ldg_x, ldg_f
        real r2_p, r1_p, r1_w, r2_b, r2_v, r5_v, g_x(ldg_x, nx * ny), g_
     *f(ldg_f), g_v(g_pmax_), g_vr(g_pmax_)
        real g_vt(g_pmax_), g_dvdx(g_pmax_), g_dvdy(g_pmax_), g_r1_w(g_p
     *max_), g_fl(g_pmax_), g_vb(g_pmax_), g_vl(g_pmax_), g_fu(g_pmax_)

        save g_v, g_vr, g_vt, g_dvdx, g_dvdy, g_r1_w, g_fl, g_vb, g_vl, 
     *g_fu

C

C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        hx = one / dble(nx + 1)
        hy = one / dble(ny + 1)
        area = p5 * hx * hy
C
C     Compute the standard starting point if task = 'XS'.
C
        if (task .eq. 'XS') then
          do 99998 j = 1, ny
            alphaj = dble(j) * hy
            do 99999 i = 1, nx
              k = nx * (j - 1) + i
              betai = dble(i) * hx
              yline = alphaj * top(i + 1) + (one - alphaj) * bottom(i + 
     *1)
              xline = betai * right(j + 1) + (one - betai) * left(j + 1)
              do g_i_ = 1, g_p_
                g_x(g_i_, k) = 0.0
              enddo
              x(k) = (yline + xline) / two
C--------
10            continue
99999       continue
20          continue
99998     continue
C
          return
C
        endif
C
        if (task .eq. 'F' .or. task .eq. 'FG') then
          feval = .true.
        else
          feval = .false.
        endif
        if (task .eq. 'G' .or. task .eq. 'FG') then
          geval = .true.
        else
          geval = .false.
        endif
C
C     Evaluate the function if task = 'F', the gradient if task = 'G',
C     or both if task = 'FG'.
C
        if (feval) then
          do g_i_ = 1, g_p_
            g_f(g_i_) = 0.0
          enddo
          f = zero
C--------
        endif
        if (geval) then
          do 99997 k = 1, nx * ny
            fgrad(k) = zero
30          continue
99997     continue
        endif
C
C     Computation of the function and gradient over the lower
C     triangular elements.
C
        do 99995 j = 0, ny
          do 99996 i = 0, nx
            k = nx * (j - 1) + i
            if (i .ge. 1 .and. j .ge. 1) then
              do g_i_ = 1, g_p_
                g_v(g_i_) = g_x(g_i_, k)
              enddo
              v = x(k)
C--------
            else
              if (j .eq. 0) then
                do g_i_ = 1, g_p_
                  g_v(g_i_) = 0.0
                enddo
                v = bottom(i + 1)
C--------
              endif
              if (i .eq. 0) then
                do g_i_ = 1, g_p_
                  g_v(g_i_) = 0.0
                enddo
                v = left(j + 1)
C--------
              endif
            endif
            if (i .lt. nx .and. j .gt. 0) then
              do g_i_ = 1, g_p_
                g_vr(g_i_) = g_x(g_i_, k + 1)
              enddo
              vr = x(k + 1)
C--------
            else
              if (i .eq. nx) then
                do g_i_ = 1, g_p_
                  g_vr(g_i_) = 0.0
                enddo
                vr = right(j + 1)
C--------
              endif
              if (j .eq. 0) then
                do g_i_ = 1, g_p_
                  g_vr(g_i_) = 0.0
                enddo
                vr = bottom(i + 2)
C--------
              endif
            endif
            if (i .gt. 0 .and. j .lt. ny) then
              do g_i_ = 1, g_p_
                g_vt(g_i_) = g_x(g_i_, k + nx)
              enddo
              vt = x(k + nx)
C--------
            else
              if (i .eq. 0) then
                do g_i_ = 1, g_p_
                  g_vt(g_i_) = 0.0
                enddo
                vt = left(j + 2)
C--------
              endif
              if (j .eq. ny) then
                do g_i_ = 1, g_p_
                  g_vt(g_i_) = 0.0
                enddo
                vt = top(i + 1)
C--------
              endif
            endif
            r2_b = 1.0 / hx
            do g_i_ = 1, g_p_
              g_dvdx(g_i_) = r2_b * g_vr(g_i_) + (-r2_b) * g_v(g_i_)
            enddo
            dvdx = (vr - v) / hx
C--------
            r2_b = 1.0 / hy
            do g_i_ = 1, g_p_
              g_dvdy(g_i_) = r2_b * g_vt(g_i_) + (-r2_b) * g_v(g_i_)
            enddo
            dvdy = (vt - v) / hy
C--------
            r2_v = dvdx * dvdx
            r2_p = 2.0e0 * dvdx
            r5_v = dvdy * dvdy
            r1_p = 2.0e0 * dvdy
            do g_i_ = 1, g_p_
              g_r1_w(g_i_) = r2_p * g_dvdx(g_i_) + r1_p * g_dvdy(g_i_)
            enddo
            r1_w = one + r2_v + r5_v
            r2_v = sqrt(r1_w)

            if ( r1_w .gt. 0.0e0 ) then
               r1_p = 1.0e0 / (2.0e0 *  r2_v)
            else
               r1_p = 0.0e0
            endif
            do g_i_ = 1, g_p_
              g_fl(g_i_) = r1_p * g_r1_w(g_i_)
            enddo
            fl = r2_v
C--------
            if (feval) then
              do g_i_ = 1, g_p_
                g_f(g_i_) = g_f(g_i_) + g_fl(g_i_)
              enddo
              f = f + fl
C--------
            endif
            if (geval) then
              if (i .ge. 1 .and. j .ge. 1) then
                fgrad(k) = fgrad(k) - (dvdx / hx + dvdy / hy) / fl
              endif
              if (i .lt. nx .and. j .gt. 0) then
                fgrad(k + 1) = fgrad(k + 1) + (dvdx / hx) / fl
              endif
              if (i .gt. 0 .and. j .lt. ny) then
                fgrad(k + nx) = fgrad(k + nx) + (dvdy / hy) / fl
              endif
            endif
40          continue
99996     continue
50        continue
99995   continue
C
C     Computation of the function and the gradient over the upper
C     triangular elements.
C
        do 99993 j = 1, ny + 1
          do 99994 i = 1, nx + 1
            k = nx * (j - 1) + i
            if (i .le. nx .and. j .gt. 1) then
              do g_i_ = 1, g_p_
                g_vb(g_i_) = g_x(g_i_, k - nx)
              enddo
              vb = x(k - nx)
C--------
            else
              if (j .eq. 1) then
                do g_i_ = 1, g_p_
                  g_vb(g_i_) = 0.0
                enddo
                vb = bottom(i + 1)
C--------
              endif
              if (i .eq. nx + 1) then
                do g_i_ = 1, g_p_
                  g_vb(g_i_) = 0.0
                enddo
                vb = right(j)
C--------
              endif
            endif
            if (i .gt. 1 .and. j .le. ny) then
              do g_i_ = 1, g_p_
                g_vl(g_i_) = g_x(g_i_, k - 1)
              enddo
              vl = x(k - 1)
C--------
            else
              if (j .eq. ny + 1) then
                do g_i_ = 1, g_p_
                  g_vl(g_i_) = 0.0
                enddo
                vl = top(i)
C--------
              endif
              if (i .eq. 1) then
                do g_i_ = 1, g_p_
                  g_vl(g_i_) = 0.0
                enddo
                vl = left(j + 1)
C--------
              endif
            endif
            if (i .le. nx .and. j .le. ny) then
              do g_i_ = 1, g_p_
                g_v(g_i_) = g_x(g_i_, k)
              enddo
              v = x(k)
C--------
            else
              if (i .eq. nx + 1) then
                do g_i_ = 1, g_p_
                  g_v(g_i_) = 0.0
                enddo
                v = right(j + 1)
C--------
              endif
              if (j .eq. ny + 1) then
                do g_i_ = 1, g_p_
                  g_v(g_i_) = 0.0
                enddo
                v = top(i + 1)
C--------
              endif
            endif
            r2_b = 1.0 / hx
            do g_i_ = 1, g_p_
              g_dvdx(g_i_) = r2_b * g_v(g_i_) + (-r2_b) * g_vl(g_i_)
            enddo
            dvdx = (v - vl) / hx
C--------
            r2_b = 1.0 / hy
            do g_i_ = 1, g_p_
              g_dvdy(g_i_) = r2_b * g_v(g_i_) + (-r2_b) * g_vb(g_i_)
            enddo
            dvdy = (v - vb) / hy
C--------
            r2_v = dvdx * dvdx
            r2_p = 2.0e0 * dvdx
            r5_v = dvdy * dvdy
            r1_p = 2.0e0 * dvdy
            do g_i_ = 1, g_p_
              g_r1_w(g_i_) = r2_p * g_dvdx(g_i_) + r1_p * g_dvdy(g_i_)
            enddo
            r1_w = one + r2_v + r5_v
            r2_v = sqrt(r1_w)

            if ( r1_w .gt. 0.0e0 ) then
               r1_p = 1.0e0 / (2.0e0 *  r2_v)
            else
               r1_p = 0.0e0
            endif
            do g_i_ = 1, g_p_
              g_fu(g_i_) = r1_p * g_r1_w(g_i_)
            enddo
            fu = r2_v
C--------
            if (feval) then
              do g_i_ = 1, g_p_
                g_f(g_i_) = g_f(g_i_) + g_fu(g_i_)
              enddo
              f = f + fu
C--------
            endif
            if (geval) then
              if (i .le. nx .and. j .gt. 1) then
                fgrad(k - nx) = fgrad(k - nx) - (dvdy / hy) / fu
              endif
              if (i .gt. 1 .and. j .le. ny) then
                fgrad(k - 1) = fgrad(k - 1) - (dvdx / hx) / fu
              endif
              if (i .le. nx .and. j .le. ny) then
                fgrad(k) = fgrad(k) + (dvdx / hx + dvdy / hy) / fu
              endif
            endif
60          continue
99994     continue
70        continue
99993   continue
C
C     Scale the function and the gradient.
C
        if (feval) then
          do g_i_ = 1, g_p_
            g_f(g_i_) = area * g_f(g_i_)
          enddo
          f = area * f
C--------
        endif
        if (geval) then
          do 99992 k = 1, nx * ny
            fgrad(k) = area * fgrad(k)
80          continue
99992     continue
        endif
C
      end
