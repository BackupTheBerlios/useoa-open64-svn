C                           DISCLAIMER
C
C   This file was generated on 05/23/96 by the version of
C   ADIFOR compiled on Aug 21 1995.
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
      subroutine g_dsfdfj(g_p_, n, x, g_x, ldg_x, fvec, g_fvec, ldg_fvec
     *, fjac, ldfjac, task, eps, nint)
        character*(*) task
        integer n, ldfjac, nint
        double precision eps
        double precision x(n), fvec(n), fjac(ldfjac, n)
C     **********
C
C     Subroutine dsfdfj
C
C     This subroutine computes the function and Jacobian matrix of the
C     swirling flow between disks problem.
C
C     The subroutine statement is
C
C       subroutine dsfdfj(n,x,fvec,fjac,ldfjac,task,eps,nint)
C
C     where
C
C       n is an integer variable.
C         On entry n is the number of variables. n = 14*nint.
C         On exit n is unchanged.
C
C       x is a double precision array of dimension n.
C         On entry x specifies the vector x if task = 'F', 'J', or 'FJ'.
C            Otherwise x need not be specified.
C         On exit x is unchanged if task = 'F', 'J', or 'FJ'. Otherwise
C            x is set according to task.
C
C       fvec is a double precision array of dimension n.
C         On entry fvec need not be specified.
C         On exit fvec contains the function evaluated at x if
C            task = 'F' or 'FJ'.
C
C       fjac is a double precision array of dimension (ldfjac,n).
C         On entry fjac need not be specified.
C         On exit fjac contains the Jacobian matrix evaluated at x if
C            task = 'J' or 'FJ'.
C
C       ldfjac is an integer variable.
C          On entry ldfjac is the leading dimension of fjac.
C          On exit ldfjac is unchanged.
C
C       task is a character variable.
C         On entry task specifies the action of the subroutine:
C
C            task               action
C            ----               ------
C             'F'     Evaluate the function at x.
C             'J'     Evaluate the Jacobian matrix at x.
C             'FJ'    Evaluate the function and the Jacobian at x.
C             'XS'    Set x to the standard starting point xs.
C
C         On exit task is unchanged.
C
C       eps is a double precision variable.
C         On entry eps is the viscosity of the fluid.
C         On exit eps is unchanged.
C
C       nint is an integer variable.
C         On entry nint is the number of subintervals in the k-stage
C            collocation.
C         On exit nint is unchanged.
C
C     MINPACK-2 Project. November 1993.
C     Argonne National Laboratory and University of Minnesota.
C     Brett M. Averick.
C
C     **********
        integer bc, cpts, dim, fdeg, gdeg, mdeg, npi
        parameter (bc = 3, cpts = 4, fdeg = 4, gdeg = 2, mdeg = 4)
        parameter (dim = mdeg + cpts - 1, npi = 2 * cpts + gdeg + fdeg)
        double precision omega1, omega2, one, zero
        parameter (zero = 0.0d0, one = 1.0d0)
        parameter (omega1 = -1.0d0, omega2 = 1.0d0)
C
        logical feval, jeval
        integer eqn1, eqn2, i, j, k, m, var1, var2
        double precision h, hm, nf, rhoijh, xt
        double precision dwf(fdeg + 1, cpts + fdeg), dwg(gdeg + 1, cpts 
     *+ gdeg), rhnfhk(cpts, 0:dim, 0:dim, 0:mdeg), rho(cpts), wg(gdeg + 
     *1), wf(fdeg + 1)
C
        integer g_pmax_
        parameter (g_pmax_ = 56)
        integer g_i_, g_p_, ldg_x, ldg_fvec
        double precision g_x(ldg_x, n), g_fvec(ldg_fvec, n), g_wf(g_pmax
     *_, fdeg + 1), g_wg(g_pmax_, gdeg + 1)
        save g_wf, g_wg
        data (rho(i), i = 1, cpts) /0.694318413734436035d-1, 0.330009490
     *251541138d0, 0.669990539550781250d0, 0.930568158626556396d0/
C
C     Check input arguments for errors.
C
        integer g_ehfid
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'dsfdfj','g_dsfdfj.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        if (n .ne. 14 * nint) then
          task = 'ERROR: N .NE. 14*NINT IN DSFDFJ'
C
          return
C
        endif
C
C     Initialization.
C
        h = one / dble(nint)
C
C     Compute the standard starting point if task = 'XS'.
C
        if (task .eq. 'XS') then
          do 99999 i = 1, n
            do g_i_ = 1, g_p_
              g_x(g_i_, i) = 0.0d0
            enddo
            x(i) = zero
C--------
10          continue
99999     continue
C
C        The standard starting point corresponds to the solution
C        of the swirling flow problem with infinite viscosity.
C
          xt = zero
          do 99998 i = 1, nint
            var1 = (i - 1) * npi + fdeg + cpts
            do g_i_ = 1, g_p_
              g_x(g_i_, var1 + 1) = 0.0d0
            enddo
            x(var1 + 1) = omega1 + (omega2 - omega1) * xt
C--------
            do g_i_ = 1, g_p_
              g_x(g_i_, var1 + 2) = 0.0d0
            enddo
            x(var1 + 2) = omega2 - omega1
C--------
            xt = xt + h
20          continue
99998     continue
C
          return
C
        endif
C
C     Store all possible combinations of rho, h, and n factorial.
C
        hm = one
        do 99994 m = 0, mdeg
          do 99995 i = 1, cpts
            rhoijh = hm
            do 99996 j = 0, dim
              nf = one
              do 99997 k = 0, dim
                rhnfhk(i, j, k, m) = rhoijh / nf
                nf = nf * dble(k + 1)
30              continue
99997         continue
              rhoijh = rhoijh * rho(i)
40            continue
99996       continue
50          continue
99995     continue
          hm = hm * h
60        continue
99994   continue
C
        if (task .eq. 'F' .or. task .eq. 'FJ') then
          feval = .true.
        else
          feval = .false.
        endif
        if (task .eq. 'J' .or. task .eq. 'FJ') then
          jeval = .true.
        else
          jeval = .false.
        endif
C
C     Evaluate the function if task = 'F', the Jacobian matrix if
C     task = 'J', or both if task = 'FJ'.
C
C     Initialize arrays.
C
        if (feval) then
          do 99993 j = 1, n
            do g_i_ = 1, g_p_
              g_fvec(g_i_, j) = 0.0d0
            enddo
            fvec(j) = zero
C--------
70          continue
99993     continue
        endif
        if (jeval) then
          do 99991 j = 1, n
            do 99992 i = 1, n
              fjac(i, j) = zero
80            continue
99992       continue
90          continue
99991     continue
        endif
        do 99989 k = 1, cpts + fdeg
          do 99990 j = 1, fdeg + 1
            dwf(j, k) = zero
100         continue
99990     continue
110       continue
99989   continue
        do 99987 k = 1, cpts + gdeg
          do 99988 j = 1, gdeg + 1
            dwg(j, k) = zero
120         continue
99988     continue
130       continue
99987   continue
C
C     Set up the boundary equations at t = 0.
C     f(0) = 0, f'(0) = 0, g(0) = omega1.
C
        if (feval) then
          do g_i_ = 1, g_p_
            g_fvec(g_i_, 1) = g_x(g_i_, 1)
          enddo
          fvec(1) = x(1)
C--------
          do g_i_ = 1, g_p_
            g_fvec(g_i_, 2) = g_x(g_i_, 2)
          enddo
          fvec(2) = x(2)
C--------
          do g_i_ = 1, g_p_
            g_fvec(g_i_, 3) = g_x(g_i_, cpts + fdeg + 1)
          enddo
          fvec(3) = x(cpts + fdeg + 1) - omega1
C--------
        endif
        if (jeval) then
          fjac(1, 1) = one
          fjac(2, 2) = one
          fjac(3, cpts + fdeg + 1) = one
        endif
C
C     Set up the collocation equations.
C
        do 99977 i = 1, nint
          var1 = (i - 1) * npi
          eqn1 = var1 + bc
          var2 = var1 + cpts + fdeg
          eqn2 = eqn1 + cpts
          do 99978 k = 1, cpts
            do 99984 m = 1, fdeg + 1
              do g_i_ = 1, g_p_
                g_wf(g_i_, m) = 0.0d0
              enddo
              wf(m) = zero
C--------
              do 99986 j = m, fdeg
                do g_i_ = 1, g_p_
                  g_wf(g_i_, m) = rhnfhk(k, j - m, j - m, j - m) * g_x(g
     *_i_, var1 + j) + g_wf(g_i_, m)
                enddo
                wf(m) = wf(m) + rhnfhk(k, j - m, j - m, j - m) * x(var1 
     *+ j)
C--------
                dwf(m, j) = rhnfhk(k, j - m, j - m, j - m)
140             continue
99986         continue
              do 99985 j = 1, cpts
                do g_i_ = 1, g_p_
                  g_wf(g_i_, m) = rhnfhk(k, fdeg + j - m, fdeg + j - m, 
     *fdeg - m + 1) * g_x(g_i_, var1 + fdeg + j) + g_wf(g_i_, m)
                enddo
                wf(m) = wf(m) + x(var1 + fdeg + j) * rhnfhk(k, fdeg + j 
     *- m, fdeg + j - m, fdeg - m + 1)
C--------
                dwf(m, fdeg + j) = rhnfhk(k, fdeg + j - m, fdeg + j - m,
     * fdeg - m + 1)
150             continue
99985         continue
160           continue
99984       continue
            do 99981 m = 1, gdeg + 1
              do g_i_ = 1, g_p_
                g_wg(g_i_, m) = 0.0d0
              enddo
              wg(m) = zero
C--------
              do 99983 j = m, gdeg
                do g_i_ = 1, g_p_
                  g_wg(g_i_, m) = rhnfhk(k, j - m, j - m, j - m) * g_x(g
     *_i_, var2 + j) + g_wg(g_i_, m)
                enddo
                wg(m) = wg(m) + rhnfhk(k, j - m, j - m, j - m) * x(var2 
     *+ j)
C--------
                dwg(m, j) = rhnfhk(k, j - m, j - m, j - m)
170             continue
99983         continue
              do 99982 j = 1, cpts
                do g_i_ = 1, g_p_
                  g_wg(g_i_, m) = rhnfhk(k, gdeg + j - m, gdeg + j - m, 
     *gdeg - m + 1) * g_x(g_i_, var2 + gdeg + j) + g_wg(g_i_, m)
                enddo
                wg(m) = wg(m) + x(var2 + gdeg + j) * rhnfhk(k, gdeg + j 
     *- m, gdeg + j - m, gdeg - m + 1)
C--------
                dwg(m, gdeg + j) = rhnfhk(k, gdeg + j - m, gdeg + j - m,
     * gdeg - m + 1)
180             continue
99982         continue
190           continue
99981       continue
            if (feval) then
              do g_i_ = 1, g_p_
                g_fvec(g_i_, eqn1 + k) = wg(2) * g_wg(g_i_, 1) + wg(1) *
     * g_wg(g_i_, 2) + wf(4) * g_wf(g_i_, 1) + wf(1) * g_wf(g_i_, 4) + e
     *ps * g_wf(g_i_, 5)
              enddo
              fvec(eqn1 + k) = eps * wf(5) + wf(4) * wf(1) + wg(2) * wg(
     *1)
C--------
              do g_i_ = 1, g_p_
                g_fvec(g_i_, eqn2 + k) = -wf(2) * g_wg(g_i_, 1) + (-wg(1
     *)) * g_wf(g_i_, 2) + wf(1) * g_wg(g_i_, 2) + wg(2) * g_wf(g_i_, 1)
     * + eps * g_wg(g_i_, 3)
              enddo
              fvec(eqn2 + k) = eps * wg(3) + wf(1) * wg(2) - wf(2) * wg(
     *1)
C--------
            endif
            if (jeval) then
              do 99980 j = 1, cpts + fdeg
                fjac(eqn1 + k, var1 + j) = eps * dwf(5, j) + dwf(4, j) *
     * wf(1) + wf(4) * dwf(1, j)
                fjac(eqn2 + k, var1 + j) = dwf(1, j) * wg(2) - dwf(2, j)
     * * wg(1)
200             continue
99980         continue
              do 99979 j = 1, cpts + gdeg
                fjac(eqn1 + k, var2 + j) = dwg(2, j) * wg(1) + wg(2) * d
     *wg(1, j)
                fjac(eqn2 + k, var2 + j) = eps * dwg(3, j) + wf(1) * dwg
     *(2, j) - wf(2) * dwg(1, j)
210             continue
99979         continue
            endif
C
220         continue
99978     continue
230       continue
99977   continue
C
C     Set up the continuity equations.
C
        do 99966 i = 1, nint - 1
          var1 = (i - 1) * npi
          eqn1 = var1 + bc + 2 * cpts
          var2 = var1 + fdeg + cpts
          eqn2 = eqn1 + fdeg
          do 99974 m = 1, fdeg
            do g_i_ = 1, g_p_
              g_wf(g_i_, m) = 0.0d0
            enddo
            wf(m) = zero
C--------
            do 99976 j = m, fdeg
              do g_i_ = 1, g_p_
                g_wf(g_i_, m) = rhnfhk(1, 0, j - m, j - m) * g_x(g_i_, v
     *ar1 + j) + g_wf(g_i_, m)
              enddo
              wf(m) = wf(m) + rhnfhk(1, 0, j - m, j - m) * x(var1 + j)
C--------
              dwf(m, j) = rhnfhk(1, 0, j - m, j - m)
240           continue
99976       continue
            do 99975 j = 1, cpts
              do g_i_ = 1, g_p_
                g_wf(g_i_, m) = rhnfhk(1, 0, fdeg + j - m, fdeg - m + 1)
     * * g_x(g_i_, var1 + fdeg + j) + g_wf(g_i_, m)
              enddo
              wf(m) = wf(m) + rhnfhk(1, 0, fdeg + j - m, fdeg - m + 1) *
     * x(var1 + fdeg + j)
C--------
              dwf(m, fdeg + j) = rhnfhk(1, 0, fdeg + j - m, fdeg - m + 1
     *)
250           continue
99975       continue
260         continue
99974     continue
          do 99971 m = 1, gdeg
            do g_i_ = 1, g_p_
              g_wg(g_i_, m) = 0.0d0
            enddo
            wg(m) = zero
C--------
            do 99973 j = m, gdeg
              do g_i_ = 1, g_p_
                g_wg(g_i_, m) = rhnfhk(1, 0, j - m, j - m) * g_x(g_i_, v
     *ar2 + j) + g_wg(g_i_, m)
              enddo
              wg(m) = wg(m) + rhnfhk(1, 0, j - m, j - m) * x(var2 + j)
C--------
              dwg(m, j) = rhnfhk(1, 0, j - m, j - m)
270           continue
99973       continue
            do 99972 j = 1, cpts
              do g_i_ = 1, g_p_
                g_wg(g_i_, m) = rhnfhk(1, 0, gdeg + j - m, gdeg - m + 1)
     * * g_x(g_i_, var2 + gdeg + j) + g_wg(g_i_, m)
              enddo
              wg(m) = wg(m) + rhnfhk(1, 0, gdeg + j - m, gdeg - m + 1) *
     * x(var2 + gdeg + j)
C--------
              dwg(m, gdeg + j) = rhnfhk(1, 0, gdeg + j - m, gdeg - m + 1
     *)
280           continue
99972       continue
290         continue
99971     continue
          do 99969 m = 1, fdeg
            if (feval) then
              do g_i_ = 1, g_p_
                g_fvec(g_i_, eqn1 + m) = -g_wf(g_i_, m) + g_x(g_i_, var1
     * + npi + m)
              enddo
              fvec(eqn1 + m) = x(var1 + npi + m) - wf(m)
C--------
            endif
            if (jeval) then
              fjac(eqn1 + m, var1 + npi + m) = one
              do 99970 j = 1, cpts + fdeg
                fjac(eqn1 + m, var1 + j) = -dwf(m, j)
300             continue
99970         continue
            endif
310         continue
99969     continue
          do 99967 m = 1, gdeg
            if (feval) then
              do g_i_ = 1, g_p_
                g_fvec(g_i_, eqn2 + m) = -g_wg(g_i_, m) + g_x(g_i_, var2
     * + npi + m)
              enddo
              fvec(eqn2 + m) = x(var2 + npi + m) - wg(m)
C--------
            endif
            if (jeval) then
              fjac(eqn2 + m, var2 + npi + m) = one
              do 99968 j = 1, cpts + gdeg
                fjac(eqn2 + m, var2 + j) = -dwg(m, j)
320             continue
99968         continue
            endif
330         continue
99967     continue
340       continue
99966   continue
C
C     Prepare for setting up the boundary conditions at t = 1.
C
        var1 = n - npi
        do 99963 m = 1, fdeg + 1
          do g_i_ = 1, g_p_
            g_wf(g_i_, m) = 0.0d0
          enddo
          wf(m) = zero
C--------
          do 99965 j = m, fdeg
            do g_i_ = 1, g_p_
              g_wf(g_i_, m) = rhnfhk(1, 0, j - m, j - m) * g_x(g_i_, var
     *1 + j) + g_wf(g_i_, m)
            enddo
            wf(m) = wf(m) + rhnfhk(1, 0, j - m, j - m) * x(var1 + j)
C--------
            dwf(m, j) = rhnfhk(1, 0, j - m, j - m)
350         continue
99965     continue
          do 99964 j = 1, cpts
            do g_i_ = 1, g_p_
              g_wf(g_i_, m) = rhnfhk(1, 0, fdeg + j - m, fdeg - m + 1) *
     * g_x(g_i_, var1 + fdeg + j) + g_wf(g_i_, m)
            enddo
            wf(m) = wf(m) + rhnfhk(1, 0, fdeg + j - m, fdeg - m + 1) * x
     *(var1 + fdeg + j)
C--------
            dwf(m, fdeg + j) = rhnfhk(1, 0, fdeg + j - m, fdeg - m + 1)
360         continue
99964     continue
370       continue
99963   continue
        var2 = var1 + fdeg + cpts
        do 99960 m = 1, gdeg + 1
          do g_i_ = 1, g_p_
            g_wg(g_i_, m) = 0.0d0
          enddo
          wg(m) = zero
C--------
          do 99962 j = m, gdeg
            do g_i_ = 1, g_p_
              g_wg(g_i_, m) = rhnfhk(1, 0, j - m, j - m) * g_x(g_i_, var
     *2 + j) + g_wg(g_i_, m)
            enddo
            wg(m) = wg(m) + rhnfhk(1, 0, j - m, j - m) * x(var2 + j)
C--------
            dwg(m, j) = rhnfhk(1, 0, j - m, j - m)
380         continue
99962     continue
          do 99961 j = 1, cpts
            do g_i_ = 1, g_p_
              g_wg(g_i_, m) = rhnfhk(1, 0, gdeg + j - m, gdeg - m + 1) *
     * g_x(g_i_, var2 + gdeg + j) + g_wg(g_i_, m)
            enddo
            wg(m) = wg(m) + rhnfhk(1, 0, gdeg + j - m, gdeg - m + 1) * x
     *(var2 + gdeg + j)
C--------
            dwg(m, gdeg + j) = rhnfhk(1, 0, gdeg + j - m, gdeg - m + 1)
390         continue
99961     continue
400       continue
99960   continue
C
C     Set up the boundary equations at t = 1.
C     f(1) = 0, f'(1) = 0, g(1) = omega2.
C
        if (feval) then
          do g_i_ = 1, g_p_
            g_fvec(g_i_, n - 2) = g_wf(g_i_, 1)
          enddo
          fvec(n - 2) = wf(1)
C--------
          do g_i_ = 1, g_p_
            g_fvec(g_i_, n - 1) = g_wf(g_i_, 2)
          enddo
          fvec(n - 1) = wf(2)
C--------
          do g_i_ = 1, g_p_
            g_fvec(g_i_, n) = g_wg(g_i_, 1)
          enddo
          fvec(n) = wg(1) - omega2
C--------
        endif
        if (jeval) then
          do 99959 j = 1, cpts + fdeg
            fjac(n - 2, var1 + j) = dwf(1, j)
            fjac(n - 1, var1 + j) = dwf(2, j)
410         continue
99959     continue
          do 99958 j = 1, cpts + gdeg
            fjac(n, var2 + j) = dwg(1, j)
420         continue
99958     continue
        endif
C
      end
