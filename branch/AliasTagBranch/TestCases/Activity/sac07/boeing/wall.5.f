      subroutine g$wall$5(g$p$, eqns, g$eqns, ldg$eqns, m)
C       
C       Common block // contains active variables.
C       Variable hh in Common block // is active.
C       Variable gg in Common block // is active.
C       Variable ff in Common block // is active.
C       Variable eq in Common block // is active.
C       Variable u in Common block // is active.
C       Formal eqns is active.
C       
        integer g$p$
        integer g$pmax$
        parameter (g$pmax$ = 190)
        integer g$i$
        double precision d$7bar
        double precision zbar
        double precision d$6bar
        double precision d$5bar
        double precision d$4bar
        double precision d$6
        double precision d$4
        double precision d$3bar
        double precision d$3
        double precision d$1bar
        double precision d$0bar
        double precision vmbar
        double precision d$2
        double precision d$0
        double precision g$vx(g$pmax$)
        double precision g$ux(g$pmax$)
        double precision g$px(g$pmax$)
        double precision g$pff(g$pmax$)
        double precision g$vff(g$pmax$)
        double precision g$uff(g$pmax$)
        double precision g$pbb(g$pmax$)
        double precision g$vbb(g$pmax$)
        double precision g$ubb(g$pmax$)
        double precision g$vy(g$pmax$)
        double precision g$uy(g$pmax$)
        double precision g$py(g$pmax$)
        double precision g$cinhom(g$pmax$)
        double precision g$cvy(g$pmax$)
        double precision g$cuy(g$pmax$)
        double precision g$cpy(g$pmax$)
        double precision g$cvx(g$pmax$)
        double precision g$cux(g$pmax$)
        double precision g$cpx(g$pmax$)
        double precision g$z(g$pmax$)
        double precision g$z2(g$pmax$)
        double precision g$value(g$pmax$)
        double precision g$pb(g$pmax$)
        double precision g$vb(g$pmax$)
        double precision g$ub(g$pmax$)
        double precision g$pf(g$pmax$)
        double precision g$dum2(g$pmax$)
        double precision g$vf(g$pmax$)
        double precision g$uf(g$pmax$)
        double precision g$dum1(g$pmax$)
        double precision g$beta(g$pmax$)
        double precision g$xth(g$pmax$)
        double precision g$xr(g$pmax$)
        double precision g$qtan(g$pmax$)
        double precision g$pm(g$pmax$)
        double precision g$em(g$pmax$)
        double precision g$vm(g$pmax$)
        double precision g$um(g$pmax$)
        double precision g$dm(g$pmax$)
        double precision g$eqns(ldg$eqns, *)
        double precision g$xincr(g$pmax$)
        double precision g$xend(g$pmax$)
        integer g$icont(g$pmax$)
        double precision g$dtheta(g$pmax$)
        integer g$kprint(g$pmax$)
        double precision g$eqmax(g$pmax$)
        double precision g$emax(g$pmax$)
        integer g$nprint(g$pmax$)
        double precision g$alpha(g$pmax$)
        double precision g$sstol(g$pmax$)
        integer g$itmax(g$pmax$)
        integer g$kblk3(g$pmax$)
        integer g$kblk2(g$pmax$)
        integer g$ksize(g$pmax$)
        integer g$kblk(g$pmax$)
        integer g$kdiag(g$pmax$)
        double precision g$delta(g$pmax$)
        double precision g$eqmaxs(g$pmax$, 200)
        double precision g$emaxs(g$pmax$, 200)
        double precision g$dstag(g$pmax$)
        double precision g$swall(g$pmax$)
        double precision g$htot(g$pmax$)
        double precision g$ainc(g$pmax$)
        double precision g$einc(g$pmax$)
        double precision g$pinc(g$pmax$)
        double precision g$dinc(g$pmax$)
        double precision g$qinc(g$pmax$)
        integer g$mcncf(g$pmax$)
        integer g$ncf(g$pmax$)
        double precision g$xmach(g$pmax$)
        double precision g$gm1(g$pmax$)
        double precision g$gamma(g$pmax$)
        double precision g$theta1(g$pmax$)
        integer g$mc3(g$pmax$)
        integer g$mc2(g$pmax$)
        integer g$nc1(g$pmax$)
        integer g$mc1(g$pmax$)
        double precision g$ddy2(g$pmax$)
        double precision g$ddx2(g$pmax$)
        double precision g$ddy(g$pmax$)
        double precision g$ddx(g$pmax$)
        double precision g$dy(g$pmax$)
        double precision g$dx(g$pmax$)
        integer g$nc(g$pmax$)
        integer g$mc(g$pmax$)
        double precision g$hh(g$pmax$, 3, 6, 10)
        double precision g$gg(g$pmax$, 3, 6, 10)
        double precision g$ff(g$pmax$, 3, 6, 10)
        double precision g$bth(g$pmax$, 10)
        double precision g$b(g$pmax$, 10)
        double precision g$eqplus(g$pmax$, 190)
        double precision g$a(g$pmax$, 190, 95)
        double precision g$eqold(g$pmax$, 190)
        double precision g$eq(g$pmax$, 190)
        double precision g$u(g$pmax$, 190)
        integer ldg$eqns
C       
        implicit double precision (a-h, o-z)
        common // u(190), eq(190), eqold(190), a(190, 95), eqplus(190), 
     *b(10), bth(10), ff(3, 6, 10), gg(3, 6, 10), hh(3, 6, 10), mc, nc, 
     *dx, dy, ddx, ddy, ddx2, ddy2, mc1, nc1, mc2, mc3, theta1, gamma, g
     *m1, xmach, ncf, mcncf, qinc, dinc, pinc, einc, ainc, htot, swall, 
     *dstag, emaxs(200), eqmaxs(200), delta, kdiag, kblk, ksize, kblk2, 
     *kblk3, itmax, sstol, alpha, nprint, emax, eqmax, kprint, dtheta, i
     *cont, xend, xincr
C       dimension eqns(1)
        double precision eqns(*)
C       
C       evaluates eqns at wall
C       
        common /g$/ g$u, g$eq, g$eqold, g$a, g$eqplus, g$b, g$bth, g$ff,
     * g$gg, g$hh, g$mc, g$nc, g$dx, g$dy, g$ddx, g$ddy, g$ddx2, g$ddy2,
     * g$mc1, g$nc1, g$mc2, g$mc3, g$theta1, g$gamma, g$gm1, g$xmach, g$
     *ncf, g$mcncf, g$qinc, g$dinc, g$pinc, g$einc, g$ainc, g$htot, g$sw
     *all, g$dstag, g$emaxs, g$eqmaxs, g$delta, g$kdiag, g$kblk, g$ksize
     *, g$kblk2, g$kblk3, g$itmax, g$sstol, g$alpha, g$nprint, g$emax, g
     *$eqmax, g$kprint, g$dtheta, g$icont, g$xend, g$xincr
        if (g$p$ .gt. g$pmax$) then
          print *, 'Parameter g$p is greater than g$pmax.'
          stop
        endif
        mm = (m - 1) * ncf
        call g$var$252(g$p$, 1, m, dm, g$dm(1), g$pmax$, um, g$um(1), g$
     *pmax$, vm, g$vm(1), g$pmax$, em, g$em(1), g$pmax$, pm, g$pm(1), g$
     *pmax$)
        if (m .eq. 1) then
          goto 100
        endif
        bm = b(m)
        bm2 = bm * bm
        bthm = bth(m)
        bthb = bthm / bm
        dee2 = 1. + bthb ** 2
        dee = dsqrt(dee2)
C       qtan = (um * bthb + vm) / dee
        d$1bar = (1.0d0 / dee)
        do 99971 g$i$ = 1, g$p$
          g$qtan(g$i$) = d$1bar * bthb * g$um(g$i$) + d$1bar * g$vm(g$i$
     *)
99971   continue
        qtan = (um * bthb + vm) / dee
        call g$tran$140(g$p$, 1, m, xr, g$xr(1), g$pmax$, xth, g$xth(1),
     * g$pmax$, yr, yth, ajac)
C       beta = um * yr + vm * yth / bm
        do 99970 g$i$ = 1, g$p$
          g$beta(g$i$) = yr * g$um(g$i$) + (1.0d0 / bm) * yth * g$vm(g$i
     *$)
99970   continue
        beta = um * yr + vm * yth / bm
        call g$mwall$8(g$p$, m, mf, mb)
        call g$var$252(g$p$, 1, mf, dum1, g$dum1(1), g$pmax$, uf, g$uf(1
     *), g$pmax$, vf, g$vf(1), g$pmax$, dum2, g$dum2(1), g$pmax$, pf, g$
     *pf(1), g$pmax$)
        call g$var$252(g$p$, 1, mb, dum1, g$dum1(1), g$pmax$, ub, g$ub(1
     *), g$pmax$, vb, g$vb(1), g$pmax$, dum2, g$dum2(1), g$pmax$, pb, g$
     *pb(1), g$pmax$)
        deltax = 2. * dx
        deltay = (mf - mb) * dy
C       boundary condition
C       value = um - vm * bthb
        do 99969 g$i$ = 1, g$p$
          g$value(g$i$) = g$um(g$i$) + (-bthb * g$vm(g$i$))
99969   continue
        value = um - (vm * (bthb))
        eqns(mm + 1) = value
        do 99968 g$i$ = 1, g$p$
          g$eqns(g$i$, mm + 1) = g$value(g$i$)
99968   continue
C       entropy on wall=constant
C       eqns(mm + 2) = dlog(pm / pinc) - gamma * dlog(dm / dinc) - swall
        d$0 = pm / pinc
        d$2 = dm / dinc
        do 99967 g$i$ = 1, g$p$
          g$eqns(g$i$, mm + 2) = 1.0d0 / d$0 * (1.0d0 / pinc) * g$pm(g$i
     *$) + (-gamma / (d$2) * (1.0d0 / (dinc)) * g$dm(g$i$))
99967   continue
        eqns(mm + 2) = dlog(d$0) - gamma * dlog(d$2) - swall
C       l4 compat.
        call g$rth$16(g$p$, 1, m, dum, theta)
        cotth = 1. / dtan(theta)
C       z2 = gamma * gm1 * em
        do 99966 g$i$ = 1, g$p$
          g$z2(g$i$) = (gamma * gm1) * g$em(g$i$)
99966   continue
        z2 = gamma * gm1 * em
C       z = dsqrt(z2)
        d$0 = dsqrt(z2)
        do 99965 g$i$ = 1, g$p$
          g$z(g$i$) = 1.0d0 / (2 * d$0) * g$z2(g$i$)
99965   continue
        z = d$0
C       cpx = -z * xr * dee
        do 99964 g$i$ = 1, g$p$
          g$cpx(g$i$) = -(dee * xr) * g$z(g$i$) + dee * (-z) * g$xr(g$i$
     *)
99964   continue
        cpx = -z * (xr) * (dee)
C       cux = dm * z2 * xr
        d$0 = dm * z2
        do 99963 g$i$ = 1, g$p$
          g$cux(g$i$) = xr * z2 * g$dm(g$i$) + xr * dm * g$z2(g$i$) + d$
     *0 * g$xr(g$i$)
99963   continue
        cux = d$0 * xr
C       cvx = dm * z2 * xth / bm
        d$0 = dm * z2
        d$1bar = (1.0d0 / bm)
        d$0bar = d$1bar * xth
        do 99962 g$i$ = 1, g$p$
          g$cvx(g$i$) = d$0bar * z2 * g$dm(g$i$) + d$0bar * dm * g$z2(g$
     *i$) + d$1bar * d$0 * g$xth(g$i$)
99962   continue
        cvx = d$0 * xth / bm
C       cpy = beta + z * (-yr + bthb * yth / bm) / dee
        do 99961 g$i$ = 1, g$p$
          g$cpy(g$i$) = g$beta(g$i$) + (1.0d0 / dee) * (-yr + bthb * yth
     * / bm) * g$z(g$i$)
99961   continue
        cpy = beta + z * (-yr + bthb * yth / bm) / dee
C       cuy = dm * z * (z * yr - beta / dee)
        d$0 = dm * z
        d$3 = z * yr - beta / dee
        zbar = d$0 * yr
        zbar = zbar + d$3 * dm
        do 99960 g$i$ = 1, g$p$
          g$cuy(g$i$) = d$3 * z * g$dm(g$i$) + zbar * g$z(g$i$) + (-d$0 
     ** (1.0d0 / dee) * g$beta(g$i$))
99960   continue
        cuy = d$0 * (d$3)
C       cvy = dm * z * (z * yth + bthm * beta / dee) / bm
        d$0 = dm * z
        d$4 = z * yth + bthm * beta / dee
        d$5bar = (1.0d0 / bm)
        d$0bar = d$5bar * d$4
        d$4bar = d$5bar * d$0
        zbar = d$4bar * yth
        zbar = zbar + d$0bar * dm
        do 99959 g$i$ = 1, g$p$
          g$cvy(g$i$) = d$0bar * z * g$dm(g$i$) + zbar * g$z(g$i$) + d$4
     *bar * (1.0d0 / dee) * bthm * g$beta(g$i$)
99959   continue
        cvy = d$0 * d$4 / bm
C       cinhom = dm * z * (z * (2. * um + vm * cotth) + vm * qtan) / bm
        d$0 = dm * z
        d$3 = 2. * um + vm * cotth
        d$6 = z * d$3 + vm * qtan
        d$7bar = (1.0d0 / bm)
        d$0bar = d$7bar * d$6
        d$6bar = d$7bar * d$0
        vmbar = d$6bar * qtan
        zbar = d$6bar * d$3
        d$3bar = d$6bar * z
        vmbar = vmbar + d$3bar * cotth
        zbar = zbar + d$0bar * dm
        do 99958 g$i$ = 1, g$p$
          g$cinhom(g$i$) = d$0bar * z * g$dm(g$i$) + zbar * g$z(g$i$) + 
     *d$3bar * 2. * g$um(g$i$) + vmbar * g$vm(g$i$) + d$6bar * vm * g$qt
     *an(g$i$)
99958   continue
        cinhom = d$0 * d$6 / bm
C       determine spatial differences
C       py = (pf - pb) / deltay
        d$0bar = (1.0d0 / deltay)
        do 99957 g$i$ = 1, g$p$
          g$py(g$i$) = d$0bar * g$pf(g$i$) + (-d$0bar * g$pb(g$i$))
99957   continue
        py = (pf - (pb)) / deltay
C       uy = (uf - ub) / deltay
        d$0bar = (1.0d0 / deltay)
        do 99956 g$i$ = 1, g$p$
          g$uy(g$i$) = d$0bar * g$uf(g$i$) + (-d$0bar * g$ub(g$i$))
99956   continue
        uy = (uf - (ub)) / deltay
C       vy = (vf - vb) / deltay
        d$0bar = (1.0d0 / deltay)
        do 99955 g$i$ = 1, g$p$
          g$vy(g$i$) = d$0bar * g$vf(g$i$) + (-d$0bar * g$vb(g$i$))
99955   continue
        vy = (vf - (vb)) / deltay
        if (m .ne. mc) then
          goto 50
        endif
        call g$var$252(g$p$, 1, mc - 2, dum1, g$dum1(1), g$pmax$, ubb, g
     *$ubb(1), g$pmax$, vbb, g$vbb(1), g$pmax$, dum2, g$dum2(1), g$pmax$
     *, pbb, g$pbb(1), g$pmax$)
        deltay = 2. * dy
C       py = (pbb - 4. * pb + 3. * pf) / deltay
        d$3bar = (1.0d0 / deltay)
        do 99954 g$i$ = 1, g$p$
          g$py(g$i$) = d$3bar * g$pbb(g$i$) + (-d$3bar * (4.) * g$pb(g$i
     *$)) + (d$3bar * (3.) * g$pf(g$i$))
99954   continue
        py = (pbb - (4. * (pb)) + (3. * (pf))) / deltay
C       uy = (ubb - 4. * ub + 3. * uf) / deltay
        d$3bar = (1.0d0 / deltay)
        do 99953 g$i$ = 1, g$p$
          g$uy(g$i$) = d$3bar * g$ubb(g$i$) + (-d$3bar * (4.) * g$ub(g$i
     *$)) + (d$3bar * (3.) * g$uf(g$i$))
99953   continue
        uy = (ubb - (4. * (ub)) + (3. * (uf))) / deltay
C       vy = (vbb - 4. * vb + 3. * vf) / deltay
        d$3bar = (1.0d0 / deltay)
        do 99952 g$i$ = 1, g$p$
          g$vy(g$i$) = d$3bar * g$vbb(g$i$) + (-d$3bar * (4.) * g$vb(g$i
     *$)) + (d$3bar * (3.) * g$vf(g$i$))
99952   continue
        vy = (vbb - (4. * (vb)) + (3. * (vf))) / deltay
50      continue
        call g$var$252(g$p$, 2, m, dum1, g$dum1(1), g$pmax$, uf, g$uf(1)
     *, g$pmax$, vf, g$vf(1), g$pmax$, dum2, g$dum2(1), g$pmax$, pf, g$p
     *f(1), g$pmax$)
        call g$var$252(g$p$, 3, m, dum1, g$dum1(1), g$pmax$, uff, g$uff(
     *1), g$pmax$, vff, g$vff(1), g$pmax$, dum2, g$dum2(1), g$pmax$, pff
     *, g$pff(1), g$pmax$)
C       px = (-3. * pm + 4. * pf - pff) / deltax
        d$3bar = (1.0d0 / deltax)
        do 99951 g$i$ = 1, g$p$
          g$px(g$i$) = d$3bar * (-3.) * g$pm(g$i$) + (d$3bar * (4.) * g$
     *pf(g$i$)) + (-d$3bar * g$pff(g$i$))
99951   continue
        px = (-3. * (pm) + (4. * (pf)) - (pff)) / (deltax)
C       ux = (-3. * um + 4. * uf - uff) / deltax
        d$3bar = (1.0d0 / deltax)
        do 99950 g$i$ = 1, g$p$
          g$ux(g$i$) = d$3bar * (-3.) * g$um(g$i$) + (d$3bar * (4.) * g$
     *uf(g$i$)) + (-d$3bar * g$uff(g$i$))
99950   continue
        ux = (-3. * (um) + (4. * (uf)) - (uff)) / (deltax)
C       vx = (-3. * vm + 4. * vf - vff) / deltax
        d$3bar = (1.0d0 / deltax)
        do 99949 g$i$ = 1, g$p$
          g$vx(g$i$) = d$3bar * (-3.) * g$vm(g$i$) + (d$3bar * (4.) * g$
     *vf(g$i$)) + (-d$3bar * g$vff(g$i$))
99949   continue
        vx = (-3. * (vm) + (4. * (vf)) - (vff)) / (deltax)
C       evaluate eqn
C       value = cpx * px + cux * ux + cvx * vx + cpy * py + cuy * uy + cvy * vy 
C *    + cinhom
        do 99948 g$i$ = 1, g$p$
          g$value(g$i$) = px * g$cpx(g$i$) + cpx * g$px(g$i$) + ux * g$c
     *ux(g$i$) + cux * g$ux(g$i$) + vx * g$cvx(g$i$) + cvx * g$vx(g$i$) 
     *+ py * g$cpy(g$i$) + cpy * g$py(g$i$) + uy * g$cuy(g$i$) + cuy * g
     *$uy(g$i$) + vy * g$cvy(g$i$) + cvy * g$vy(g$i$) + g$cinhom(g$i$)
99948   continue
        value = cpx * px + cux * ux + cvx * vx + cpy * py + cuy * uy + c
     *vy * vy + cinhom
        eqns(mm + 3) = value
        do 99947 g$i$ = 1, g$p$
          g$eqns(g$i$, mm + 3) = g$value(g$i$)
99947   continue
        return
100     continue
C       eqns(mm + 1) = dm - dstag
        do 99946 g$i$ = 1, g$p$
          g$eqns(g$i$, mm + 1) = g$dm(g$i$)
99946   continue
        eqns(mm + 1) = dm - dstag
        eqns(mm + 2) = um
        do 99945 g$i$ = 1, g$p$
          g$eqns(g$i$, mm + 2) = g$um(g$i$)
99945   continue
        eqns(mm + 3) = vm
        do 99944 g$i$ = 1, g$p$
          g$eqns(g$i$, mm + 3) = g$vm(g$i$)
99944   continue
        return
      end
