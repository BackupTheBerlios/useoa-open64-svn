      subroutine g$eval$240(g$p$, n, m, k, iflag, f, g$f, ldg$f, g, g$g,
     * ldg$g, h, g$h, ldg$h)
C       
C       Common block // contains active variables.
C       Variable hh in Common block // is active.
C       Variable gg in Common block // is active.
C       Variable ff in Common block // is active.
C       Variable eq in Common block // is active.
C       Variable u in Common block // is active.
C       Formal h is active.
C       Formal g is active.
C       Formal f is active.
C       
        integer g$p$
        integer g$pmax$
        parameter (g$pmax$ = 190)
        integer g$i$
        double precision d$5
        double precision d$3bar
        double precision d$2bar
        double precision vmbar
        double precision d$3
        double precision d$2
        double precision d$1bar
        double precision d$1
        double precision d$0
        double precision g$vth(g$pmax$)
        double precision g$vy(g$pmax$)
        double precision g$vf(g$pmax$)
        double precision g$h1(g$pmax$)
        double precision g$cg1(g$pmax$)
        double precision g$bb(g$pmax$)
        double precision g$cf1(g$pmax$)
        double precision g$aa(g$pmax$)
        double precision g$xir(g$pmax$)
        double precision g$pm(g$pmax$)
        double precision g$em(g$pmax$)
        double precision g$vm(g$pmax$)
        double precision g$um(g$pmax$)
        double precision g$dm(g$pmax$)
        double precision g$r(g$pmax$)
        double precision g$ajac(g$pmax$)
        double precision g$xth(g$pmax$)
        double precision g$xr(g$pmax$)
        double precision g$h(ldg$h, 3)
        double precision g$g(ldg$g, 3)
        double precision g$f(ldg$f, 3)
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
        integer ldg$f
        integer ldg$g
        integer ldg$h
C       
        implicit double precision (a-h, o-z)
        common // u(190), eq(190), eqold(190), a(190, 95), eqplus(190), 
     *b(10), bth(10), ff(3, 6, 10), gg(3, 6, 10), hh(3, 6, 10), mc, nc, 
     *dx, dy, ddx, ddy, ddx2, ddy2, mc1, nc1, mc2, mc3, theta1, gamma, g
     *m1, xmach, ncf, mcncf, qinc, dinc, pinc, einc, ainc, htot, swall, 
     *dstag, emaxs(200), eqmaxs(200), delta, kdiag, kblk, ksize, kblk2, 
     *kblk3, itmax, sstol, alpha, nprint, emax, eqmax, kprint, dtheta, i
     *cont, xend, xincr
        dimension f(3), g(3), h(3)
C       
C       subroutine returns flux vectors f,g and inhom. term h at (n,m)
C       special case iflag=1 for symmetry line m=1
C       k=1 f only, k=2 g only, k=3 h only, k=4 all
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
        call g$tran$204(g$p$, n, m, xr, g$xr(1), g$pmax$, xth, g$xth(1),
     * g$pmax$, yr, yth, ajac, g$ajac(1), g$pmax$)
        call g$rth$20(g$p$, n, m, r, g$r(1), g$pmax$, theta)
        call g$var$252(g$p$, n, m, dm, g$dm(1), g$pmax$, um, g$um(1), g$
     *pmax$, vm, g$vm(1), g$pmax$, em, g$em(1), g$pmax$, pm, g$pm(1), g$
     *pmax$)
C       xir = ajac * r * r
        d$0 = ajac * r
        do 99979 g$i$ = 1, g$p$
          g$xir(g$i$) = r * r * g$ajac(g$i$) + (d$0 + r * ajac) * g$r(g$
     *i$)
99979   continue
        xir = d$0 * r
        if (k .eq. 2) then
          goto 20
        endif
        if (k .eq. 3) then
          goto 30
        endif
10      continue
C       aa = um * xr + vm * xth / r
        d$2 = vm * xth / r
        d$1bar = (1.0d0 / r)
        do 99978 g$i$ = 1, g$p$
          g$aa(g$i$) = xr * g$um(g$i$) + um * g$xr(g$i$) + d$1bar * xth 
     ** g$vm(g$i$) + d$1bar * vm * g$xth(g$i$) + ((-d$2 / (r)) * g$r(g$i
     *$))
99978   continue
        aa = um * (xr) + (d$2)
C       flux vector f
C       cf1 = dm * aa * xir
        d$0 = dm * aa
        do 99977 g$i$ = 1, g$p$
          g$cf1(g$i$) = xir * aa * g$dm(g$i$) + xir * dm * g$aa(g$i$) + 
     *d$0 * g$xir(g$i$)
99977   continue
        cf1 = d$0 * xir
        f(1) = cf1
        do 99976 g$i$ = 1, g$p$
          g$f(g$i$, 1) = g$cf1(g$i$)
99976   continue
C       f(2) = um * cf1 + xir * xr * pm
        d$1 = xir * xr
        do 99975 g$i$ = 1, g$p$
          g$f(g$i$, 2) = cf1 * g$um(g$i$) + um * g$cf1(g$i$) + pm * xr *
     * g$xir(g$i$) + pm * xir * g$xr(g$i$) + d$1 * g$pm(g$i$)
99975   continue
        f(2) = um * cf1 + d$1 * pm
C       f(3) = vm * cf1 + xir * xth * pm / r
        d$1 = xir * xth
        d$3 = d$1 * pm / r
        d$2bar = (1.0d0 / r)
        d$1bar = d$2bar * pm
        do 99974 g$i$ = 1, g$p$
          g$f(g$i$, 3) = cf1 * g$vm(g$i$) + vm * g$cf1(g$i$) + d$1bar * 
     *xth * g$xir(g$i$) + d$1bar * xir * g$xth(g$i$) + d$2bar * d$1 * g$
     *pm(g$i$) + ((-d$3 / (r)) * g$r(g$i$))
99974   continue
        f(3) = vm * (cf1) + (d$3)
        if (k .ne. 4) then
          return
        endif
20      continue
C       flux vector g
C       bb = um * yr + vm * yth / r
        d$2 = vm * yth / r
        do 99973 g$i$ = 1, g$p$
          g$bb(g$i$) = yr * g$um(g$i$) + ((1.0d0 / r) * yth * g$vm(g$i$)
     *) + ((-d$2 / (r)) * g$r(g$i$))
99973   continue
        bb = um * (yr) + (d$2)
C       cg1 = dm * bb * xir
        d$0 = dm * bb
        do 99972 g$i$ = 1, g$p$
          g$cg1(g$i$) = xir * bb * g$dm(g$i$) + xir * dm * g$bb(g$i$) + 
     *d$0 * g$xir(g$i$)
99972   continue
        cg1 = d$0 * xir
        g(1) = cg1
        do 99971 g$i$ = 1, g$p$
          g$g(g$i$, 1) = g$cg1(g$i$)
99971   continue
C       g(2) = um * cg1 + xir * yr * pm
        d$1 = xir * yr
        do 99970 g$i$ = 1, g$p$
          g$g(g$i$, 2) = cg1 * g$um(g$i$) + um * g$cg1(g$i$) + pm * yr *
     * g$xir(g$i$) + d$1 * g$pm(g$i$)
99970   continue
        g(2) = um * cg1 + d$1 * pm
C       g(3) = vm * cg1 + xir * yth * pm / r
        d$1 = xir * yth
        d$3 = d$1 * pm / r
        d$2bar = (1.0d0 / r)
        do 99969 g$i$ = 1, g$p$
          g$g(g$i$, 3) = cg1 * g$vm(g$i$) + vm * g$cg1(g$i$) + d$2bar * 
     *pm * yth * g$xir(g$i$) + d$2bar * d$1 * g$pm(g$i$) + ((-d$3 / (r))
     * * g$r(g$i$))
99969   continue
        g(3) = vm * (cg1) + (d$3)
        if (k .ne. 4) then
          return
        endif
30      continue
C       inhomogeneous term h
        if (iflag .eq. 1) then
          goto 40
        endif
        costh = cos(theta)
        sinth = sin(theta)
C       h1 = ajac * costh * r * dm * vm / sinth
        d$0 = ajac * costh
        d$1 = d$0 * r
        d$2 = d$1 * dm
        d$3bar = (1.0d0 / sinth)
        d$2bar = d$3bar * vm
        d$1bar = d$2bar * dm
        do 99968 g$i$ = 1, g$p$
          g$h1(g$i$) = d$1bar * r * costh * g$ajac(g$i$) + d$1bar * d$0 
     ** g$r(g$i$) + d$2bar * d$1 * g$dm(g$i$) + d$3bar * d$2 * g$vm(g$i$
     *)
99968   continue
        h1 = d$2 * vm / sinth
        h(1) = h1
        do 99967 g$i$ = 1, g$p$
          g$h(g$i$, 1) = g$h1(g$i$)
99967   continue
C       h(2) = h1 * um - ajac * r * (2. * pm + dm * vm * vm)
        d$1 = ajac * r
        d$3 = dm * vm
        d$5 = 2. * pm + d$3 * vm
        d$3bar = -d$1 * (vm)
        vmbar = -d$1 * (d$3)
        vmbar = vmbar + d$3bar * dm
        do 99966 g$i$ = 1, g$p$
          g$h(g$i$, 2) = um * g$h1(g$i$) + h1 * g$um(g$i$) + (-d$5 * (r)
     * * g$ajac(g$i$)) + (-d$5 * (ajac) * g$r(g$i$)) + (-d$1 * (2.) * g$
     *pm(g$i$)) + (d$3bar * (vm) * g$dm(g$i$)) + (vmbar * g$vm(g$i$))
99966   continue
        h(2) = h1 * (um) - (d$1 * (d$5))
C       h(3) = h1 * vm + ajac * r * dm * um * vm
        d$1 = ajac * r
        d$2 = d$1 * dm
        d$3 = d$2 * um
        d$2bar = vm * um
        d$1bar = d$2bar * dm
        do 99965 g$i$ = 1, g$p$
          g$h(g$i$, 3) = vm * g$h1(g$i$) + (d$3 + h1) * g$vm(g$i$) + d$1
     *bar * r * g$ajac(g$i$) + d$1bar * ajac * g$r(g$i$) + d$2bar * d$1 
     ** g$dm(g$i$) + vm * d$2 * g$um(g$i$)
99965   continue
        h(3) = h1 * vm + d$3 * vm
        return
40      continue
        call g$var$144(g$p$, n, 2, dum1, dum2, vf, g$vf(1), g$pmax$, dum
     *3, dum4)
C       vy = vf / dy
        do 99964 g$i$ = 1, g$p$
          g$vy(g$i$) = (1.0d0 / dy) * g$vf(g$i$)
99964   continue
        vy = vf / dy
C       vth = vy / theta1
        do 99963 g$i$ = 1, g$p$
          g$vth(g$i$) = (1.0d0 / theta1) * g$vy(g$i$)
99963   continue
        vth = vy / theta1
C       h(1) = 2. * r * vth * dm * ajac
        d$0 = 2. * r
        d$1 = d$0 * vth
        d$2 = d$1 * dm
        d$1bar = ajac * dm
        do 99962 g$i$ = 1, g$p$
          g$h(g$i$, 1) = d$1bar * vth * 2. * g$r(g$i$) + d$1bar * d$0 * 
     *g$vth(g$i$) + ajac * d$1 * g$dm(g$i$) + d$2 * g$ajac(g$i$)
99962   continue
        h(1) = d$2 * ajac
C       h(2) = h(1) * um - ajac * 2. * r * pm
        d$0 = h(1)
        d$2 = ajac * 2.
        d$3 = d$2 * r
        do 99961 g$i$ = 1, g$p$
          g$h(g$i$, 2) = d$0 * g$um(g$i$) + (-pm * (r) * (2.) * g$ajac(g
     *$i$)) + (-pm * (d$2) * g$r(g$i$)) + (-d$3 * g$pm(g$i$)) + (um * g$
     *h(g$i$, 1))
99961   continue
        h(2) = d$0 * (um) - (d$3 * (pm))
C       h(3) = h(1) * vm
        d$0 = h(1)
        do 99960 g$i$ = 1, g$p$
          g$h(g$i$, 3) = d$0 * g$vm(g$i$) + vm * g$h(g$i$, 1)
99960   continue
        h(3) = d$0 * vm
        return
      end
