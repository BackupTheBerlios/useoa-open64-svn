      subroutine g$shock$5(g$p$, eqns, g$eqns, ldg$eqns, m)
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
        double precision d$13bar
        double precision d$12bar
        double precision d$10bar
        double precision d$8bar
        double precision d$14
        double precision d$12
        double precision d$10
        double precision d$9
        double precision d$5bar
        double precision d$4bar
        double precision d$6
        double precision d$5
        double precision d$4
        double precision vmbar
        double precision umbar
        double precision d$3bar
        double precision d$2bar
        double precision d$3
        double precision d$2
        double precision d$1bar
        double precision d$1
        double precision d$0bar
        double precision smbar
        double precision d$0
        double precision g$vy(g$pmax$)
        double precision g$uy(g$pmax$)
        double precision g$py(g$pmax$)
        double precision g$pf(g$pmax$)
        double precision g$vf(g$pmax$)
        double precision g$uf(g$pmax$)
        double precision g$vx(g$pmax$)
        double precision g$ux(g$pmax$)
        double precision g$px(g$pmax$)
        double precision g$pbb(g$pmax$)
        double precision g$vbb(g$pmax$)
        double precision g$ubb(g$pmax$)
        double precision g$pb(g$pmax$)
        double precision g$dum2(g$pmax$)
        double precision g$vb(g$pmax$)
        double precision g$ub(g$pmax$)
        double precision g$dum1(g$pmax$)
        double precision g$cinhom(g$pmax$)
        double precision g$cvy(g$pmax$)
        double precision g$cuy(g$pmax$)
        double precision g$cpy(g$pmax$)
        double precision g$cvx(g$pmax$)
        double precision g$cux(g$pmax$)
        double precision g$cpx(g$pmax$)
        double precision g$alam3(g$pmax$)
        double precision g$bb(g$pmax$)
        double precision g$aa(g$pmax$)
        double precision g$xth(g$pmax$)
        double precision g$xr(g$pmax$)
        double precision g$sig(g$pmax$)
        double precision g$z(g$pmax$)
        double precision g$z2(g$pmax$)
        double precision g$qtanga(g$pmax$)
        double precision g$qtangb(g$pmax$)
        double precision g$value(g$pmax$)
        double precision g$capqa(g$pmax$)
        double precision g$pm(g$pmax$)
        double precision g$em(g$pmax$)
        double precision g$vm(g$pmax$)
        double precision g$um(g$pmax$)
        double precision g$dm(g$pmax$)
        double precision g$capqb(g$pmax$)
        double precision g$ds(g$pmax$)
        double precision g$ds2(g$pmax$)
        double precision g$sths(g$pmax$)
        double precision g$sthm(g$pmax$)
        double precision g$sm2(g$pmax$)
        double precision g$sm(g$pmax$)
        double precision g$dummy(g$pmax$)
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
C       subroutine evaluates r-h eqns and compatibility
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
        mm = (m - 1) * ncf + (nc - 1) * 3
C       sm=s(m)
        call g$subs$6(g$p$, m, dummy, g$dummy(1), g$pmax$)
        sm = dummy
        do 99955 g$i$ = 1, g$p$
          g$sm(g$i$) = g$dummy(g$i$)
99955   continue
C       sm2 = sm * sm
        do 99954 g$i$ = 1, g$p$
          g$sm2(g$i$) = (sm + sm) * g$sm(g$i$)
99954   continue
        sm2 = sm * sm
C       sthm=sth(m)
        call g$substh$6(g$p$, m, dummy, g$dummy(1), g$pmax$)
        sthm = dummy
        do 99953 g$i$ = 1, g$p$
          g$sthm(g$i$) = g$dummy(g$i$)
99953   continue
C       sths = sthm / sm
        d$0 = sthm / sm
        do 99952 g$i$ = 1, g$p$
          g$sths(g$i$) = (1.0d0 / sm) * g$sthm(g$i$) + ((-d$0 / (sm)) * 
     *g$sm(g$i$))
99952   continue
        sths = d$0
C       ds2 = 1. + sths ** 2
        do 99951 g$i$ = 1, g$p$
          g$ds2(g$i$) = (2 * sths) * g$sths(g$i$)
99951   continue
        ds2 = 1. + sths ** 2
C       ds = dsqrt(ds2)
        d$0 = dsqrt(ds2)
        do 99950 g$i$ = 1, g$p$
          g$ds(g$i$) = 1.0d0 / (2 * d$0) * g$ds2(g$i$)
99950   continue
        ds = d$0
        call g$rth$16(g$p$, nc, m, r, theta)
        sinth = sin(theta)
        costh = cos(theta)
C       capqb = -qinc * (costh + sths * sinth) / ds
        d$3 = -qinc * (costh + sths * sinth) / (ds)
        do 99949 g$i$ = 1, g$p$
          g$capqb(g$i$) = ((1.0d0 / ds) * (-qinc) * (sinth)) * g$sths(g$
     *i$) + ((-d$3 / (ds)) * g$ds(g$i$))
99949   continue
        capqb = d$3
        call g$var$252(g$p$, nc, m, dm, g$dm(1), g$pmax$, um, g$um(1), g
     *$pmax$, vm, g$vm(1), g$pmax$, em, g$em(1), g$pmax$, pm, g$pm(1), g
     *$pmax$)
C       capqa = (um - sths * vm) / ds
        d$2 = (um - sths * vm) / ds
        d$1bar = (1.0d0 / ds)
        do 99948 g$i$ = 1, g$p$
          g$capqa(g$i$) = d$1bar * g$um(g$i$) + (-d$1bar * (vm) * g$sths
     *(g$i$)) + (-d$1bar * (sths) * g$vm(g$i$)) + ((-d$2 / (ds)) * g$ds(
     *g$i$))
99948   continue
        capqa = d$2
C       r-h mass
C       value = dm * capqa - dinc * capqb
        do 99947 g$i$ = 1, g$p$
          g$value(g$i$) = capqa * g$dm(g$i$) + dm * g$capqa(g$i$) + (-di
     *nc * g$capqb(g$i$))
99947   continue
        value = dm * (capqa) - (dinc * (capqb))
        eqns(mm + 1) = value
        do 99946 g$i$ = 1, g$p$
          g$eqns(g$i$, mm + 1) = g$value(g$i$)
99946   continue
C       r-h momentum
C       value = pm + dm * capqa ** 2 - pinc - dinc * capqb ** 2
        d$0 = capqa ** 2
        do 99945 g$i$ = 1, g$p$
          g$value(g$i$) = g$pm(g$i$) + d$0 * g$dm(g$i$) + dm * (2 * capq
     *a) * g$capqa(g$i$) + (-dinc * (2 * capqb) * g$capqb(g$i$))
99945   continue
        value = pm + dm * d$0 - pinc - dinc * capqb ** 2
        eqns(mm + 2) = value
        do 99944 g$i$ = 1, g$p$
          g$eqns(g$i$, mm + 2) = g$value(g$i$)
99944   continue
C       tangential velocity
C       qtangb = qinc * (-sths * costh + sinth) / ds
        d$4 = qinc * (-sths * (costh) + (sinth)) / ds
        do 99943 g$i$ = 1, g$p$
          g$qtangb(g$i$) = -((1.0d0 / ds) * qinc * costh) * g$sths(g$i$)
     * + ((-d$4 / (ds)) * g$ds(g$i$))
99943   continue
        qtangb = d$4
C       qtanga = (um * sths + vm) / ds
        d$2 = (um * sths + vm) / ds
        d$1bar = (1.0d0 / ds)
        do 99942 g$i$ = 1, g$p$
          g$qtanga(g$i$) = d$1bar * sths * g$um(g$i$) + d$1bar * um * g$
     *sths(g$i$) + d$1bar * g$vm(g$i$) + ((-d$2 / (ds)) * g$ds(g$i$))
99942   continue
        qtanga = d$2
C       value = qtanga - qtangb
        do 99941 g$i$ = 1, g$p$
          g$value(g$i$) = g$qtanga(g$i$) + (-g$qtangb(g$i$))
99941   continue
        value = qtanga - (qtangb)
        eqns(mm + 3) = value
        do 99940 g$i$ = 1, g$p$
          g$eqns(g$i$, mm + 3) = g$value(g$i$)
99940   continue
C       compatibility
C       z2 = gamma * gm1 * em
        do 99939 g$i$ = 1, g$p$
          g$z2(g$i$) = (gamma * gm1) * g$em(g$i$)
99939   continue
        z2 = gamma * gm1 * em
C       z = dsqrt(z2)
        d$0 = dsqrt(z2)
        do 99938 g$i$ = 1, g$p$
          g$z(g$i$) = 1.0d0 / (2 * d$0) * g$z2(g$i$)
99938   continue
        z = d$0
C       sig = -ds * z
        do 99937 g$i$ = 1, g$p$
          g$sig(g$i$) = -z * g$ds(g$i$) + (-ds * g$z(g$i$))
99937   continue
        sig = -ds * (z)
        call g$tran$140(g$p$, nc, m, xr, g$xr(1), g$pmax$, xth, g$xth(1)
     *, g$pmax$, yr, yth, ajac)
C       aa = um * xr + vm * xth / sm
        d$2 = vm * xth / sm
        d$1bar = (1.0d0 / sm)
        do 99936 g$i$ = 1, g$p$
          g$aa(g$i$) = xr * g$um(g$i$) + um * g$xr(g$i$) + d$1bar * xth 
     ** g$vm(g$i$) + d$1bar * vm * g$xth(g$i$) + ((-d$2 / (sm)) * g$sm(g
     *$i$))
99936   continue
        aa = um * (xr) + (d$2)
C       bb = um * yr + vm * yth / sm
        d$2 = vm * yth / sm
        do 99935 g$i$ = 1, g$p$
          g$bb(g$i$) = yr * g$um(g$i$) + ((1.0d0 / sm) * yth * g$vm(g$i$
     *)) + ((-d$2 / (sm)) * g$sm(g$i$))
99935   continue
        bb = um * (yr) + (d$2)
        alam2 = 1.
C       alam3 = -sthm
        do 99934 g$i$ = 1, g$p$
          g$alam3(g$i$) = -g$sthm(g$i$)
99934   continue
        alam3 = -sthm
C       determine coeffs of various terms
C       cpx = -sig * aa + z2 * (alam2 * xr + alam3 * xth / sm2)
        d$4 = alam3 * xth / sm2
        d$5 = alam2 * xr + d$4
        d$3bar = z2 * (1.0d0 / sm2)
        do 99933 g$i$ = 1, g$p$
          g$cpx(g$i$) = -aa * g$sig(g$i$) + (-sig * g$aa(g$i$)) + (d$5 *
     * g$z2(g$i$)) + (z2 * (alam2) * g$xr(g$i$)) + (d$3bar * (xth) * g$a
     *lam3(g$i$)) + (d$3bar * (alam3) * g$xth(g$i$)) + (z2 * (-d$4 / (sm
     *2)) * g$sm2(g$i$))
99933   continue
        cpx = -sig * (aa) + (z2 * (d$5))
C       cux = dm * z2 * (-sig * xr + alam2 * aa)
        d$0 = dm * z2
        d$4 = -sig * (xr) + (alam2 * (aa))
        do 99932 g$i$ = 1, g$p$
          g$cux(g$i$) = d$4 * z2 * g$dm(g$i$) + d$4 * dm * g$z2(g$i$) + 
     *(-(d$0 * xr) * g$sig(g$i$)) + (d$0 * (-sig) * g$xr(g$i$)) + (d$0 *
     * (alam2) * g$aa(g$i$))
99932   continue
        cux = d$0 * (d$4)
C       cvx = dm * z2 * (-sig * xth + alam3 * aa) / sm
        d$0 = dm * z2
        d$4 = -sig * (xth) + (alam3 * (aa))
        d$6 = d$0 * (d$4) / (sm)
        d$5bar = (1.0d0 / (sm))
        d$0bar = d$5bar * (d$4)
        d$4bar = d$5bar * (d$0)
        do 99931 g$i$ = 1, g$p$
          g$cvx(g$i$) = d$0bar * z2 * g$dm(g$i$) + d$0bar * dm * g$z2(g$
     *i$) + (-(d$4bar * xth) * g$sig(g$i$)) + (d$4bar * (-sig) * g$xth(g
     *$i$)) + (d$4bar * (aa) * g$alam3(g$i$)) + (d$4bar * (alam3) * g$aa
     *(g$i$)) + ((-d$6 / (sm)) * g$sm(g$i$))
99931   continue
        cvx = d$6
        if (m .eq. 1) then
          goto 20
        endif
C       cpy = -sig * bb + z2 * (alam2 * yr + alam3 * yth / sm2)
        d$3 = alam3 * yth / sm2
        d$4 = alam2 * yr + d$3
        do 99930 g$i$ = 1, g$p$
          g$cpy(g$i$) = -bb * g$sig(g$i$) + (-sig * g$bb(g$i$)) + (d$4 *
     * g$z2(g$i$)) + (z2 * (1.0d0 / sm2) * (yth) * g$alam3(g$i$)) + (z2 
     ** (-d$3 / (sm2)) * g$sm2(g$i$))
99930   continue
        cpy = -sig * (bb) + (z2 * (d$4))
C       cuy = dm * z2 * (-sig * yr + alam2 * bb)
        d$0 = dm * z2
        d$4 = -sig * (yr) + (alam2 * (bb))
        do 99929 g$i$ = 1, g$p$
          g$cuy(g$i$) = d$4 * z2 * g$dm(g$i$) + d$4 * dm * g$z2(g$i$) + 
     *(-(d$0 * yr) * g$sig(g$i$)) + (d$0 * (alam2) * g$bb(g$i$))
99929   continue
        cuy = d$0 * (d$4)
C       cvy = dm * z2 * (-sig * yth + alam3 * bb) / sm
        d$0 = dm * z2
        d$4 = -sig * (yth) + (alam3 * (bb))
        d$6 = d$0 * (d$4) / (sm)
        d$5bar = (1.0d0 / (sm))
        d$0bar = d$5bar * (d$4)
        d$4bar = d$5bar * (d$0)
        do 99928 g$i$ = 1, g$p$
          g$cvy(g$i$) = d$0bar * z2 * g$dm(g$i$) + d$0bar * dm * g$z2(g$
     *i$) + (-(d$4bar * yth) * g$sig(g$i$)) + (d$4bar * (bb) * g$alam3(g
     *$i$)) + (d$4bar * (alam3) * g$bb(g$i$)) + ((-d$6 / (sm)) * g$sm(g$
     *i$))
99928   continue
        cvy = d$6
C       cinhom = -dm * z2 * (sig * (2. * um + vm * costh / sinth) + vm * (alam2 
C *    * vm - alam3 * um / sm)) / sm
        d$1 = -dm * (z2)
        d$5 = 2. * um + vm * costh / sinth
        d$9 = alam3 * um / sm
        d$10 = alam2 * vm - d$9
        d$12 = sig * d$5 + vm * d$10
        d$14 = d$1 * d$12 / sm
        d$13bar = (1.0d0 / sm)
        smbar = (-d$14 / (sm))
        d$1bar = d$13bar * (d$12)
        d$12bar = d$13bar * (d$1)
        vmbar = d$12bar * (d$10)
        d$10bar = d$12bar * (vm)
        d$8bar = -d$10bar * (1.0d0 / (sm))
        smbar = smbar + (-d$10bar * (-d$9 / (sm)))
        umbar = d$8bar * (alam3)
        vmbar = vmbar + d$10bar * alam2
        d$5bar = d$12bar * sig
        vmbar = vmbar + (d$5bar * (1.0d0 / (sinth)) * (costh))
        umbar = umbar + (d$5bar * (2.))
        do 99927 g$i$ = 1, g$p$
          g$cinhom(g$i$) = -(d$1bar * z2) * g$dm(g$i$) + d$1bar * (-dm) 
     ** g$z2(g$i$) + d$12bar * d$5 * g$sig(g$i$) + umbar * g$um(g$i$) + 
     *vmbar * g$vm(g$i$) + d$8bar * um * g$alam3(g$i$) + smbar * g$sm(g$
     *i$)
99927   continue
        cinhom = d$14
        goto 25
20      continue
C       special for shock pt on sym line
        cpy = 0.
        do 99926 g$i$ = 1, g$p$
          g$cpy(g$i$) = 0.0d0
99926   continue
        cuy = 0.
        do 99925 g$i$ = 1, g$p$
          g$cuy(g$i$) = 0.0d0
99925   continue
C       cvy = -2. * dm * z2 * sig * yth / sm
        d$0 = -2. * (dm)
        d$1 = d$0 * (z2)
        d$4 = d$1 * (sig) * (yth) / (sm)
        d$2bar = (1.0d0 / sm) * yth
        d$1bar = d$2bar * sig
        do 99924 g$i$ = 1, g$p$
          g$cvy(g$i$) = d$1bar * z2 * (-2.) * g$dm(g$i$) + (d$1bar * (d$
     *0) * g$z2(g$i$)) + (d$2bar * (d$1) * g$sig(g$i$)) + ((-d$4 / (sm))
     * * g$sm(g$i$))
99924   continue
        cvy = d$4
C       cinhom = -2. * dm * z2 * sig * um / sm
        d$0 = -2. * (dm)
        d$1 = d$0 * (z2)
        d$2 = d$1 * (sig)
        d$4 = d$2 * (um) / (sm)
        d$3bar = (1.0d0 / sm)
        d$2bar = d$3bar * um
        d$1bar = d$2bar * sig
        do 99923 g$i$ = 1, g$p$
          g$cinhom(g$i$) = d$1bar * z2 * (-2.) * g$dm(g$i$) + (d$1bar * 
     *(d$0) * g$z2(g$i$)) + (d$2bar * (d$1) * g$sig(g$i$)) + (d$3bar * (
     *d$2) * g$um(g$i$)) + ((-d$4 / (sm)) * g$sm(g$i$))
99923   continue
        cinhom = d$4
25      continue
C       determine spatial differences
        call g$var$252(g$p$, nc - 1, m, dum1, g$dum1(1), g$pmax$, ub, g$
     *ub(1), g$pmax$, vb, g$vb(1), g$pmax$, dum2, g$dum2(1), g$pmax$, pb
     *, g$pb(1), g$pmax$)
        call g$var$252(g$p$, nc - 2, m, dum1, g$dum1(1), g$pmax$, ubb, g
     *$ubb(1), g$pmax$, vbb, g$vbb(1), g$pmax$, dum2, g$dum2(1), g$pmax$
     *, pbb, g$pbb(1), g$pmax$)
        deltax = 2. * dx
C       px = (pbb - 4. * pb + 3. * pm) / deltax
        d$3bar = (1.0d0 / deltax)
        do 99922 g$i$ = 1, g$p$
          g$px(g$i$) = d$3bar * g$pbb(g$i$) + (-d$3bar * (4.) * g$pb(g$i
     *$)) + (d$3bar * (3.) * g$pm(g$i$))
99922   continue
        px = (pbb - (4. * (pb)) + (3. * (pm))) / deltax
C       ux = (ubb - 4. * ub + 3. * um) / deltax
        d$3bar = (1.0d0 / deltax)
        do 99921 g$i$ = 1, g$p$
          g$ux(g$i$) = d$3bar * g$ubb(g$i$) + (-d$3bar * (4.) * g$ub(g$i
     *$)) + (d$3bar * (3.) * g$um(g$i$))
99921   continue
        ux = (ubb - (4. * (ub)) + (3. * (um))) / deltax
C       vx = (vbb - 4. * vb + 3. * vm) / deltax
        d$3bar = (1.0d0 / deltax)
        do 99920 g$i$ = 1, g$p$
          g$vx(g$i$) = d$3bar * g$vbb(g$i$) + (-d$3bar * (4.) * g$vb(g$i
     *$)) + (d$3bar * (3.) * g$vm(g$i$))
99920   continue
        vx = (vbb - (4. * (vb)) + (3. * (vm))) / deltax
        call g$mwall$8(g$p$, m, mf, mb)
        call g$var$252(g$p$, nc, mf, dum1, g$dum1(1), g$pmax$, uf, g$uf(
     *1), g$pmax$, vf, g$vf(1), g$pmax$, dum2, g$dum2(1), g$pmax$, pf, g
     *$pf(1), g$pmax$)
        call g$var$252(g$p$, nc, mb, dum1, g$dum1(1), g$pmax$, ub, g$ub(
     *1), g$pmax$, vb, g$vb(1), g$pmax$, dum2, g$dum2(1), g$pmax$, pb, g
     *$pb(1), g$pmax$)
        deltay = (mf - mb) * dy
C       py = (pf - pb) / deltay
        d$0bar = (1.0d0 / deltay)
        do 99919 g$i$ = 1, g$p$
          g$py(g$i$) = d$0bar * g$pf(g$i$) + (-d$0bar * g$pb(g$i$))
99919   continue
        py = (pf - (pb)) / deltay
C       uy = (uf - ub) / deltay
        d$0bar = (1.0d0 / deltay)
        do 99918 g$i$ = 1, g$p$
          g$uy(g$i$) = d$0bar * g$uf(g$i$) + (-d$0bar * g$ub(g$i$))
99918   continue
        uy = (uf - (ub)) / deltay
C       vy = (vf - vb) / deltay
        d$0bar = (1.0d0 / deltay)
        do 99917 g$i$ = 1, g$p$
          g$vy(g$i$) = d$0bar * g$vf(g$i$) + (-d$0bar * g$vb(g$i$))
99917   continue
        vy = (vf - (vb)) / deltay
        if (m .ne. mc) then
          goto 50
        endif
        deltay = 2. * dy
        call g$var$252(g$p$, nc, mc - 2, dum1, g$dum1(1), g$pmax$, ubb, 
     *g$ubb(1), g$pmax$, vbb, g$vbb(1), g$pmax$, dum2, g$dum2(1), g$pmax
     *$, pbb, g$pbb(1), g$pmax$)
C       py = (pbb - 4. * pb + 3. * pf) / deltay
        d$3bar = (1.0d0 / deltay)
        do 99916 g$i$ = 1, g$p$
          g$py(g$i$) = d$3bar * g$pbb(g$i$) + (-d$3bar * (4.) * g$pb(g$i
     *$)) + (d$3bar * (3.) * g$pf(g$i$))
99916   continue
        py = (pbb - (4. * (pb)) + (3. * (pf))) / deltay
C       uy = (ubb - 4. * ub + 3. * uf) / deltay
        d$3bar = (1.0d0 / deltay)
        do 99915 g$i$ = 1, g$p$
          g$uy(g$i$) = d$3bar * g$ubb(g$i$) + (-d$3bar * (4.) * g$ub(g$i
     *$)) + (d$3bar * (3.) * g$uf(g$i$))
99915   continue
        uy = (ubb - (4. * (ub)) + (3. * (uf))) / deltay
C       vy = (vbb - 4. * vb + 3. * vf) / deltay
        d$3bar = (1.0d0 / deltay)
        do 99914 g$i$ = 1, g$p$
          g$vy(g$i$) = d$3bar * g$vbb(g$i$) + (-d$3bar * (4.) * g$vb(g$i
     *$)) + (d$3bar * (3.) * g$vf(g$i$))
99914   continue
        vy = (vbb - (4. * (vb)) + (3. * (vf))) / deltay
50      continue
C       evaluate eqn
C       value = cpx * px + cux * ux + cvx * vx + cpy * py + cuy * uy + cvy * vy 
C *    + cinhom
        do 99913 g$i$ = 1, g$p$
          g$value(g$i$) = px * g$cpx(g$i$) + cpx * g$px(g$i$) + ux * g$c
     *ux(g$i$) + cux * g$ux(g$i$) + vx * g$cvx(g$i$) + cvx * g$vx(g$i$) 
     *+ py * g$cpy(g$i$) + cpy * g$py(g$i$) + uy * g$cuy(g$i$) + cuy * g
     *$uy(g$i$) + vy * g$cvy(g$i$) + cvy * g$vy(g$i$) + g$cinhom(g$i$)
99913   continue
        value = cpx * px + cux * ux + cvx * vx + cpy * py + cuy * uy + c
     *vy * vy + cinhom
        eqns(mm + 4) = value
        do 99912 g$i$ = 1, g$p$
          g$eqns(g$i$, mm + 4) = g$value(g$i$)
99912   continue
100     continue
        return
      end
