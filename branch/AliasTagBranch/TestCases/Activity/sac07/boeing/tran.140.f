      subroutine g$tran$140(g$p$, n, m, xr, g$xr, ldg$xr, xth, g$xth, ld
     *g$xth, yr, yth, ajac)
C       
C       Common block // contains active variables.
C       Variable hh in Common block // is active.
C       Variable gg in Common block // is active.
C       Variable ff in Common block // is active.
C       Variable eq in Common block // is active.
C       Variable u in Common block // is active.
C       Formal xth is active.
C       Formal xr is active.
C       
        integer g$p$
        integer g$pmax$
        parameter (g$pmax$ = 190)
        integer g$i$
        double precision d$4bar
        double precision rbar
c        real r$6
        double precision r$6
        double precision d$5
        double precision d$2
c        real r$0
        double precision r$0
        double precision d$0bar
        double precision g$r(g$pmax$)
        double precision g$sthm(g$pmax$)
        double precision g$sb(g$pmax$)
        double precision g$sm(g$pmax$)
        double precision g$dummy(g$pmax$)
c        real g$xth(ldg$xth)
c        real g$xr(ldg$xr)
        double precision g$xth(ldg$xth)
        double precision g$xr(ldg$xr)
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
        integer ldg$xr
        integer ldg$xth
C       
        implicit double precision (a-h, o-z)
        common // u(190), eq(190), eqold(190), a(190, 95), eqplus(190), 
     *b(10), bth(10), ff(3, 6, 10), gg(3, 6, 10), hh(3, 6, 10), mc, nc, 
     *dx, dy, ddx, ddy, ddx2, ddy2, mc1, nc1, mc2, mc3, theta1, gamma, g
     *m1, xmach, ncf, mcncf, qinc, dinc, pinc, einc, ainc, htot, swall, 
     *dstag, emaxs(200), eqmaxs(200), delta, kdiag, kblk, ksize, kblk2, 
     *kblk3, itmax, sstol, alpha, nprint, emax, eqmax, kprint, dtheta, i
     *cont, xend, xincr
C       
C       subroutine determines transformation parameters xr,xth,
C       yr,yth and jacobian ajac at mesh point (n,m)
C       
C       sm=s(m)
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
        call g$subs$6(g$p$, m, dummy, g$dummy(1), g$pmax$)
        sm = dummy
        do 99994 g$i$ = 1, g$p$
          g$sm(g$i$) = g$dummy(g$i$)
99994   continue
        bm = b(m)
C       sb = sm - bm
        do 99993 g$i$ = 1, g$p$
          g$sb(g$i$) = g$sm(g$i$)
99993   continue
        sb = sm - bm
C       sthm=sth(m)
        call g$substh$6(g$p$, m, dummy, g$dummy(1), g$pmax$)
        sthm = dummy
        do 99992 g$i$ = 1, g$p$
          g$sthm(g$i$) = g$dummy(g$i$)
99992   continue
        bthm = bth(m)
        call g$rth$20(g$p$, n, m, r, g$r(1), g$pmax$, theta)
C       xr = 1. / sb
        r$0 = 1. / sb
        do 99991 g$i$ = 1, g$p$
          g$xr(g$i$) = (-r$0 / (sb)) * g$sb(g$i$)
99991   continue
        xr = r$0
C       xth = ((r - sm) * bthm - (r - bm) * sthm) / sb ** 2
        d$2 = r - bm
        d$5 = sb ** 2
        r$6 = ((r - sm) * bthm - d$2 * sthm) / d$5
        d$4bar = (1.0d0 / d$5)
        rbar = -d$4bar * (sthm)
        d$0bar = d$4bar * (bthm)
        rbar = rbar + (d$0bar)
        do 99990 g$i$ = 1, g$p$
          g$xth(g$i$) = rbar * g$r(g$i$) + (-d$0bar * g$sm(g$i$)) + (-d$
     *4bar * (d$2) * g$sthm(g$i$)) + ((-r$6 / (d$5)) * (2 * (sb)) * g$sb
     *(g$i$))
99990   continue
        xth = r$6
        yr = 0.
        yth = 1. / theta1
        ajac = theta1 * sb
        return
      end
