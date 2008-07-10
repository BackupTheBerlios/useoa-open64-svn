      subroutine g$var$252(g$p$, n, m, dm, g$dm, ldg$dm, um, g$um, ldg$u
     *m, vm, g$vm, ldg$vm, em, g$em, ldg$em, pm, g$pm, ldg$pm)
C       
C       Common block // contains active variables.
C       Variable hh in Common block // is active.
C       Variable gg in Common block // is active.
C       Variable ff in Common block // is active.
C       Variable eq in Common block // is active.
C       Variable u in Common block // is active.
C       Formal pm is active.
C       Formal em is active.
C       Formal vm is active.
C       Formal um is active.
C       Formal dm is active.
C       
        integer g$p$
        integer g$pmax$
        parameter (g$pmax$ = 190)
        integer g$i$
        double precision d$0
c        real r$2bar
c        real g$pm(ldg$pm)
c        real g$em(ldg$em)
c        real g$vm(ldg$vm)
c        real g$um(ldg$um)
c        real g$dm(ldg$dm)
        double precision r$2bar
        double precision g$pm(ldg$pm)
        double precision g$em(ldg$em)
        double precision g$vm(ldg$vm)
        double precision g$um(ldg$um)
        double precision g$dm(ldg$dm)
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
        integer ldg$dm
        integer ldg$um
        integer ldg$vm
        integer ldg$em
        integer ldg$pm
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
C       subroutine returns density, r-velocity,theta-velocity,
C       specific energy, total energy, and pressure at point(n,m)
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
        mm = (m - 1) * ncf + (n - 1) * 3
C       dm = u(mm + 1)
        do 99994 g$i$ = 1, g$p$
          g$dm(g$i$) = g$u(g$i$, mm + 1)
99994   continue
        dm = u(mm + 1)
C       um = u(mm + 2)
        do 99993 g$i$ = 1, g$p$
          g$um(g$i$) = g$u(g$i$, mm + 2)
99993   continue
        um = u(mm + 2)
C       vm = u(mm + 3)
        do 99992 g$i$ = 1, g$p$
          g$vm(g$i$) = g$u(g$i$, mm + 3)
99992   continue
        vm = u(mm + 3)
C       em = (htot - (um ** 2 + vm ** 2) / 2.) / gamma
        r$2bar = -(1.0d0 / gamma) * (1.0d0 / 2.)
        do 99991 g$i$ = 1, g$p$
          g$em(g$i$) = r$2bar * (2 * um) * g$um(g$i$) + r$2bar * (2 * vm
     *) * g$vm(g$i$)
99991   continue
        em = (htot - (um ** 2 + vm ** 2) / 2.) / gamma
C       pm = gm1 * em * dm
        d$0 = gm1 * em
        do 99990 g$i$ = 1, g$p$
          g$pm(g$i$) = dm * gm1 * g$em(g$i$) + d$0 * g$dm(g$i$)
99990   continue
        pm = d$0 * dm
        return
      end
