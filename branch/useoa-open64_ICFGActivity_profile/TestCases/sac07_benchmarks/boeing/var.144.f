      subroutine g$var$144(g$p$, n, m, dm, um, vm, g$vm, ldg$vm, em, pm)
C       
C       Common block // contains active variables.
C       Variable hh in Common block // is active.
C       Variable gg in Common block // is active.
C       Variable ff in Common block // is active.
C       Variable eq in Common block // is active.
C       Variable u in Common block // is active.
C       Formal vm is active.
C       
        integer g$p$
        integer g$pmax$
        parameter (g$pmax$ = 190)
        integer g$i$
c        real g$vm(ldg$vm)
        double precision g$vm(ldg$vm)
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
        integer ldg$vm
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
        dm = u(mm + 1)
        um = u(mm + 2)
C       vm = u(mm + 3)
        do 99998 g$i$ = 1, g$p$
          g$vm(g$i$) = g$u(g$i$, mm + 3)
99998   continue
        vm = u(mm + 3)
        em = (htot - (um ** 2 + vm ** 2) / 2.) / gamma
        pm = gm1 * em * dm
        return
      end