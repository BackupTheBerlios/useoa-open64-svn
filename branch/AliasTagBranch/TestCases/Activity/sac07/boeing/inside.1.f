      subroutine g$inside$1(g$p$)
C       
C       Common block // contains active variables.
C       Variable hh in Common block // is active.
C       Variable gg in Common block // is active.
C       Variable ff in Common block // is active.
C       Variable eq in Common block // is active.
C       Variable u in Common block // is active.
C       
        integer g$p$
        integer g$pmax$
        parameter (g$pmax$ = 190)
        integer g$i$
        double precision d$3bar
        double precision d$2bar
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
C       
C       evaluates interior equations
C       centered differences except outflow m=mc
C       and symmetry line m=1
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
        do 99998, n = 1, nc
          iflag = 0
          do 99999, m = 2, mc
            call g$eval$240(g$p$, n, m, 4, iflag, ff(1, n, m), g$ff(1, 1
     *, n, m), g$pmax$, gg(1, n, m), g$gg(1, 1, n, m), g$pmax$, hh(1, n,
     * m), g$hh(1, 1, n, m), g$pmax$)
20          continue
99999     continue
          iflag = 1
          m = 1
          call g$eval$240(g$p$, n, m, 4, iflag, ff(1, n, m), g$ff(1, 1, 
     *n, m), g$pmax$, gg(1, n, m), g$gg(1, 1, n, m), g$pmax$, hh(1, n, m
     *), g$hh(1, 1, n, m), g$pmax$)
10        continue
99998   continue
        do 99995, m = 1, mc
          call g$mmm$8(g$p$, m, mf, mb)
          deltay = (mf - mb) * dy
          do 99996, n = 2, nc1
            call g$nnn$0(g$p$, n, nf, nb)
            deltax = (nf - nb) * dx
            do 99997, i = 1, 3
              index = (m - 1) * ncf + 3 * (n - 1) + i
C             eq(index) = (ff(i, nf, m) - ff(i, nb, m)) / deltax + hh(i, n, m)
              d$2bar = (1.0d0 / deltax)
              do 99991 g$i$ = 1, g$p$
                g$eq(g$i$, index) = d$2bar * g$ff(g$i$, i, nf, m) + (-d$
     *2bar * g$ff(g$i$, i, nb, m)) + g$hh(g$i$, i, n, m)
99991         continue
              eq(index) = (ff(i, nf, m) - ff(i, nb, m)) / deltax + hh(i,
     * n, m)
              if (m .eq. mc) then
                goto 45
              endif
              if (m .ne. 1) then
C               eq(index) = eq(index) + (gg(i, n, mf) - gg(i, n, mb)) / deltay
                d$3bar = (1.0d0 / deltay)
                do 99990 g$i$ = 1, g$p$
                  g$eq(g$i$, index) = g$eq(g$i$, index) + d$3bar * g$gg(
     *g$i$, i, n, mf) + (-d$3bar * g$gg(g$i$, i, n, mb))
99990           continue
                eq(index) = eq(index) + (gg(i, n, mf) - gg(i, n, mb)) / 
     *deltay
              endif
              goto 50
C             special case for second order outflow at m=mc
45            continue
C             eq(index) = eq(index) + (gg(i, n, mc2) - 4. * gg(i, n, mc1) + 3. *
C *     gg(i, n, mc)) * ddy2
              do 99989 g$i$ = 1, g$p$
                g$eq(g$i$, index) = g$eq(g$i$, index) + ddy2 * g$gg(g$i$
     *, i, n, mc2) + (-ddy2 * (4.) * g$gg(g$i$, i, n, mc1)) + (ddy2 * (3
     *.) * g$gg(g$i$, i, n, mc))
99989         continue
              eq(index) = eq(index) + (gg(i, n, mc2) - 4. * gg(i, n, mc1
     *) + 3. * gg(i, n, mc)) * ddy2
50            continue
99997       continue
40          continue
99996     continue
30        continue
99995   continue
        return
      end
