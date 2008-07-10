      subroutine g$substh$6(g$p$, m, sthout, g$sthout, ldg$sthout)
C       
C       Common block // contains active variables.
C       Variable hh in Common block // is active.
C       Variable gg in Common block // is active.
C       Variable ff in Common block // is active.
C       Variable eq in Common block // is active.
C       Variable u in Common block // is active.
C       Formal sthout is active.
C       
        integer g$p$
        integer g$pmax$
        parameter (g$pmax$ = 190)
        integer g$i$
        double precision d$6bar
        double precision d$5bar
        double precision d$3bar
        double precision g$sy(g$pmax$)
        double precision g$smp1(g$pmax$), g$sm(g$pmax$), g$smm1(g$pmax$)
     *, g$smm2(g$pmax$), g$smm3(g$pmax$)
c        real g$sthout(ldg$sthout)
        double precision g$sthout(ldg$sthout)
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
        integer ldg$sthout
C       
        implicit double precision (a-h, o-z)
        common // u(190), eq(190), eqold(190), a(190, 95), eqplus(190), 
     *b(10), bth(10), ff(3, 6, 10), gg(3, 6, 10), hh(3, 6, 10), mc, nc, 
     *dx, dy, ddx, ddy, ddx2, ddy2, mc1, nc1, mc2, mc3, theta1, gamma, g
     *m1, xmach, ncf, mcncf, qinc, dinc, pinc, einc, ainc, htot, swall, 
     *dstag, emaxs(200), eqmaxs(200), delta, kdiag, kblk, ksize, kblk2, 
     *kblk3, itmax, sstol, alpha, nprint, emax, eqmax, kprint, dtheta, i
     *cont, xend, xincr
C       Added variables smp1,sm,smm1,smm2,smm3 to hold values
C       from calls to subs() - PDH
        double precision smp1, sm, smm1, smm2, smm3
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
        if (m .ne. 1) then
          goto 10
        endif
        sthout = 0.
        do 99994 g$i$ = 1, g$p$
          g$sthout(g$i$) = 0.0
99994   continue
        return
10      continue
C       if(m .eq. 2)sy=(2.*s(m)-3.*s(m-1)+s(m+1))/(3.*dy)
        if (m .eq. 2) then
          call g$subs$6(g$p$, m, sm, g$sm(1), g$pmax$)
          call g$subs$6(g$p$, m - 1, smm1, g$smm1(1), g$pmax$)
          call g$subs$6(g$p$, m + 1, smp1, g$smp1(1), g$pmax$)
C         sy = (2. * sm - 3. * smm1 + smp1) / (3. * dy)
          d$3bar = (1.0d0 / (3. * dy))
          do 99993 g$i$ = 1, g$p$
            g$sy(g$i$) = d$3bar * 2. * g$sm(g$i$) + (-d$3bar * (3.) * g$
     *smm1(g$i$)) + (d$3bar * g$smp1(g$i$))
99993     continue
          sy = (2. * (sm) - (3. * (smm1)) + (smp1)) / (3. * (dy))
        endif
C       if(m .ge. 3 .and. m .le. (mc-1))sy=(s(m-2)-6.*s(m-1)
C       1 +3.*s(m)+2.*s(m+1))/(6.*dy)
        if (m .ge. 3 .and. m .le. (mc - 1)) then
          call g$subs$6(g$p$, m + 1, smp1, g$smp1(1), g$pmax$)
          call g$subs$6(g$p$, m, sm, g$sm(1), g$pmax$)
          call g$subs$6(g$p$, m - 1, smm1, g$smm1(1), g$pmax$)
          call g$subs$6(g$p$, m - 2, smm2, g$smm2(1), g$pmax$)
C         sy = (smm2 - 6. * smm1 + 3. * sm + 2. * smp1) / (6. * dy)
          d$5bar = (1.0d0 / (6. * dy))
          do 99992 g$i$ = 1, g$p$
            g$sy(g$i$) = d$5bar * g$smm2(g$i$) + (-d$5bar * (6.) * g$smm
     *1(g$i$)) + (d$5bar * (3.) * g$sm(g$i$)) + (d$5bar * (2.) * g$smp1(
     *g$i$))
99992     continue
          sy = (smm2 - (6. * (smm1)) + (3. * (sm)) + (2. * (smp1))) / (6
     *. * (dy))
        endif
C       if(m .eq. mc)sy=(-2.*s(m-3)+9.*s(m-2)-18.*s(m-1)+11.*s(m))
C       1 /(6.*dy)
        if (m .eq. mc) then
          call g$subs$6(g$p$, m, sm, g$sm(1), g$pmax$)
          call g$subs$6(g$p$, m - 1, smm1, g$smm1(1), g$pmax$)
          call g$subs$6(g$p$, m - 2, smm2, g$smm2(1), g$pmax$)
          call g$subs$6(g$p$, m - 3, smm3, g$smm3(1), g$pmax$)
C         sy = (-2. * smm3 + 9. * smm2 - 18. * smm1 + 11. * sm) / (6. * dy)
          d$6bar = (1.0d0 / (6. * dy))
          do 99991 g$i$ = 1, g$p$
            g$sy(g$i$) = d$6bar * (-2.) * g$smm3(g$i$) + (d$6bar * (9.) 
     ** g$smm2(g$i$)) + (-d$6bar * (18.) * g$smm1(g$i$)) + (d$6bar * (11
     *.) * g$sm(g$i$))
99991     continue
          sy = (-2. * (smm3) + (9. * (smm2)) - (18. * (smm1)) + (11. * (
     *sm))) / (6. * (dy))
        endif
C       sthout = sy / theta1
        do 99990 g$i$ = 1, g$p$
          g$sthout(g$i$) = (1.0d0 / theta1) * g$sy(g$i$)
99990   continue
        sthout = sy / theta1
        return
      end
