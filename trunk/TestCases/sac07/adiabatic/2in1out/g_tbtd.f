C                           DISCLAIMER
C
C   This file was generated on 11/12/98 by the version of
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
      subroutine g_tbtd(g_p_, ks, jw, p, g_p, ldg_p, tb, g_tb, ldg_tb, t
     *d, g_td, ldg_td, hb, g_hb, ldg_hb, hd, g_hd, ldg_hd)
C     *******************************
C
C         CALCULATES BUBBLE AND DEW-POINT TEMPERATURES (TB AND TD) FOR
C         STREAM ASSOCIATED WITH POINT JW, AT PRESSURE P, USING VALUES
C         PRESET IN TB AND TD AS INITIAL ESTIMATES.  KS INDICATES OPTION
C              KS = 1  -  BUBBLE-POINT ONLY
C              KS = 2  -  DEW-POINT ONLY
C              KS = 3  -  BOTH BUBBLE- AND DEW-POINT
C         IF NON-CONDENSABLES OR NON-VOLATILES ARE FOUND
C         TB OR TD CALCULATED ON THE BASIS OF EQLM. VAP. FR.
C         OR EQLM. LIQ. FR. DEFINED AS VFAC*(TOTAL MOLES OF
C         NON-CONDENSABLES OR NON-VOLATILES AS THE CASE MAY BE)
C
C         WRITTEN BY R.R. HUGHES & R.K. MALIK     EES IDENT SP/TBTD
C              LAST REVISION SEPTEMBER 9,1974
C
C+@+@+@+@+@+@+@@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@@+
        real pvres
        common /qp/ csxx(12, 28), anamc(12, 6)
        common /unpt/ jout, kntrl, kflag, ncps, nptp, ncst, nrec, nen, w
     *ten(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
C     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
C     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
        dimension nmopt(2), kj(12), zf(12), ake(12)
        integer g_pmax_
        parameter (g_pmax_ = 11)
        integer g_i_, g_p_, ldg_tb, ldg_td, ldg_p, ldg_hb, ldg_hd, g_jou
     *t(g_pmax_), g_kntrl(g_pmax_), g_kflag(g_pmax_)
        integer g_ncps(g_pmax_), g_nptp(g_pmax_), g_ncst(g_pmax_), g_nre
     *c(g_pmax_), g_nen(g_pmax_), g_ktln(g_pmax_, 15)
        real r2_w, r13_b, r12_b, r12_v, r10_b, r9_b, r2_v, r3_v, r11_v, 
     *r2_b
        real r3_b, r4_v, r5_v, r4_b, r5_b, r1_w, r1_p, r2_p, r6_v, r7_b
        real g_tb(ldg_tb), g_td(ldg_td), g_t(g_pmax_), g_d(g_pmax_), g_p
     *(ldg_p), g_wten(g_pmax_, 5), g_del(g_pmax_), g_w(g_pmax_), g_wen(g
     *_pmax_, 5, 12), g_pv(g_pmax_)
        real g_pvres(g_pmax_), g_r1_w(g_pmax_), g_dt(g_pmax_), g_delp(g_
     *pmax_), g_sumz(g_pmax_), g_zf(g_pmax_, 12), g_vf(g_pmax_), g_sumd(
     *g_pmax_), g_pvk(g_pmax_), g_funxon(g_pmax_)
        real g_dfunc(g_pmax_), g_funcs(g_pmax_), g_r2_w(g_pmax_), g_alf(
     *g_pmax_), g_hb(ldg_hb), g_hd(ldg_hd), g_ptpen(g_pmax_, 5, 6), g_co
     *st(g_pmax_, 5), g_en(g_pmax_, 100)
        common /g_unpt/ g_jout, g_kntrl, g_kflag, g_ncps, g_nptp, g_ncst
     *, g_nrec, g_nen, g_wten, g_wen, g_ptpen, g_cost, g_en, g_ktln
        integer g_ehfid
        save g_zf, g_vf, g_sumd, g_pvk, g_funxon, g_dfunc, g_funcs, g_r2
     *_w, g_alf, /g_unpt/
        save g_t, g_d, g_del, g_w, g_pv, g_pvres, g_r1_w, g_dt, g_delp, 
     *g_sumz
        external g_pvap
        data vfac /0.1/, lim2 /50/
        data nmopt /'BBL', 'DEW'/
        data tz /-459.67/, ptol /1.e-10/, wtol /1.e-3/, tlim /5000./
        data eps /1.e-3/, lim /50/, fr /0.02/, dl /-30./, dh /30./
C
C         CHECK DATA, INITALIZE, AND SET GATES
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'tbtd','g_tbtd.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
2       if (p .le. ptol .or. wten(jw) .le. wtol) then
          goto 500
        endif
        if (tb .lt. tz .or. tb .gt. tlim) then
          do g_i_ = 1, g_p_
            g_tb(g_i_) = 0.0
          enddo
          tb = 0.
C--------
        endif
        if (td .lt. tz .or. td .gt. tlim) then
          do g_i_ = 1, g_p_
            g_td(g_i_) = 0.0
          enddo
          td = 0.
C--------
        endif
C
C         CHECK IF NON CONDENSABLES ARE PRESENT
C
        akbarl = 0.
        do 99999 i = 1, ncps
C+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+
          call g_pvap(g_p_, i, tb, g_tb, ldg_tb, pvres, g_pvres, g_pmax_
     *)
          ake(i) = pvres / p
5         akbarl = akbarl + wen(jw, i) / wten(jw) * alog10(ake(i) + 1.e-
     *10)
99999   continue
        akbar = 10. ** akbarl
        if (ks .eq. 2) then
          goto 7
        endif
        do 99998 i = 1, ncps
          ratiok = ake(i) / akbar
          if (ratiok .ge. 100. .and. wen(jw, i) .gt. 0.) then
            goto 351
          endif
6         continue
99998   continue
C
9       do g_i_ = 1, g_p_
          g_t(g_i_) = g_tb(g_i_)
        enddo
        t = tb
C--------
        do g_i_ = 1, g_p_
          g_d(g_i_) = wten(jw) * g_p(g_i_) + p * g_wten(g_i_, jw)
        enddo
        d = p * wten(jw)
C--------
        ms = 1
        goto 20
C
C         CHECK IF NON-VOLATILES ARE PRESENT
C
7       do 99997 i = 1, ncps
          ratiok = ake(i) / akbar
          if (ratiok .le. 0.01 .and. wen(jw, i) .gt. 0.) then
            goto 400
          endif
8         continue
99997   continue
C
C
10      do g_i_ = 1, g_p_
          g_t(g_i_) = g_td(g_i_)
        enddo
        t = td
C--------
        r3_v = wten(jw) / p
        r2_b = 1.0 / p
        r3_b = (-r3_v) / p
        do g_i_ = 1, g_p_
          g_d(g_i_) = r2_b * g_wten(g_i_, jw) + r3_b * g_p(g_i_)
        enddo
        d = r3_v
C--------
        ms = 2
C
20      if (t .lt. tz .or. t .gt. tlim) then
          do g_i_ = 1, g_p_
            g_t(g_i_) = 0.0
          enddo
          t = 0.
C--------
        endif
        assign 110 to mb
C
C         BEGIN MAJOR ITERATION LOOP FOR TEMPERATURE ADJUSTMENT
        do 99995 j = 1, lim
          do g_i_ = 1, g_p_
            g_del(g_i_) = g_d(g_i_)
          enddo
          del = d
C--------
C
C         BEGIN MINOR ITERATION LOOP FOR COMPONENT SUMMATION
          do 99996 i = 1, ncps
            do g_i_ = 1, g_p_
              g_w(g_i_) = g_wen(g_i_, jw, i)
            enddo
            w = wen(jw, i)
C--------
            if (w .le. wtol) then
              goto 100
            endif
C+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@@+@+@+@+@+@+@+@+@+@+@+@+
            call g_pvap(g_p_, i, t, g_t, g_pmax_, pvres, g_pvres, g_pmax
     *_)
            do g_i_ = 1, g_p_
              g_pv(g_i_) = g_pvres(g_i_)
            enddo
            pv = pvres
C--------
            goto (70, 80), ms
C
70          do g_i_ = 1, g_p_
              g_del(g_i_) = g_del(g_i_) + (-pv) * g_w(g_i_) + (-w) * g_p
     *v(g_i_)
            enddo
            del = del - w * pv
C--------
            goto 100
C
80          if (pv .le. ptol) then
              goto 150
            endif
            r4_v = w / pv
            r4_b = -(1.0 / pv)
            r5_b = -((-r4_v) / pv)
            do g_i_ = 1, g_p_
              g_del(g_i_) = g_del(g_i_) + r4_b * g_w(g_i_) + r5_b * g_pv
     *(g_i_)
            enddo
            del = del - r4_v
C--------
100         continue
99996     continue
C
C         CHECK FOR COMPLETION
          r3_v = del / d
          r2_b = 1.0 / d
          r3_b = (-r3_v) / d
          do g_i_ = 1, g_p_
            g_del(g_i_) = r2_b * g_del(g_i_) + r3_b * g_d(g_i_)
          enddo
          del = r3_v
C--------
          if (abs(del) .le. eps) then
            goto 300
          endif
C
C         SET UP NEXT ITERATION
          goto mb, (110, 120)
C
C              FIRST TIME - ARBITRARY DT
110       assign 120 to mb
          do g_i_ = 1, g_p_
            g_r1_w(g_i_) = fr * g_t(g_i_)
          enddo
          r1_w = fr * (t - tz)
          r3_v = sign (r1_w, del)

          if ((r1_w .eq. 0.0e0) .or. ( del .eq. 0.0e0)) then
             call ehbfSO (5,r1_w, del, r3_v, r1_p, r2_p,
     +g_ehfid,
     +249)
          else if (r1_w .gt. 0.0e0) then
             if ( del .gt. 0.0e0) then
                r1_p = 1.0e0
             else
                r1_p = -1.0e0
             endif
          else
             if ( del .gt. 0.0e0) then
                r1_p = -1.0e0
             else
                r1_p =  1.0e0
             endif
          endif

          r2_p = 0.0e0
          do g_i_ = 1, g_p_
            g_dt(g_i_) = r1_p * g_r1_w(g_i_) + r2_p * g_del(g_i_)
          enddo
          dt = r3_v
C--------
          if (ms .eq. 2) then
            do g_i_ = 1, g_p_
              g_dt(g_i_) = -g_dt(g_i_)
            enddo
            dt = -dt
C--------
          endif
          goto 130
C
C              LATER TRIES - USE NEWTONS METHOD
120       r4_v = (delp - del) / del
          r2_b = 1.0 / del
          r3_b = (-r4_v) / del + (-r2_b)
          do g_i_ = 1, g_p_
            g_delp(g_i_) = r2_b * g_delp(g_i_) + r3_b * g_del(g_i_)
          enddo
          delp = r4_v
C--------
          if (abs(delp) .gt. eps) then
            goto 128
          endif
          goto 110
128       r3_v = dt / delp
          r2_b = 1.0 / delp
          r3_b = (-r3_v) / delp
          do g_i_ = 1, g_p_
            g_dt(g_i_) = r2_b * g_dt(g_i_) + r3_b * g_delp(g_i_)
          enddo
          dt = r3_v
C--------
C
130       r2_v = min (dh, dt)

          if (dh .lt.  dt) then
             r1_p = 1.0e0
             r2_p = 0.0e0
          else if (dh .gt.  dt) then
             r1_p = 0.0e0
             r2_p = 1.0e0
          else
             call ehbfSO (8,dh, dt, r2_v, r1_p, r2_p,
     +g_ehfid,
     +312)
             r2_p = 1.0e0 -  r1_p
          endif
          do g_i_ = 1, g_p_
            g_r1_w(g_i_) = r2_p * g_dt(g_i_)
          enddo
          r1_w = r2_v
          r2_v = max (dl, r1_w)

          if (dl .gt.  r1_w) then
             r1_p = 1.0e0
             r2_p = 0.0e0
          else if (dl .lt.  r1_w) then
             r1_p = 0.0e0
             r2_p = 1.0e0
          else
             call ehbfSO (7,dl, r1_w, r2_v, r1_p, r2_p,
     +g_ehfid,
     +330)
             r2_p = 1.0e0 -  r1_p
          endif
          do g_i_ = 1, g_p_
            g_dt(g_i_) = r2_p * g_r1_w(g_i_)
          enddo
          dt = r2_v
C--------
          do g_i_ = 1, g_p_
            g_delp(g_i_) = g_del(g_i_)
          enddo
          delp = del
C--------
          goto 200
C
C         FOR LOW VOLATILITY IN DEW POINT CALCULATION, RAISE TEMPERATURE
C         ARBITRARILY
150       do g_i_ = 1, g_p_
            g_dt(g_i_) = 0.0
          enddo
          dt = dh
C--------
          assign 110 to mb
C
200       do g_i_ = 1, g_p_
            g_t(g_i_) = g_t(g_i_) + g_dt(g_i_)
          enddo
          t = t + dt
C--------
99995   continue
C
C
C         PRINT ERROR FLAG IF DO-LOOP HAS NORMAL EXIT
210     nmp = nmopt(ms)
C*ADIFOR* I/O statement contains active variables
        write (jout, 9200) nmp, lim, p, tb, td, dt, del, delp, eps, wten
     *(jw), (wen(jw, k), k = 1, ncps)
C
C         ONE CALCULATION COMPLETE, - STORE RESULT AND CHECK SPECS FOR 2ND
300     goto (310, 320), ms
C
310     do g_i_ = 1, g_p_
          g_tb(g_i_) = g_t(g_i_)
        enddo
        tb = t
C--------
        if (ks .eq. 3) then
          goto 7
        endif
        goto 330
320     do g_i_ = 1, g_p_
          g_td(g_i_) = g_t(g_i_)
        enddo
        td = t
C--------
330     if (tb .lt. tz .or. tb .gt. tlim) then
          goto 335
        endif
        if (td .gt. tz .and. td .lt. tlim) then
          goto 340
        endif
C*ADIFOR* I/O statement contains active variables
335     write (jout, 9355) tb, td
        stop
340     if (kflag .le. 1) then
          goto 350
        endif
C*ADIFOR* I/O statement contains active variables
        write (jout, 9330) p
        if (ks .ne. 2) then
C*ADIFOR* I/O statement contains active variables
          write (jout, 9331) tb
        endif
        if (ks .gt. 1) then
C*ADIFOR* I/O statement contains active variables
          write (jout, 9332) td
        endif
350     return
C
C         CALCULATION OF BUBBLEPOINT - NONCONDENSABLES CASE
351     continue
        j = 0
        do g_i_ = 1, g_p_
          g_sumz(g_i_) = 0.0
        enddo
        sumz = 0.
C--------
        do 99994 i = 1, ncps
          ratiok = ake(i) / akbar
          if (ratiok .lt. 100.) then
            goto 360
          endif
          j = j + 1
          r3_v = wen(jw, i) / wten(jw)
          r2_b = 1.0 / wten(jw)
          r3_b = (-r3_v) / wten(jw)
          do g_i_ = 1, g_p_
            g_zf(g_i_, j) = r2_b * g_wen(g_i_, jw, i) + r3_b * g_wten(g_
     *i_, jw)
          enddo
          zf(j) = r3_v
C--------
          kj(j) = i
          do g_i_ = 1, g_p_
            g_sumz(g_i_) = g_sumz(g_i_) + g_zf(g_i_, j)
          enddo
          sumz = sumz + zf(j)
C--------
360       continue
99994   continue
        r2_b = 1. + vfac
        do g_i_ = 1, g_p_
          g_vf(g_i_) = r2_b * g_sumz(g_i_)
        enddo
        vf = (1. + vfac) * sumz
C--------
        r2_v = max (0.001, vf)

        if (0.001 .gt.  vf) then
           r1_p = 1.0e0
           r2_p = 0.0e0
        else if (0.001 .lt.  vf) then
           r1_p = 0.0e0
           r2_p = 1.0e0
        else
           call ehbfSO (7,0.001, vf, r2_v, r1_p, r2_p,
     +g_ehfid,
     +457)
           r2_p = 1.0e0 -  r1_p
        endif
        do g_i_ = 1, g_p_
          g_vf(g_i_) = r2_p * g_vf(g_i_)
        enddo
        vf = r2_v
C--------
C*ADIFOR* I/O statement contains active variables
        write (jout, 9350) vf
C         USE NEWTON'S METHOD
        do g_i_ = 1, g_p_
          g_t(g_i_) = g_tb(g_i_)
        enddo
        t = tb
C--------
        i1 = 1
        do 99991 i = 1, lim2
          do g_i_ = 1, g_p_
            g_sumd(g_i_) = 0.0
          enddo
          sumd = 0.
C--------
          do 99992 k = 1, ncps
            do 99993 kk = 1, j
366           if (k .eq. kj(kk)) then
                goto 365
              endif
99993       continue
C+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@@+@+@+@+@+@++@+@+@+@+@
            call g_pvap(g_p_, k, t, g_t, g_pmax_, pvres, g_pvres, g_pmax
     *_)
            r3_v = pvres / p
            r2_b = 1.0 / p
            r3_b = (-r3_v) / p
            do g_i_ = 1, g_p_
              g_pvk(g_i_) = r2_b * g_pvres(g_i_) + r3_b * g_p(g_i_)
            enddo
            pvk = r3_v
C--------
            r4_v = wen(jw, k) / wten(jw)
            r6_v = pvk - 1.
            r11_v = vf * pvk + 1. - vf
            r12_v = r4_v * r6_v / r11_v
            r4_b = 1.0 / r11_v
            r5_b = (-r12_v) / r11_v
            r7_b = -r5_b + r5_b * pvk
            r10_b = r4_b * r6_v
            r9_b = r5_b * vf + r4_b * r4_v
            r12_b = r10_b * (1.0 / wten(jw))
            r13_b = r10_b * ((-r4_v) / wten(jw))
            do g_i_ = 1, g_p_
              g_sumd(g_i_) = g_sumd(g_i_) + r12_b * g_wen(g_i_, jw, k) +
     * r13_b * g_wten(g_i_, jw) + r9_b * g_pvk(g_i_) + r7_b * g_vf(g_i_)
            enddo
            sumd = sumd + r12_v
C--------
365         continue
99992     continue
          do g_i_ = 1, g_p_
            g_funxon(g_i_) = g_sumd(g_i_)
          enddo
          funxon = sumd + 1. / (1. + vfac)
C--------
          if (abs(funxon) .lt. eps) then
            goto 310
          endif
          if (i .gt. 1) then
            goto 368
          endif
          do g_i_ = 1, g_p_
            g_dfunc(g_i_) = 0.0
          enddo
          dfunc = 0.
C--------
          goto 369
368       r4_v = funxon - funcs
          r5_v = dt / r4_v
          r2_b = 1.0 / r4_v
          r3_b = (-r5_v) / r4_v
          do g_i_ = 1, g_p_
            g_dfunc(g_i_) = r2_b * g_dt(g_i_) + r3_b * g_funxon(g_i_) + 
     *(-r3_b) * g_funcs(g_i_)
          enddo
          dfunc = r5_v
C--------
369       r2_v = abs(dfunc)

          if (dfunc .gt. 0.0e0) then
             r1_p =  1.0e0
          else if (dfunc .lt. 0.0e0) then
             r1_p = -1.0e0
          else
             call ehufSO (3,dfunc, r2_v, r1_p,
     +g_ehfid,
     +552)
          endif
          do g_i_ = 1, g_p_
            g_r2_w(g_i_) = r1_p * g_dfunc(g_i_)
          enddo
          r2_w = r2_v
          r2_v = max ((1.e-2), r2_w)

          if ((1.e-2) .gt.  r2_w) then
             r1_p = 1.0e0
             r2_p = 0.0e0
          else if ((1.e-2) .lt.  r2_w) then
             r1_p = 0.0e0
             r2_p = 1.0e0
          else
             call ehbfSO (7,1.e-2, r2_w, r2_v, r1_p, r2_p,
     +g_ehfid,
     +569)
             r2_p = 1.0e0 -  r1_p
          endif
          do g_i_ = 1, g_p_
            g_r1_w(g_i_) = r2_p * g_r2_w(g_i_)
          enddo
          r1_w = r2_v
          r5_v = sign (r1_w, dfunc)

          if ((r1_w .eq. 0.0e0) .or. ( dfunc .eq. 0.0e0)) then
             call ehbfSO (5,r1_w, dfunc, r5_v, r1_p, r2_p,
     +g_ehfid,
     +581)
          else if (r1_w .gt. 0.0e0) then
             if ( dfunc .gt. 0.0e0) then
                r1_p = 1.0e0
             else
                r1_p = -1.0e0
             endif
          else
             if ( dfunc .gt. 0.0e0) then
                r1_p = -1.0e0
             else
                r1_p =  1.0e0
             endif
          endif

          r2_p = 0.0e0
          r4_b = (-funxon) * r1_p
          r5_b = (-funxon) * r2_p
          do g_i_ = 1, g_p_
            g_dt(g_i_) = (-r5_v) * g_funxon(g_i_) + r4_b * g_r1_w(g_i_) 
     *+ r5_b * g_dfunc(g_i_)
          enddo
          dt = (-funxon) * r5_v
C--------
          r2_v = min (dh, dt)

          if (dh .lt.  dt) then
             r1_p = 1.0e0
             r2_p = 0.0e0
          else if (dh .gt.  dt) then
             r1_p = 0.0e0
             r2_p = 1.0e0
          else
             call ehbfSO (8,dh, dt, r2_v, r1_p, r2_p,
     +g_ehfid,
     +616)
             r2_p = 1.0e0 -  r1_p
          endif
          do g_i_ = 1, g_p_
            g_r1_w(g_i_) = r2_p * g_dt(g_i_)
          enddo
          r1_w = r2_v
          r2_v = max (dl, r1_w)

          if (dl .gt.  r1_w) then
             r1_p = 1.0e0
             r2_p = 0.0e0
          else if (dl .lt.  r1_w) then
             r1_p = 0.0e0
             r2_p = 1.0e0
          else
             call ehbfSO (7,dl, r1_w, r2_v, r1_p, r2_p,
     +g_ehfid,
     +634)
             r2_p = 1.0e0 -  r1_p
          endif
          do g_i_ = 1, g_p_
            g_dt(g_i_) = r2_p * g_r1_w(g_i_)
          enddo
          dt = r2_v
C--------
          if (abs(dt) .le. eps) then
            goto 310
          endif
          do g_i_ = 1, g_p_
            g_funcs(g_i_) = g_funxon(g_i_)
          enddo
          funcs = funxon
C--------
371       if ((t + dt) .gt. tz .and. (t + dt) .lt. tlim) then
            goto 370
          endif
          r2_b = 1.0 / 2.
          do g_i_ = 1, g_p_
            g_dt(g_i_) = r2_b * g_dt(g_i_)
          enddo
          dt = dt / 2.
C--------
          goto 371
370       do g_i_ = 1, g_p_
            g_t(g_i_) = g_t(g_i_) + g_dt(g_i_)
          enddo
          t = t + dt
C--------
99991   continue
        ms = 1
        goto 210
C
C         CALCULATION OF DEW POINT NON-VOLATILES CASE
C
400     if (ks .eq. 1) then
          goto 9
        endif
        j = 0
        do g_i_ = 1, g_p_
          g_sumz(g_i_) = 0.0
        enddo
        sumz = 0.
C--------
        do 99990 i = 1, ncps
          ratiok = ake(i) / akbar
          if (ratiok .gt. .01) then
            goto 410
          endif
          j = j + 1
          r3_v = wen(jw, i) / wten(jw)
          r2_b = 1.0 / wten(jw)
          r3_b = (-r3_v) / wten(jw)
          do g_i_ = 1, g_p_
            g_zf(g_i_, j) = r2_b * g_wen(g_i_, jw, i) + r3_b * g_wten(g_
     *i_, jw)
          enddo
          zf(j) = r3_v
C--------
          kj(j) = i
          do g_i_ = 1, g_p_
            g_sumz(g_i_) = g_sumz(g_i_) + g_zf(g_i_, j)
          enddo
          sumz = sumz + zf(j)
C--------
410       continue
99990   continue
        r2_b = 1. + vfac
        do g_i_ = 1, g_p_
          g_alf(g_i_) = r2_b * g_sumz(g_i_)
        enddo
        alf = (1. + vfac) * sumz
C--------
        r2_v = max (0.001, alf)

        if (0.001 .gt.  alf) then
           r1_p = 1.0e0
           r2_p = 0.0e0
        else if (0.001 .lt.  alf) then
           r1_p = 0.0e0
           r2_p = 1.0e0
        else
           call ehbfSO (7,0.001, alf, r2_v, r1_p, r2_p,
     +g_ehfid,
     +720)
           r2_p = 1.0e0 -  r1_p
        endif
        do g_i_ = 1, g_p_
          g_alf(g_i_) = r2_p * g_alf(g_i_)
        enddo
        alf = r2_v
C--------
        do g_i_ = 1, g_p_
          g_vf(g_i_) = -g_alf(g_i_)
        enddo
        vf = 1. - alf
C--------
C*ADIFOR* I/O statement contains active variables
        write (jout, 9400) alf
C         USE NEWTON'S METHOD
        do g_i_ = 1, g_p_
          g_t(g_i_) = g_td(g_i_)
        enddo
        t = td
C--------
        i1 = 1
        do 99987 i = 1, lim2
          do g_i_ = 1, g_p_
            g_sumd(g_i_) = 0.0
          enddo
          sumd = 0.
C--------
          do 99988 k = 1, ncps
            do 99989 kk = 1, j
419           if (k .eq. kj(kk)) then
                goto 418
              endif
99989       continue
C+@+@+@+@+@++@+@+@+@@@@@@@@@++++++++++++++++++++++++
            call g_pvap(g_p_, k, t, g_t, g_pmax_, pvres, g_pvres, g_pmax
     *_)
            r3_v = pvres / p
            r2_b = 1.0 / p
            r3_b = (-r3_v) / p
            do g_i_ = 1, g_p_
              g_pvk(g_i_) = r2_b * g_pvres(g_i_) + r3_b * g_p(g_i_)
            enddo
            pvk = r3_v
C--------
            r4_v = wen(jw, k) / wten(jw)
            r6_v = pvk - 1.
            r11_v = vf * pvk + 1. - vf
            r12_v = r4_v * r6_v / r11_v
            r4_b = 1.0 / r11_v
            r5_b = (-r12_v) / r11_v
            r7_b = -r5_b + r5_b * pvk
            r10_b = r4_b * r6_v
            r9_b = r5_b * vf + r4_b * r4_v
            r12_b = r10_b * (1.0 / wten(jw))
            r13_b = r10_b * ((-r4_v) / wten(jw))
            do g_i_ = 1, g_p_
              g_sumd(g_i_) = g_sumd(g_i_) + r12_b * g_wen(g_i_, jw, k) +
     * r13_b * g_wten(g_i_, jw) + r9_b * g_pvk(g_i_) + r7_b * g_vf(g_i_)
            enddo
            sumd = sumd + r12_v
C--------
418         continue
99988     continue
          do g_i_ = 1, g_p_
            g_funxon(g_i_) = g_sumd(g_i_)
          enddo
          funxon = sumd + 1. / (1. + vfac)
C--------
          if (abs(funxon) .lt. eps) then
            goto 320
          endif
          if (i .gt. 1) then
            goto 412
          endif
          do g_i_ = 1, g_p_
            g_dfunc(g_i_) = 0.0
          enddo
          dfunc = 0.
C--------
          goto 413
412       r4_v = funxon - funcs
          r5_v = dt / r4_v
          r2_b = 1.0 / r4_v
          r3_b = (-r5_v) / r4_v
          do g_i_ = 1, g_p_
            g_dfunc(g_i_) = r2_b * g_dt(g_i_) + r3_b * g_funxon(g_i_) + 
     *(-r3_b) * g_funcs(g_i_)
          enddo
          dfunc = r5_v
C--------
413       r2_v = abs(dfunc)

          if (dfunc .gt. 0.0e0) then
             r1_p =  1.0e0
          else if (dfunc .lt. 0.0e0) then
             r1_p = -1.0e0
          else
             call ehufSO (3,dfunc, r2_v, r1_p,
     +g_ehfid,
     +820)
          endif
          do g_i_ = 1, g_p_
            g_r2_w(g_i_) = r1_p * g_dfunc(g_i_)
          enddo
          r2_w = r2_v
          r2_v = max ((1.e-2), r2_w)

          if ((1.e-2) .gt.  r2_w) then
             r1_p = 1.0e0
             r2_p = 0.0e0
          else if ((1.e-2) .lt.  r2_w) then
             r1_p = 0.0e0
             r2_p = 1.0e0
          else
             call ehbfSO (7,1.e-2, r2_w, r2_v, r1_p, r2_p,
     +g_ehfid,
     +837)
             r2_p = 1.0e0 -  r1_p
          endif
          do g_i_ = 1, g_p_
            g_r1_w(g_i_) = r2_p * g_r2_w(g_i_)
          enddo
          r1_w = r2_v
          r5_v = sign (r1_w, dfunc)

          if ((r1_w .eq. 0.0e0) .or. ( dfunc .eq. 0.0e0)) then
             call ehbfSO (5,r1_w, dfunc, r5_v, r1_p, r2_p,
     +g_ehfid,
     +849)
          else if (r1_w .gt. 0.0e0) then
             if ( dfunc .gt. 0.0e0) then
                r1_p = 1.0e0
             else
                r1_p = -1.0e0
             endif
          else
             if ( dfunc .gt. 0.0e0) then
                r1_p = -1.0e0
             else
                r1_p =  1.0e0
             endif
          endif

          r2_p = 0.0e0
          r4_b = (-funxon) * r1_p
          r5_b = (-funxon) * r2_p
          do g_i_ = 1, g_p_
            g_dt(g_i_) = (-r5_v) * g_funxon(g_i_) + r4_b * g_r1_w(g_i_) 
     *+ r5_b * g_dfunc(g_i_)
          enddo
          dt = (-funxon) * r5_v
C--------
          r2_v = min (dh, dt)

          if (dh .lt.  dt) then
             r1_p = 1.0e0
             r2_p = 0.0e0
          else if (dh .gt.  dt) then
             r1_p = 0.0e0
             r2_p = 1.0e0
          else
             call ehbfSO (8,dh, dt, r2_v, r1_p, r2_p,
     +g_ehfid,
     +884)
             r2_p = 1.0e0 -  r1_p
          endif
          do g_i_ = 1, g_p_
            g_r1_w(g_i_) = r2_p * g_dt(g_i_)
          enddo
          r1_w = r2_v
          r2_v = max (dl, r1_w)

          if (dl .gt.  r1_w) then
             r1_p = 1.0e0
             r2_p = 0.0e0
          else if (dl .lt.  r1_w) then
             r1_p = 0.0e0
             r2_p = 1.0e0
          else
             call ehbfSO (7,dl, r1_w, r2_v, r1_p, r2_p,
     +g_ehfid,
     +902)
             r2_p = 1.0e0 -  r1_p
          endif
          do g_i_ = 1, g_p_
            g_dt(g_i_) = r2_p * g_r1_w(g_i_)
          enddo
          dt = r2_v
C--------
          if (abs(dt) .lt. eps) then
            goto 320
          endif
          do g_i_ = 1, g_p_
            g_funcs(g_i_) = g_funxon(g_i_)
          enddo
          funcs = funxon
C--------
414       if ((t + dt) .gt. tz .and. (t + dt) .lt. tlim) then
            goto 420
          endif
          r2_b = 1.0 / 2.
          do g_i_ = 1, g_p_
            g_dt(g_i_) = r2_b * g_dt(g_i_)
          enddo
          dt = dt / 2.
C--------
          goto 414
420       do g_i_ = 1, g_p_
            g_t(g_i_) = g_t(g_i_) + g_dt(g_i_)
          enddo
          t = t + dt
C--------
99987   continue
        ms = 2
        goto 210
C
C*ADIFOR* I/O statement contains active variables
500     write (jout, 9500) ks, jw, p, tb, td, wten(jw)
        return
C
C
9200    format ('0**** ',a3,'-POINT CALCN HAS NOT CONVERGED IN ',i3,' IT
     *ERATIONS -'/6x,'DATA AND RESULTS ARE -'/6x,'P =',f10.4,', TB =',f1
     *0.4,', TD =',f10.4,', DT =',f10.4/6x,'DEL =',f12.8,', DEL(PREV.) =
     *',f12.8,', EPS =',f12.8/6x,'FLOWS (LB MOLS/HR) - TOTAL = ',f12.3/(
     *3x,6f11.3))
C
9330    format (' AT',f10.5,' PSIA,')
9331    format ('+',20x,'BBLE PT =',f10.5,' DF,')
9332    format ('+',44x,'DEW PT =',f10.5)
9350    format ('0****NON-CONDENSABLES PRESENT,BUBBLE POINT CAL. WITH VF
     *= ',f6.3)
9355    format (/,10x,'BUBBLE AND/OR DEW POINT TEMPS OUT OF BOUNDS',2e20
     *.4)
9400    format ('0****NON-VOLATILES PRESENT,DEW POINT CAL. WITH LF= ',f6
     *.3)
C
9500    format ('0**** TBTD HAS BAD DATA --- KS =',i3,', JW =',i3/6x,'P 
     *=',f10.4,', TB =',f10.4,', TD =',f10.4,', WT =',f12.3)
C
      end
