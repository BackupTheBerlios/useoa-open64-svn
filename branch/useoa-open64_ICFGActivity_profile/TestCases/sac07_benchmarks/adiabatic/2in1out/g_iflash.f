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
      subroutine g_iflash(g_p_, jw)
C     *********************
C
C         ISOTHERMAL FLASH OF STREAM JW AT PRESSURE AND TEMP PRESET IN
C         PTPEN(JW,...). USES PVAP TO CALCULATE EQUILIBRIUM CONSTANTS
C         RETURNS MOL FRACTION VAPOR IN PTPEN(JW,4), COMPONENT VAPOR
C         FLOWS IN FV(I) AND COMPONENT LIQUID FLOWS IN FL(I).
C
C         WRITTEN BY R.R. HUGHES             EES IDENT  SP/IFLASH
C              LAST REVISION MAY 9, 1974
C
        real numer, pvres
        common /unpt/ jout, kntrl, kflag, ncps, nptp, ncst, nrec, nen, w
     *ten(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
C     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
C     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
        common /flos/ fv(12), fl(12), fw(12)
        common /qp/ csxx(12, 28), anamc(12, 6)
        dimension rk(12)
        integer g_pmax_
        parameter (g_pmax_ = 11)
        integer g_i_, g_p_, g_jout(g_pmax_), g_kntrl(g_pmax_), g_kflag(g
     *_pmax_), g_ncps(g_pmax_), g_nptp(g_pmax_), g_ncst(g_pmax_), g_nrec
     *(g_pmax_), g_nen(g_pmax_)
        integer g_ktln(g_pmax_, 15)
        real r1_p, r8_b, r7_b, r6_b, r6_v, r2_v, r3_v, r7_v, r2_b, r3_b
        real r4_v, r4_b, r5_v, r5_b, g_rk(g_pmax_, 12), g_pvres(g_pmax_)
     *, g_ptpen(g_pmax_, 5, 6), g_psi(g_pmax_), g_test(g_pmax_), g_wten(
     *g_pmax_, 5)
        real g_deriv(g_pmax_), g_numer(g_pmax_), g_denom(g_pmax_), g_wen
     *(g_pmax_, 5, 12), g_del(g_pmax_), g_r(g_pmax_), g_fv(g_pmax_, 12),
     * g_fl(g_pmax_, 12), g_cost(g_pmax_, 5), g_en(g_pmax_, 100)
        real g_fw(g_pmax_, 12)
        common /g_unpt/ g_jout, g_kntrl, g_kflag, g_ncps, g_nptp, g_ncst
     *, g_nrec, g_nen, g_wten, g_wen, g_ptpen, g_cost, g_en, g_ktln
        common /g_flos/ g_fv, g_fl, g_fw
        integer g_ehfid
        save /g_flos/
        save g_rk, g_pvres, g_psi, g_test, g_deriv, g_numer, g_denom, g_
     *del, g_r, /g_unpt/
        external g_pvap
        data rkmin /1.e-15/
C
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'iflash','g_iflash.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        do 99999 i = 1, ncps
C@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+
          call g_pvap(g_p_, i, ptpen(jw, 2), g_ptpen(1, jw, 2), g_pmax_,
     * pvres, g_pvres, g_pmax_)
          r3_v = pvres / ptpen(jw, 1)
          r2_b = 1.0 / ptpen(jw, 1)
          r3_b = (-r3_v) / ptpen(jw, 1)
          do g_i_ = 1, g_p_
            g_rk(g_i_, i) = r2_b * g_pvres(g_i_) + r3_b * g_ptpen(g_i_, 
     *jw, 1)
          enddo
          rk(i) = r3_v
C--------
          if (rk(i) .ge. rkmin) then
            goto 10
          endif
          write (jout, 8010) i
          do g_i_ = 1, g_p_
            g_rk(g_i_, i) = 0.0
          enddo
          rk(i) = rkmin
C--------
10        continue
99999   continue
C
C              FIRST TRY METHOD FOR LOW VAPORIZATION
        mcal = 1
        do g_i_ = 1, g_p_
          g_psi(g_i_) = 0.0
        enddo
        psi = 1.
C--------
C
C         START BASIC ALGORITHMS
20      assign 35 to mdo
C
        do 99997 k = 1, 20
          do g_i_ = 1, g_p_
            g_test(g_i_) = -g_wten(g_i_, jw)
          enddo
          test = -wten(jw)
C--------
          do g_i_ = 1, g_p_
            g_deriv(g_i_) = 0.0
          enddo
          deriv = 0.
C--------
C
          do 99998 i = 1, ncps
C
            goto (24, 26), mcal
24          r2_v = 1. / rk(i)
            r3_b = -((-r2_v) / rk(i))
            do g_i_ = 1, g_p_
              g_numer(g_i_) = r3_b * g_rk(g_i_, i)
            enddo
            numer = 1. - r2_v
C--------
            do g_i_ = 1, g_p_
              g_denom(g_i_) = (-numer) * g_psi(g_i_) + (-psi) * g_numer(
     *g_i_)
            enddo
            denom = 1. - psi * numer
C--------
            goto 28
26          do g_i_ = 1, g_p_
              g_numer(g_i_) = g_rk(g_i_, i)
            enddo
            numer = rk(i) - 1.
C--------
            do g_i_ = 1, g_p_
              g_denom(g_i_) = g_rk(g_i_, i) + (-numer) * g_psi(g_i_) + (
     *-psi) * g_numer(g_i_)
            enddo
            denom = rk(i) - psi * numer
C--------
C
28          r4_v = wen(jw, i) / denom
            r4_b = 1.0 / denom
            r5_b = (-r4_v) / denom
            do g_i_ = 1, g_p_
              g_test(g_i_) = g_test(g_i_) + r4_b * g_wen(g_i_, jw, i) + 
     *r5_b * g_denom(g_i_)
            enddo
            test = test + r4_v
C--------
30          r6_v = denom * denom
            r1_p = 2.0e0 * denom
            r7_v = wen(jw, i) * numer / r6_v
            r4_b = 1.0 / r6_v
            r6_b = (-r7_v) / r6_v * r1_p
            r7_b = r4_b * numer
            r8_b = r4_b * wen(jw, i)
            do g_i_ = 1, g_p_
              g_deriv(g_i_) = g_deriv(g_i_) + r7_b * g_wen(g_i_, jw, i) 
     *+ r8_b * g_numer(g_i_) + r6_b * g_denom(g_i_)
            enddo
            deriv = deriv + r7_v
C--------
99998     continue
C
          goto mdo, (35, 40)
35        if (test .le. 0.) then
            goto (200, 300), mcal
          endif
C              (SINGLE PHASE, - OUTSIDE BUBBLE PRESSURE/DEW PRES. RANGE)
          assign 40 to mdo
40        r3_v = test / deriv
          r2_b = 1.0 / deriv
          r3_b = (-r3_v) / deriv
          do g_i_ = 1, g_p_
            g_del(g_i_) = r2_b * g_test(g_i_) + r3_b * g_deriv(g_i_)
          enddo
          del = r3_v
C--------
          do g_i_ = 1, g_p_
            g_psi(g_i_) = g_psi(g_i_) + (-g_del(g_i_))
          enddo
          psi = psi - del
C--------
          if (abs(del) .le. 1.e-6) then
            goto 100
          endif
C
C         FOR LARGE VAPORIZATION, SWITCH TO ALTERNATIVE CALCULATION
          if (mcal .eq. 1 .and. psi .lt. 0.4) then
            goto 60
          endif
50        continue
99997   continue
C
C*ADIFOR* I/O statement contains active variables
        write (jout, 8050) mcal, del, psi
        goto 100
C
C         SECOND METHOD USES DIFFERENT TEST FUNCTION
60      mcal = 2
        do g_i_ = 1, g_p_
          g_psi(g_i_) = 0.0
        enddo
        psi = 0.
C--------
        goto 20
C
C         PERCENT VAPORIZED ESTABLISHED - CALCULATION DEPENDS ON DEGREE
C         OF VAPORIZATION
100     goto (110, 130), mcal
110     if (psi .gt. 0.999999) then
          goto 200
        endif
        r2_v = 1. - psi
        r3_v = psi / r2_v
        r2_b = 1.0 / r2_v + (-((-r3_v) / r2_v))
        do g_i_ = 1, g_p_
          g_r(g_i_) = r2_b * g_psi(g_i_)
        enddo
        r = r3_v
C--------
        do 99996 i = 1, ncps
          r4_v = r / rk(i)
          r5_v = 1. + r4_v
          r6_v = wen(jw, i) / r5_v
          r2_b = 1.0 / r5_v
          r4_b = (-r6_v) / r5_v
          r5_b = r4_b * (1.0 / rk(i))
          r6_b = r4_b * ((-r4_v) / rk(i))
          do g_i_ = 1, g_p_
            g_fv(g_i_, i) = r2_b * g_wen(g_i_, jw, i) + r5_b * g_r(g_i_)
     * + r6_b * g_rk(g_i_, i)
          enddo
          fv(i) = r6_v
C--------
120       do g_i_ = 1, g_p_
            g_fl(g_i_, i) = g_wen(g_i_, jw, i) + (-g_fv(g_i_, i))
          enddo
          fl(i) = wen(jw, i) - fv(i)
C--------
99996   continue
        goto 900
C
130     if (psi .lt. 1.e-6) then
          goto 300
        endif
        r3_v = (1. - psi) / psi
        r3_b = (-r3_v) / psi + (-(1.0 / psi))
        do g_i_ = 1, g_p_
          g_r(g_i_) = r3_b * g_psi(g_i_)
        enddo
        r = r3_v
C--------
        do 99995 i = 1, ncps
          r5_v = 1. + r * rk(i)
          r6_v = wen(jw, i) / r5_v
          r2_b = 1.0 / r5_v
          r4_b = (-r6_v) / r5_v
          r5_b = r4_b * rk(i)
          r6_b = r4_b * r
          do g_i_ = 1, g_p_
            g_fl(g_i_, i) = r2_b * g_wen(g_i_, jw, i) + r5_b * g_r(g_i_)
     * + r6_b * g_rk(g_i_, i)
          enddo
          fl(i) = r6_v
C--------
140       do g_i_ = 1, g_p_
            g_fv(g_i_, i) = g_wen(g_i_, jw, i) + (-g_fl(g_i_, i))
          enddo
          fv(i) = wen(jw, i) - fl(i)
C--------
99995   continue
        goto 900
C
C         LIQUID ONLY, - AT OR ABOVE THE BUBBLE PRESSURE
200     do 99994 i = 1, ncps
          do g_i_ = 1, g_p_
            g_fl(g_i_, i) = g_wen(g_i_, jw, i)
          enddo
          fl(i) = wen(jw, i)
C--------
210       do g_i_ = 1, g_p_
            g_fv(g_i_, i) = 0.0
          enddo
          fv(i) = 0.
C--------
99994   continue
        goto 900
C
C         VAPOR ONLY, - AT OR BELOW DEW PRESSURE
300     do 99993 i = 1, ncps
          do g_i_ = 1, g_p_
            g_fv(g_i_, i) = g_wen(g_i_, jw, i)
          enddo
          fv(i) = wen(jw, i)
C--------
310       do g_i_ = 1, g_p_
            g_fl(g_i_, i) = 0.0
          enddo
          fl(i) = 0.
C--------
99993   continue
C
900     do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 4) = -g_psi(g_i_)
        enddo
        ptpen(jw, 4) = 1. - psi
C--------
        if (kflag .le. 1) then
          goto 910
        endif
        write (jout, 8910) (rk(i), fv(i), fl(i), (anamc(i, jj), jj = 1, 
     *6), i = 1, ncps)
910     return
C
8010    format ('0** COMPONENT',i4,' SHOWS NEGL. VOLATILITY')
8050    format ('0*** FLASH CALCN, TYPE',i3,', FAILED TO CNVRGE IN 20 IT
     *ER -'/10x,'DEL =',g13.3,', FOR PSI =',g13.6)
8910    format (10x,'K(I)',8x,'VAPOR FLOWS',5x,'LIQ FLOWS',3x,'COMPONENT
     *',/,(5x,3g15.5,3x,6a3))
C
      end
