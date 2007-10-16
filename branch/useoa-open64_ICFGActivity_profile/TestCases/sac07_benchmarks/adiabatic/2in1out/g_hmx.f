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
C
C
      subroutine g_hmx(g_p_, jw, flag)
C     ************************
C
C         SETS PTPEN(JW,3) = MIXTURE ENTHALPY (BTU/LB MOL) AND
C         PTPEN(JW,4) = MOL FR VAPZD, AT PROCESS POINT JW (IN UNIT LIST)
C         ASSUMES TEMP IN PTPEN(JW,2), FLOWS IN WEN(JW,I) AND --
C           IF FLAG = -1.0 - VAPOR-LIQUID MIX AT PRESSURE IN PTPEN(JW,1)
C              FLAG =  0.0 - SATURATED LIQUID
C              FLAG =  1.0 - VAPOR, AT LOW PRESSURES
C
C         WRITTEN BY R.R. HUGHES                  EES IDENT  SP/HMX
C              LAST REVISION AUGUST 25, 1973
C
        common /qp/ csxx(12, 28), anamc(12, 6)
        common /unpt/ jout, kntrl, kflag, ncps, nptp, ncst, nrec, nen, w
     *ten(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
        common /flos/ fv(12), fl(12), fw(12)
        common /skiper/ kip1, kip2
C     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
C     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
        dimension x(12)
        integer g_pmax_
        parameter (g_pmax_ = 11)
        integer g_i_, g_p_, g_jout(g_pmax_), g_kntrl(g_pmax_), g_kflag(g
     *_pmax_), g_ncps(g_pmax_), g_nptp(g_pmax_), g_ncst(g_pmax_), g_nrec
     *(g_pmax_), g_nen(g_pmax_)
        integer g_ktln(g_pmax_, 15)
        real r3_v, r3_b, r2_b, g_ptpen(g_pmax_, 5, 6), g_t(g_pmax_), g_f
     *l(g_pmax_, 12), g_wen(g_pmax_, 5, 12), g_fv(g_pmax_, 12), g_h(g_pm
     *ax_), g_hv(g_pmax_)
        real g_hvres(g_pmax_), g_hl(g_pmax_), g_hdres(g_pmax_), g_wten(g
     *_pmax_, 5), g_cost(g_pmax_, 5), g_en(g_pmax_, 100), g_fw(g_pmax_, 
     *12)
        common /g_unpt/ g_jout, g_kntrl, g_kflag, g_ncps, g_nptp, g_ncst
     *, g_nrec, g_nen, g_wten, g_wen, g_ptpen, g_cost, g_en, g_ktln
        common /g_flos/ g_fv, g_fl, g_fw
        integer g_ehfid
        save g_t, g_h, g_hv, g_hvres, g_hl, g_hdres, /g_unpt/, /g_flos/
        external g_hdel
        external g_hvap
        external g_iflash
        external g_wtmol
        data wtol /1.e-10/, vzlo /1.e-6/, vzhi /0.999999/
C+@++@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+
        real hdres, hvres
C
C         CHECK FLOWS,- IF ALL = 0, SET ENTH AND VPZN = 0, AND RETURN
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'hmx','g_hmx.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        if (wten(jw) .gt. wtol) then
          goto 10
        endif
        call g_wtmol(g_p_, jw)
        if (wten(jw) .gt. wtol) then
          goto 10
        endif
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 3) = 0.0
        enddo
        ptpen(jw, 3) = 0.
C--------
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 4) = 0.0
        enddo
        ptpen(jw, 4) = 0.
C--------
        goto 900
C
C         SET UP PHASE CONTROL AND FIND VAPORIZATION
10      klag = ifix(flag + 2.1)
        klag = min0(3, max0(1, klag))
        do g_i_ = 1, g_p_
          g_t(g_i_) = g_ptpen(g_i_, jw, 2)
        enddo
        t = ptpen(jw, 2)
C--------
        goto (100, 200, 300), klag
C           VAPOR-LIQUID MIX, DETERMINE THE PROPER OPTION
100     call g_iflash(g_p_, jw)
        if (ptpen(jw, 4) .lt. vzlo) then
          klag = 2
        endif
        if (ptpen(jw, 4) .gt. vzhi) then
          klag = 3
        endif
        goto 400
C            SATURATED LIQUID
200     do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 4) = 0.0
        enddo
        ptpen(jw, 4) = 0.
C--------
        do 99999 i = 1, ncps
          do g_i_ = 1, g_p_
            g_fl(g_i_, i) = g_wen(g_i_, jw, i)
          enddo
          fl(i) = wen(jw, i)
C--------
201       continue
99999   continue
        goto 400
C              VAPOR, AT LOW PRESSURES
300     do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 4) = 0.0
        enddo
        ptpen(jw, 4) = 1.
C--------
        do 99998 i = 1, ncps
          do g_i_ = 1, g_p_
            g_fv(g_i_, i) = g_wen(g_i_, jw, i)
          enddo
          fv(i) = wen(jw, i)
C--------
301       continue
99998   continue
C         ENTHALPY CALCULATION
400     do g_i_ = 1, g_p_
          g_h(g_i_) = 0.0
        enddo
        h = 0.
C--------
        do 99997 i = 1, ncps
C+@+@+@+@+@+@+@+@+@++@@+@+@+@@+@+@+@+@+
          call g_hvap(g_p_, i, t, g_t, g_pmax_, hvres, g_hvres, g_pmax_)
          do g_i_ = 1, g_p_
            g_hv(g_i_) = g_hvres(g_i_)
          enddo
          hv = hvres
C--------
          call g_hdel(g_p_, i, t, g_t, g_pmax_, hdres, g_hdres, g_pmax_)
          do g_i_ = 1, g_p_
            g_hl(g_i_) = g_hv(g_i_) + (-g_hdres(g_i_))
          enddo
          hl = hv - hdres
C--------
          goto (410, 420, 430), klag
410       do g_i_ = 1, g_p_
            g_h(g_i_) = g_h(g_i_) + hl * g_fl(g_i_, i) + fl(i) * g_hl(g_
     *i_) + hv * g_fv(g_i_, i) + fv(i) * g_hv(g_i_)
          enddo
          h = h + fl(i) * hl + fv(i) * hv
C--------
          goto 450
420       do g_i_ = 1, g_p_
            g_h(g_i_) = g_h(g_i_) + wen(jw, i) * g_hl(g_i_) + hl * g_wen
     *(g_i_, jw, i)
          enddo
          h = h + hl * wen(jw, i)
C--------
          goto 450
430       do g_i_ = 1, g_p_
            g_h(g_i_) = g_h(g_i_) + wen(jw, i) * g_hv(g_i_) + hv * g_wen
     *(g_i_, jw, i)
          enddo
          h = h + hv * wen(jw, i)
C--------
450       continue
99997   continue
C
C         STORE RESULT
        r3_v = h / wten(jw)
        r2_b = 1.0 / wten(jw)
        r3_b = (-r3_v) / wten(jw)
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 3) = r2_b * g_h(g_i_) + r3_b * g_wten(g_i_, 
     *jw)
        enddo
        ptpen(jw, 3) = r3_v
C--------
900     return
      end