C                           DISCLAIMER
C
C   This file was generated on 10/26/98 by the version of
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
      subroutine g_aifl(g_p_, kf, pipe, g_pipe, ldg_pipe, pressure, vapo
     *r, g_vapor, ldg_vapor, liquid, g_liquid, ldg_liquid)
C     ********************
C
C         SR CALLED BY AFL AND IFL TO CHECK DATA, COMBINE FEEDS,
C         CALL CORRECT FLASH CALCULATION, AND CALC. EFFLUENT PROPERTIES.
C         KF = 1 FOR ADIABATIC FLASH, KF = 2 FOR ISOTHERMAL FLASH
C         WRITTEN BY R.R. HUGHES        EES IDENT  SP/AIFL
C         MODIFIED BY L. T. BIEGLER, AUGUST, 1991
C
        common /unpt/ jout, kntrl, kflag, ncps, nptp, ncst, nrec, nen, w
     *ten(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
        real pipe(*), pressure(*), vapor(*), liquid(*)
C#include "common.h"
        common /flos/ fv(12), fl(12), fw(12)
        equivalence (pfl, en(2)), (tfl, en(3)), (vzn, en(18))
        equivalence (hin, en(19)), (tin, en(20))
        dimension nt(2, 2)
        real r1
        integer g_pmax_
        parameter (g_pmax_ = 8)
        integer g_i_, g_p_, ldg_pipe, ldg_vapor, ldg_liquid, g_jout(g_pm
     *ax_), g_kntrl(g_pmax_), g_kflag(g_pmax_), g_ncps(g_pmax_), g_nptp(
     *g_pmax_)
        integer g_ncst(g_pmax_), g_nrec(g_pmax_), g_nen(g_pmax_), g_ktln
     *(g_pmax_, 15)
        real r3_v, r3_b, r2_b, g_wen(g_pmax_, 5, 12), g_pipe(ldg_pipe, *
     *), g_wten(g_pmax_, 5), g_hin(g_pmax_), g_ptpen(g_pmax_, 5, 6), g_f
     *v(g_pmax_, 12), g_fl(g_pmax_, 12)
        real g_vapor(ldg_vapor, *), g_liquid(ldg_liquid, *), g_cost(g_pm
     *ax_, 5), g_en(g_pmax_, 100), g_pfl(g_pmax_), g_tfl(g_pmax_), g_vzn
     *(g_pmax_), g_tin(g_pmax_), g_fw(g_pmax_, 12)
        common /g_unpt/ g_jout, g_kntrl, g_kflag, g_ncps, g_nptp, g_ncst
     *, g_nrec, g_nen, g_wten, g_wen, g_ptpen, g_cost, g_en, g_ktln
        common /g_flos/ g_fv, g_fl, g_fw
        equivalence (g_pfl, g_en(1, 2)), (g_tfl, g_en(1, 3)), (g_vzn, g_
     *en(1, 18))
        equivalence (g_hin, g_en(1, 19)), (g_tin, g_en(1, 20))
        integer g_ehfid
        save /g_unpt/, /g_flos/
        external g_dnsty
        external g_tset
        external g_hmx
        external g_wtmol
        data nt /'ADI', 'AB.', 'ISO', 'TH.'/
        data wtol /1.e-5/, htol /1.e-3/
C
C          DETERMINE THE NUMBER OF OUTLET STREAMS
C          THIS DEPENDS ON THE NUMBER OF PHASES
C          INVOLVED IN THE FLASH CALCULATION
C
C      initialization of wten that was before in main program
C      changed to accommodate possibility of multiple pipes -- pdh
C
C these lines copied from below
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'aifl','g_aifl.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        nin = ifix(en(1))
        nout = 2
        num = nin + nout
C
C loop over all inlets (before did only for j==3)
        do j = nout1, num
C
          do ii = 1, ncps
            do g_i_ = 1, g_p_
              g_wen(g_i_, j, ii) = g_pipe(g_i_, ii)
            enddo
            wen(j, ii) = pipe(ii)
C--------
            do g_i_ = 1, g_p_
              g_wten(g_i_, j) = g_wten(g_i_, j) + g_wen(g_i_, j, ii)
            enddo
            wten(j) = wten(j) + wen(j, ii)
C--------
          enddo
C
        enddo
C*****
C
C
        nin = ifix(en(1))
5       nout = 2
25      continue
C         CHECK FEED DATA, AND SUM INLET FLOWS AND ENTHALPY
        num = nin + nout
        do g_i_ = 1, g_p_
          g_wten(g_i_, 1) = 0.0
        enddo
        wten(1) = 0.
C--------
        do g_i_ = 1, g_p_
          g_hin(g_i_) = 0.0
        enddo
        hin = 0.
C--------
        nout1 = nout + 1
C
C
C*****************************************************************
C      initialization of ptpen that previously was in main program
C
        do ii = 1, num
          do g_i_ = 1, g_p_
            g_ptpen(g_i_, ii, 1) = 0.0
          enddo
          ptpen(ii, 1) = pressure(ii)
C--------
        enddo
C
C*****************************************************************
C
C
C
        do 99999 j = nout1, num
          if (ptpen(j, 1) .lt. pfl) then
            write (jout, 8110) j
          endif
          if (wten(j) .le. wtol) then
            call g_wtmol(g_p_, j)
          endif
          if (kprops) 800, 800, 801
800       if (ptpen(j, 3) .le. htol) then
            r1 = -1.
            call g_hmx(g_p_, j, r1)
          endif
801       do g_i_ = 1, g_p_
            g_wten(g_i_, 1) = g_wten(g_i_, 1) + g_wten(g_i_, j)
          enddo
          wten(1) = wten(1) + wten(j)
C--------
          do g_i_ = 1, g_p_
            g_hin(g_i_) = g_hin(g_i_) + wten(j) * g_ptpen(g_i_, j, 3) + 
     *ptpen(j, 3) * g_wten(g_i_, j)
          enddo
          hin = hin + ptpen(j, 3) * wten(j)
C--------
110       continue
99999   continue
        write (jout, 1987) (ptpen(j, 3), j = nout1, num)
1987    format (2x,'aifl:',f10.4)
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, nout1, 1) = 0.0
        enddo
        ptpen(nout1, 1) = pfl
C--------
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, nout1, 2) = 0.0
        enddo
        ptpen(nout1, 2) = tfl
C--------
        if (wten(1) .le. wtol) then
          goto 180
        endif
C
C         COMBINE FEEDS - STORE TOTAL AS POINT NO. (NOUT1)
C
        if (num .le. nout1) then
          goto 130
        endif
        do 99997 j = nout1 + 1, num
          do 99998 i = 1, ncps
120         do g_i_ = 1, g_p_
              g_wen(g_i_, nout1, i) = g_wen(g_i_, nout1, i) + g_wen(g_i_
     *, j, i)
            enddo
            wen(nout1, i) = wen(nout1, i) + wen(j, i)
C--------
99998     continue
125       continue
99997   continue
C
C         CALCULATE FLASH
130     do g_i_ = 1, g_p_
          g_wten(g_i_, nout1) = 0.0
        enddo
        wten(nout1) = 0.
C--------
        call g_wtmol(g_p_, nout1)
        if (abs(wten(nout1) - wten(1)) .gt. wtol) then
          write (jout, 8130)
        endif
        r3_v = hin / wten(nout1)
        r2_b = 1.0 / wten(nout1)
        r3_b = (-r3_v) / wten(nout1)
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, nout1, 3) = r2_b * g_hin(g_i_) + r3_b * g_wten(g
     *_i_, nout1)
        enddo
        ptpen(nout1, 3) = r3_v
C--------
        if (kflag .le. 1) then
          goto 131
        endif
        write (jout, 9130) (nt(k, kf), k = 1, 2), (ptpen(nout1, j), j = 
     *1, 2)
        if (kf .le. 1) then
          write (jout, 9131)
        endif
C*ADIFOR* I/O statement contains active variables
        write (jout, 9132) ptpen(nout1, 3), wten(nout1), (wen(nout1, i),
     * i = 1, ncps)
131     r1 = -1.
        call g_tset(g_p_, nout1, r1)
        goto (135, 132), kf
132     tin = ptpen(nout1, 2)
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, nout1, 2) = 0.0
        enddo
        ptpen(nout1, 2) = tfl
C--------
        r1 = -1.
        call g_hmx(g_p_, nout1, r1)
C
135     vzn = ptpen(nout1, 4)
        do 99996 i = 1, ncps
          do g_i_ = 1, g_p_
            g_wen(g_i_, 1, i) = g_fv(g_i_, i)
          enddo
          wen(1, i) = fv(i)
C--------
          do g_i_ = 1, g_p_
            g_wen(g_i_, 2, i) = g_fl(g_i_, i)
          enddo
          wen(2, i) = fl(i)
C--------
          if (nout .ge. 3) then
            do g_i_ = 1, g_p_
              g_wen(g_i_, 3, i) = 0.0
            enddo
            wen(3, i) = fw(i)
C--------
          endif
140       continue
99996   continue
C         VAPOR AND LIQUID PROPERTIES
        do 99995 j = 1, nout
          do g_i_ = 1, g_p_
            g_ptpen(g_i_, j, 1) = g_ptpen(g_i_, nout1, 1)
          enddo
          ptpen(j, 1) = ptpen(nout1, 1)
C--------
          do g_i_ = 1, g_p_
            g_ptpen(g_i_, j, 2) = g_ptpen(g_i_, nout1, 2)
          enddo
          ptpen(j, 2) = ptpen(nout1, 2)
C--------
          do g_i_ = 1, g_p_
            g_wten(g_i_, j) = 0.0
          enddo
          wten(j) = 0.
C--------
          call g_wtmol(g_p_, j)
          flag = amax1(float(2 - j), 0.)
          call g_hmx(g_p_, j, flag)
          call g_dnsty(g_p_, j, flag)
150       continue
99995   continue
C
        goto 900
C
C
C         ZERO-FLOW RETURN
180     write (jout, 8180) wtol
        do 99992 j = 1, nout
          do g_i_ = 1, g_p_
            g_ptpen(g_i_, j, 1) = g_ptpen(g_i_, nout1, 1)
          enddo
          ptpen(j, 1) = ptpen(nout1, 1)
C--------
          do g_i_ = 1, g_p_
            g_ptpen(g_i_, j, 2) = g_ptpen(g_i_, nout1, 2)
          enddo
          ptpen(j, 2) = ptpen(nout1, 2)
C--------
          do g_i_ = 1, g_p_
            g_wten(g_i_, j) = 0.0
          enddo
          wten(j) = 0.
C--------
          do 99994 i = 1, ncps
182         do g_i_ = 1, g_p_
              g_wen(g_i_, j, i) = 0.0
            enddo
            wen(j, i) = 0.
C--------
99994     continue
          do 99993 i = 3, 6
184         do g_i_ = 1, g_p_
              g_ptpen(g_i_, j, i) = 0.0
            enddo
            ptpen(j, i) = 0.
C--------
99993     continue
186       continue
99992   continue
C
C
900     continue
        do ii = 1, ncps
          do g_i_ = 1, g_p_
            g_vapor(g_i_, ii) = g_wen(g_i_, 1, ii)
          enddo
          vapor(ii) = wen(1, ii)
C--------
          do g_i_ = 1, g_p_
            g_liquid(g_i_, ii) = g_wen(g_i_, 2, ii)
          enddo
          liquid(ii) = wen(2, ii)
C--------
        enddo
        return
C
8110    format ('0**** PRESSURE AT INLET',i3,' LESS THAN FLASH PRESSURE 
     *****')
8130    format ('0**** COMPONENT FLOW TOTAL DIFFERS FROM TOTAL FLOW ***'
     *)
8180    format ('0**** COMBINED FEED FLOW LT',e12.3,' ****')
9130    format (1x,2a3,'FLASH, AT ',f10.4,' PSIA, AND',f10.4,' DF')
9131    format ('+',50x,'(STARTING TRIAL)')
9132    format (2x,'COMBINED FEEDS --'/5x,f12.3,' BTU/LB MOL ',5x,f12.4,
     *' LB MOLS/HR'/5x,'COMPONENT FLOWS -'/(5x,4f12.4))
      end
