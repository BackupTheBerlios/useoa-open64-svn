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
C
      subroutine g_wtmol(g_p_, jw)
C     ********************
C
C        SPAD SUBROUTINE FOR CALCULATING AVG MOL WT OF A STREAM WITH
C        COMPONENT MOLAL FLOWS WEN(JW,I) AND TOTAL MOLAL FLOW WTEN(JW)
C        (WTEN WILL BE CALCULATED IF ENTRY IS ZERO)
C                                      EES IDENT SP/WTMOL
C        WRITTEN BY R. R. HUGHES
C             LAST REVISION AUG. 1 , 1973
C
        logical mdo
        common /unpt/ jout, kntrl, kflag, ncps, nptp, ncst, nrec, nen, w
     *ten(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
C     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
C     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
        common /qp/ csxx(12, 28), anamc(12, 6)
C            CHECK THE TOTAL FLOW
        integer g_pmax_
        parameter (g_pmax_ = 8)
        integer g_i_, g_p_, g_jout(g_pmax_), g_kntrl(g_pmax_), g_kflag(g
     *_pmax_), g_ncps(g_pmax_), g_nptp(g_pmax_), g_ncst(g_pmax_), g_nrec
     *(g_pmax_), g_nen(g_pmax_)
        integer g_ktln(g_pmax_, 15)
        real r2_b, r3_v, r3_b, g_wten(g_pmax_, 5), g_wm(g_pmax_), g_wen(
     *g_pmax_, 5, 12), g_ptpen(g_pmax_, 5, 6), g_cost(g_pmax_, 5), g_en(
     *g_pmax_, 100)
        common /g_unpt/ g_jout, g_kntrl, g_kflag, g_ncps, g_nptp, g_ncst
     *, g_nrec, g_nen, g_wten, g_wen, g_ptpen, g_cost, g_en, g_ktln
        integer g_ehfid
        save g_wm, /g_unpt/
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'wtmol','g_wtmol.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        if (wten(jw) .le. 1.0e-10) then
          goto 30
        endif
C
        mdo = .false.
        goto 40
30      mdo = .true.
        do g_i_ = 1, g_p_
          g_wten(g_i_, jw) = 0.0
        enddo
        wten(jw) = 0.
C--------
40      do g_i_ = 1, g_p_
          g_wm(g_i_) = 0.0
        enddo
        wm = 0.
C--------
C
        do 99999 i = 1, ncps
C           CALCULATE THE TOTAL FLOW, IF NECESSARY
          if (mdo) then
            do g_i_ = 1, g_p_
              g_wten(g_i_, jw) = g_wten(g_i_, jw) + g_wen(g_i_, jw, i)
            enddo
            wten(jw) = wten(jw) + wen(jw, i)
C--------
          endif
C           SUM THE MOLECULAR WEIGHTS OF EACH COMPONENT
          do g_i_ = 1, g_p_
            g_wm(g_i_) = g_wm(g_i_) + csxx(i, 6) * g_wen(g_i_, jw, i)
          enddo
          wm = wm + wen(jw, i) * csxx(i, 6)
C--------
70        continue
99999   continue
C
        if (wten(jw) .le. 0.) then
          goto 80
        endif
C           CALCULATE THE AVERAGE MOLECULAR WEIGHT OF STREAM JW
        r3_v = wm / wten(jw)
        r2_b = 1.0 / wten(jw)
        r3_b = (-r3_v) / wten(jw)
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 6) = r2_b * g_wm(g_i_) + r3_b * g_wten(g_i_,
     * jw)
        enddo
        ptpen(jw, 6) = r3_v
C--------
75      return
C           ZERO FLOW MEANS ZERO MOLECULAR WEIGHT
80      do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 6) = 0.0
        enddo
        ptpen(jw, 6) = 0.
C--------
        goto 75
C
      end
