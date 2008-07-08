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
      subroutine g_vmliq(g_p_, fl, g_fl, ldg_fl, t, g_t, ldg_t, liqres, 
     *g_liqres, ldg_liqres)
C     *********************
C
C         CALCULATES MOLAL VOLUME (CU FT/LB MOL) OF STREAM WITH MOLAL
C              FLOWS, FL, AS SATURATED LIQUID AT T DEG FAHR.
C
C         WRITTEN BY R.R.HUGHES         EES IDENT SP/VML
C              LAST REVISION AUGUST 27, 1973
C
        common /unpt/ jout, kntrl, kflag, ncp, nptp, ncst, nrec, nen, wt
     *en(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
C     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
C     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCP,KTLN(15)
        common /qp/ csxx(12, 28), anamc(12, 6)
        dimension fl(12)
C+++++++++++++++++++++++++++++++++++++++++++++++++
        real liqres, sum
C
        integer g_pmax_
        parameter (g_pmax_ = 8)
        integer g_i_, g_p_, ldg_t, ldg_liqres, ldg_fl
        real r6_b, r2_b, r2_p, r1_p, r5_b, r1_w, r3_b, r5_v, r2_v, r3_v
        real g_ts(g_pmax_), g_t(ldg_t), g_sum(g_pmax_), g_liqres(ldg_liq
     *res), g_r1_w(g_pmax_), g_dli(g_pmax_), g_fl(ldg_fl, 12)
        integer g_ehfid
        save g_ts, g_sum, g_r1_w, g_dli
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'vmliq','g_vmliq.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        r2_b = 1.0 / 1000.
        do g_i_ = 1, g_p_
          g_ts(g_i_) = r2_b * g_t(g_i_)
        enddo
        ts = t / 1000.
C--------
        do g_i_ = 1, g_p_
          g_sum(g_i_) = 0.0
        enddo
        sum = 0.
C--------
        do g_i_ = 1, g_p_
          g_liqres(g_i_) = 0.0
        enddo
        liqres = 0.
C--------
C
        do 99999 i = 1, ncp
C               LIQUID DENSITY CORRELATION
          r3_v = csxx(i, 17) + ts * csxx(i, 18)
          r3_b = r3_v + ts * csxx(i, 18)
          do g_i_ = 1, g_p_
            g_r1_w(g_i_) = r3_b * g_ts(g_i_)
          enddo
          r1_w = csxx(i, 16) + ts * r3_v
          r2_v = max (1.0, r1_w)

          if (1.0 .gt.  r1_w) then
             r1_p = 1.0e0
             r2_p = 0.0e0
          else if (1.0 .lt.  r1_w) then
             r1_p = 0.0e0
             r2_p = 1.0e0
          else
             call ehbfSO (7,1.0, r1_w, r2_v, r1_p, r2_p,
     +g_ehfid,
     +88)
             r2_p = 1.0e0 -  r1_p
          endif
          do g_i_ = 1, g_p_
            g_dli(g_i_) = r2_p * g_r1_w(g_i_)
          enddo
          dli = r2_v
C--------
          r5_v = csxx(i, 6) * fl(i) / dli
          r5_b = (-r5_v) / dli
          r6_b = 1.0 / dli * csxx(i, 6)
          do g_i_ = 1, g_p_
            g_liqres(g_i_) = g_liqres(g_i_) + r6_b * g_fl(g_i_, i) + r5_
     *b * g_dli(g_i_)
          enddo
          liqres = liqres + r5_v
C--------
100       do g_i_ = 1, g_p_
            g_sum(g_i_) = g_sum(g_i_) + g_fl(g_i_, i)
          enddo
          sum = sum + fl(i)
C--------
99999   continue
        r3_v = liqres / sum
        r2_b = 1.0 / sum
        r3_b = (-r3_v) / sum
        do g_i_ = 1, g_p_
          g_liqres(g_i_) = r2_b * g_liqres(g_i_) + r3_b * g_sum(g_i_)
        enddo
        liqres = r3_v
C--------
C
        return
      end