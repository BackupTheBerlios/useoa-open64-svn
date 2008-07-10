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
      subroutine g_vmgas(g_p_, fv, g_fv, ldg_fv, t, g_t, ldg_t, p, g_p, 
     *ldg_p, gasres, g_gasres, ldg_gasres)
C
C USED TO BE      FUNCTION VMGAS (FV,T,P)
C     ***********************
C
C         CALCULATES MOLAL VOLUME (CU FT/LB MOL) OF STREAM WITH MOLAL
C         FLOWS FV, AS A GAS AT TEMPERATURE T (D FAHR) AND PRESS P (PSIA)
C         USES APPROX EQN FOR CORRESPONDING-STATE COMPRESSIBILITY
C         FACTOR CORRELATION
C
C         WRITTEN BY R.R. HUGHES             EES IDENT SP/VMG
C              LAST REVISION AUGUST 1, 1973
C
C
        common /unpt/ jout, kntrl, kflag, ncp, nptp, ncst, nrec, nen, wt
     *en(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
C     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
C     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCP,KTLN(15)
        common /qp/ csxx(12, 28), anamc(12, 6)
        dimension fv(12)
        equivalence (tc, tr), (sum, pr), (vc, pc), (zc, z)
C++++++++++++++++++++++++++++++++++++++++++++++++++
        real gasres
C
        integer g_pmax_
        parameter (g_pmax_ = 8)
        integer g_i_, g_p_, ldg_t, ldg_fv, ldg_p, ldg_gasres
        real r2_p, r1_p, r1_w, r6_b, r6_v, r5_b, r3_v, r3_b, r2_v, r4_v
        real r2_b, g_sum(g_pmax_), g_tc(g_pmax_), g_vc(g_pmax_), g_zc(g_
     *pmax_), g_ta(g_pmax_), g_t(ldg_t), g_fv(ldg_fv, 12), g_pc(g_pmax_)
     *, g_tr(g_pmax_)
        real g_pr(g_pmax_), g_p(ldg_p), g_r1_w(g_pmax_), g_za(g_pmax_), 
     *g_zb(g_pmax_), g_z(g_pmax_), g_gasres(ldg_gasres)
        equivalence (g_tc, g_tr), (g_sum, g_pr), (g_vc, g_pc), (g_zc, g_
     *z)
        integer g_ehfid
        save g_zb, g_z
        save g_sum, g_tc, g_vc, g_zc, g_ta, g_pc, g_tr, g_pr, g_r1_w, g_
     *za
        intrinsic real
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'vmgas','g_vmgas.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        do g_i_ = 1, g_p_
          g_sum(g_i_) = 0.0
        enddo
        sum = 0.0
C--------
        do g_i_ = 1, g_p_
          g_tc(g_i_) = 0.0
        enddo
        tc = 0.0
C--------
        do g_i_ = 1, g_p_
          g_vc(g_i_) = 0.0
        enddo
        vc = 0.0
C--------
        do g_i_ = 1, g_p_
          g_zc(g_i_) = 0.0
        enddo
        zc = 0.0
C--------
        do g_i_ = 1, g_p_
          g_ta(g_i_) = g_t(g_i_)
        enddo
        ta = t + 459.67
C--------
C            SUM THE CRITICAL PROPERTIES
        do 99999 i = 1, ncp
          do g_i_ = 1, g_p_
            g_sum(g_i_) = g_sum(g_i_) + g_fv(g_i_, i)
          enddo
          sum = sum + fv(i)
C--------
          do g_i_ = 1, g_p_
            g_tc(g_i_) = g_tc(g_i_) + csxx(i, 1) * g_fv(g_i_, i)
          enddo
          tc = tc + fv(i) * csxx(i, 1)
C--------
          r5_b = 1.0 / 62.43 * csxx(i, 22)
          do g_i_ = 1, g_p_
            g_vc(g_i_) = g_vc(g_i_) + r5_b * g_fv(g_i_, i)
          enddo
          vc = vc + fv(i) * csxx(i, 22) / 62.43
C--------
          do g_i_ = 1, g_p_
            g_zc(g_i_) = g_zc(g_i_) + csxx(i, 21) * g_fv(g_i_, i)
          enddo
          zc = zc + fv(i) * csxx(i, 21)
C--------
100       continue
99999   continue
C            WEIGHTED AVERAGE CRITICAL PROPERTIES
        r3_v = tc / sum
        r2_b = 1.0 / sum
        r3_b = (-r3_v) / sum
        do g_i_ = 1, g_p_
          g_tc(g_i_) = r2_b * g_tc(g_i_) + r3_b * g_sum(g_i_)
        enddo
        tc = r3_v
C--------
        r3_v = vc / sum
        r2_b = 1.0 / sum
        r3_b = (-r3_v) / sum
        do g_i_ = 1, g_p_
          g_vc(g_i_) = r2_b * g_vc(g_i_) + r3_b * g_sum(g_i_)
        enddo
        vc = r3_v
C--------
        r3_v = zc / sum
        r2_b = 1.0 / sum
        r3_b = (-r3_v) / sum
        do g_i_ = 1, g_p_
          g_zc(g_i_) = r2_b * g_zc(g_i_) + r3_b * g_sum(g_i_)
        enddo
        zc = r3_v
C--------
C            GENERALIZED CORRELATION
        r2_v = 10.7335 * tc
        r6_v = r2_v * zc / vc
        r2_b = 1.0 / vc
        r3_b = (-r6_v) / vc
        r5_b = r2_b * r2_v
        r6_b = r2_b * zc * 10.7335
        do g_i_ = 1, g_p_
          g_pc(g_i_) = r6_b * g_tc(g_i_) + r5_b * g_zc(g_i_) + r3_b * g_
     *vc(g_i_)
        enddo
        pc = r6_v
C--------
        r3_v = ta / tc
        r2_b = 1.0 / tc
        r3_b = (-r3_v) / tc
        do g_i_ = 1, g_p_
          g_tr(g_i_) = r2_b * g_ta(g_i_) + r3_b * g_tc(g_i_)
        enddo
        tr = r3_v
C--------
        r3_v = p / pc
        r2_b = 1.0 / pc
        r3_b = (-r3_v) / pc
        do g_i_ = 1, g_p_
          g_pr(g_i_) = r2_b * g_p(g_i_) + r3_b * g_pc(g_i_)
        enddo
        pr = r3_v
C--------
        do g_i_ = 1, g_p_
          g_r1_w(g_i_) = (-4.0) * g_tr(g_i_)
        enddo
        r1_w = (-4.0) * tr
        r2_v = 19.38 * pr
        r4_v = exp(r1_w)
        r1_p =  r4_v
        r5_b = (-r2_v) * r1_p
        r6_b = (-r4_v) * 19.38
        do g_i_ = 1, g_p_
          g_za(g_i_) = r6_b * g_pr(g_i_) + r5_b * g_r1_w(g_i_)
        enddo
        za = 1.0 - r2_v * r4_v
C--------
        bz = -4.0928e-3
        r2_v = pr * pr
        r1_p = 2.0e0 * pr
        r5_b = 0.15402 + bz * r1_p
        do g_i_ = 1, g_p_
          g_zb(g_i_) = r5_b * g_pr(g_i_)
        enddo
        zb = bz * r2_v + 0.15402 * pr + bz / real(10)
C--------
        r3_v = max (za, zb)

        if (za .gt.  zb) then
           r1_p = 1.0e0
           r2_p = 0.0e0
        else if (za .lt.  zb) then
           r1_p = 0.0e0
           r2_p = 1.0e0
        else
           call ehbfSO (7,za, zb, r3_v, r1_p, r2_p,
     +g_ehfid,
     +203)
           r2_p = 1.0e0 -  r1_p
        endif
        do g_i_ = 1, g_p_
          g_z(g_i_) = r1_p * g_za(g_i_) + r2_p * g_zb(g_i_)
        enddo
        z = r3_v
C--------
        r2_v = 10.7335 * ta
        r6_v = r2_v * z / p
        r2_b = 1.0 / p
        r3_b = (-r6_v) / p
        r5_b = r2_b * r2_v
        r6_b = r2_b * z * 10.7335
        do g_i_ = 1, g_p_
          g_gasres(g_i_) = r6_b * g_ta(g_i_) + r5_b * g_z(g_i_) + r3_b *
     * g_p(g_i_)
        enddo
        gasres = r6_v
C--------
C
        return
      end
