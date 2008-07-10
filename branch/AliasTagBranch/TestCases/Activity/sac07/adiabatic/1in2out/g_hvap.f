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
      subroutine g_hvap(g_p_, i, t, g_t, ldg_t, hvres, g_hvres, ldg_hvre
     *s)
C
C USED TO BE    FUNCTION HVAP(I,T)
C     ******************
C
C         VAPOR ENTHALPY (BTU/LB MOL) OF COMPONENT I, AT TEMPERATURE T (F)
C              BASIS IS LIQUID AT 0 F
C         WRITTEN BY R.R.HUGHES         EES IDENT SP/HVAP
C              LAST REVISION SEPT 7, 1973
C
        common /unpt/ jout, kntrl, kflag, ncp, nptp, ncst, nrec, nen, wt
     *en(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
C     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
C     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCP,KTLN(15)
        common /qp/ csxx(12, 28), anamc(12, 6)
        dimension h(2)
C  subroutine decs
        real hvres, xxxx, hdres
C
        integer g_pmax_
        parameter (g_pmax_ = 8)
        integer g_i_, g_p_, ldg_t, ldg_hvres
        real r6_p, r5_p, r4_p, r3_p, r2_p, r1_p, r18_v, r9_v, r3_v, r5_b
        real r15_v, r6_v, r12_v, g_ta(g_pmax_), g_h(g_pmax_, 2), g_t(ldg
     *_t), g_hvres(ldg_hvres), g_xxxx(g_pmax_), g_hdres(g_pmax_)
        integer g_ehfid
        save g_ta, g_h, g_xxxx, g_hdres
        external g_hdel
        data tz /459.67/
C
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'hvap','g_hvap.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        xxxx = 0.0
        do g_i_ = 1, g_p_
          g_ta(g_i_) = 0.0
        enddo
        ta = tz
C--------
C                 IDEAL GAS ENTHALPY EQUATION
        do 99999 j = 1, 2
          r3_v = ta * ta
          r6_p = 2.0e0 * ta
          r6_v = ta ** ( 3 - 2)
          r6_v =  r6_v * ta
          r5_p =  3 *  r6_v
          r6_v =  r6_v * ta
          r9_v = ta ** ( 4 - 2)
          r9_v =  r9_v * ta
          r4_p =  4 *  r9_v
          r9_v =  r9_v * ta
          r12_v = ta ** ( 5 - 2)
          r12_v =  r12_v * ta
          r3_p =  5 *  r12_v
          r12_v =  r12_v * ta
          r15_v = ta ** ( 6 - 2)
          r15_v =  r15_v * ta
          r2_p =  6 *  r15_v
          r15_v =  r15_v * ta
          r18_v = ta ** ( 7 - 2)
          r18_v =  r18_v * ta
          r1_p =  7 *  r18_v
          r18_v =  r18_v * ta
          r5_b = csxx(i, 15) * r1_p + csxx(i, 14) * r2_p + csxx(i, 13) *
     * r3_p + csxx(i, 12) * r4_p + csxx(i, 11) * r5_p + csxx(i, 10) * r6
     *_p + csxx(i, 9)
          do g_i_ = 1, g_p_
            g_h(g_i_, j) = r5_b * g_ta(g_i_)
          enddo
          h(j) = ta * csxx(i, 9) + r3_v * csxx(i, 10) + r6_v * csxx(i, 1
     *1) + r9_v * csxx(i, 12) + r12_v * csxx(i, 13) + r15_v * csxx(i, 14
     *) + r18_v * csxx(i, 15)
C--------
          do g_i_ = 1, g_p_
            g_ta(g_i_) = g_t(g_i_)
          enddo
          ta = tz + t
C--------
10        continue
99999   continue
C                 COMPUTE THE ENTHALPY OF THE VAPOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++
        call g_hdel(g_p_, i, xxxx, g_xxxx, g_pmax_, hdres, g_hdres, g_pm
     *ax_)
        do g_i_ = 1, g_p_
          g_hvres(g_i_) = g_h(g_i_, 2) + (-g_h(g_i_, 1))
        enddo
        hvres = h(2) - h(1) + hdres
C--------
C
        return
      end
