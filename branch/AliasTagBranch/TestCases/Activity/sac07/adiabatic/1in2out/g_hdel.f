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
      subroutine g_hdel(g_p_, i, t, g_t, ldg_t, hdres, g_hdres, ldg_hdre
     *s)
C  USED TO BE FUNCTION HDEL(I,T) 
C     ******************
C
C         HEAT OF VAPORIZATION (BTU/LB MOL) OF COMPONENT I AT TEMPERATURE T (F)
C         WRITTEN BY R.R.HUGHES         EES IDENT SP/HDEL
C              LAST REVISION SEP 27, 1973
C
C
        common /unpt/ jout, kntrl, kflag, ncp, nptp, ncst, nrec, nen, wt
     *en(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
C     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
C     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCP,KTLN(15)
        common /qp/ csxx(12, 28), anamc(12, 6)
        real lamda
        real hdres
        dimension tcrit(12), tboil(12), lamda(12), expn(12)
        equivalence (csxx(1, 1), tcrit(1)), (csxx(1, 8), tboil(1)), (csx
     *x(1, 19), lamda(1)), (csxx(1, 20), expn(1))
C
        integer g_pmax_
        parameter (g_pmax_ = 8)
        integer g_i_, g_p_, ldg_t, ldg_hdres
        real r1_p, r3_b, r2_v, g_tabs(g_pmax_), g_t(ldg_t), g_a(g_pmax_)
     *, g_hdres(ldg_hdres)
        integer g_ehfid
        save g_tabs, g_a
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'hdel','g_hdel.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        tboili = tboil(i) + 459.67
        do g_i_ = 1, g_p_
          g_tabs(g_i_) = g_t(g_i_)
        enddo
        tabs = t + 459.67
C--------
C                   WATSON CORRELATION
        r3_b = -(1.0 / (tcrit(i) - tboili))
        do g_i_ = 1, g_p_
          g_a(g_i_) = r3_b * g_tabs(g_i_)
        enddo
        a = (tcrit(i) - tabs) / (tcrit(i) - tboili)
C--------
        if (a .le. 0) then
          do g_i_ = 1, g_p_
            g_a(g_i_) = 0.0
          enddo
          a = 1.e-20
C--------
        endif

        if ( a .ne. 0.0e0 ) then
           r2_v = a ** ( expn(i) - 2.0e0)
           r2_v =  r2_v * a
           r1_p =  expn(i) *  r2_v
           r2_v =  r2_v * a
        else
C          (a = 0)
           r2_v = a **  expn(i)

           if (  expn(i) .lt. 1.0e0 ) then
              call ehbfSO (10,a, expn(i), r2_v, r1_p, 0.0,
     +g_ehfid,
     +86)
           else if (  expn(i) .lt. 2.0e0 ) then
              r1_p = 0.0e0
              call ehbfSO (10,a, expn(i), r2_v, r1_p, 0.0,
     +g_ehfid,
     +91)
           else
              r1_p = 0.0e0
           endif
        endif
        r3_b = lamda(i) * r1_p
        do g_i_ = 1, g_p_
          g_hdres(g_i_) = r3_b * g_a(g_i_)
        enddo
        hdres = lamda(i) * r2_v
C--------
        return
      end