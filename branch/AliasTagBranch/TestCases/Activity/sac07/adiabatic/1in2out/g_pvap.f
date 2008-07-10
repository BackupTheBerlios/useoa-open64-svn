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
      subroutine g_pvap(g_p_, i, t, g_t, ldg_t, pvres, g_pvres, ldg_pvre
     *s)
C
C USED TO BE     FUNCTION PVAP(I,T)
C     ******************
C
C         VAPOR PRESSURE (K(I)*P OF COMPONENT I AT TEMPERATURE T (F)
C         WRITTEN BY R.R.HUGHES         EES IDENT SP/PVAP
C              LAST REVISION MAY 9, 1974
C
        common /unpt/ jout, kntrl, kflag, ncp, nptp, ncst, nrec, nen, wt
     *en(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
C     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
C     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCP,KTLN(15)
        common /qp/ csxx(12, 28), anamc(12, 6)
        dimension a(12), b(12), c(12)
        equivalence (csxx(1, 24), a(1)), (csxx(1, 25), b(1)), (csxx(1, 2
     *6), c(1))
C+++++++++++++++++++++++++++++++++++++++++++++++++
        real pvres
        integer g_pmax_
        parameter (g_pmax_ = 8)
        integer g_i_, g_p_, ldg_t, ldg_pvres
        real r1_p, r4_b, r2_v, r3_b, r3_v, g_tc(g_pmax_), g_t(ldg_t), g_
     *pliq(g_pmax_), g_pvres(ldg_pvres)
        integer g_ehfid
        save g_tc, g_pliq
        data plmin /-38./, plmax /38./
C
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'pvap','g_pvap.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        r3_b = 1.0 / 1.8
        do g_i_ = 1, g_p_
          g_tc(g_i_) = r3_b * g_t(g_i_)
        enddo
        tc = (t - 32.) / 1.8
C--------
C              ANTOINE EQUATION
        r2_v = tc + c(i)
        r3_v = b(i) / r2_v
        r4_b = -((-r3_v) / r2_v)
        do g_i_ = 1, g_p_
          g_pliq(g_i_) = r4_b * g_tc(g_i_)
        enddo
        pliq = a(i) - r3_v
C--------
        if (pliq .lt. plmax) then
          goto 15
        endif
        do g_i_ = 1, g_p_
          g_pvres(g_i_) = 0.0
        enddo
        pvres = 1.e10
C--------
        goto 90
15      if (pliq .gt. plmin) then
          goto 10
        endif
        do g_i_ = 1, g_p_
          g_pvres(g_i_) = 0.0
        enddo
        pvres = 0.
C--------
        goto 90
C            CONVERT UNITS AND CALCULATE THE VAPOR PRESSURE
10      r2_v = 10. **  pliq
        if ( 10. .gt. 0.0e0 ) then
           
C          Use  r1_p to store `log(10.)'
           r1_p = log (10.)

           r1_p =  r1_p *  r2_v
        else
C          ( 10. <= 0 )

           
           if ( (10. .eq. 0.0e0) .and. ( pliq .ne. 0.0e0) ) then
              r1_p = 0.0e0
           else
              call ehbfSO (11,10., pliq, r2_v, 0.0, r1_p,
     +g_ehfid,
     +103)
           endif

        endif
        r3_b = 0.019336842 * r1_p
        do g_i_ = 1, g_p_
          g_pvres(g_i_) = r3_b * g_pliq(g_i_)
        enddo
        pvres = 0.019336842 * r2_v
C--------
90      return
      end
