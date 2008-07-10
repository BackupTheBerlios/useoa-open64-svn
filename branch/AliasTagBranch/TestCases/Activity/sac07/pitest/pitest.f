 
      PARAMETER (IMAX=20,JMAX=10)
      IMPLICIT REAL (M)
      COMMON /C1/ Q(IMAX,JMAX), R(IMAX,JMAX), M(JMAX),
     &            X(IMAX,JMAX), Y(IMAX,JMAX), H(JMAX),
     &            PI(IMAX,JMAX), RSTOUT, DRMAX, MUCON, MSF, GCON, GAM

C
      DO J=1, JMAX
        CALL PICALC(J)
      END DO
C
      STOP
      END 





      SUBROUTINE PICALC(J)
C
      PARAMETER (IMAX=20,JMAX=10)
      IMPLICIT REAL (M)
      COMMON /C1/ Q(IMAX,JMAX), R(IMAX,JMAX), M(JMAX),
     &            X(IMAX,JMAX), Y(IMAX,JMAX), H(JMAX),
     &            PI(IMAX,JMAX), RSTOUT, DRMAX, MUCON, MSF, GCON, GAM

c$openad INDEPENDENT(x)
c$openad INDEPENDENT(y)
      GM1 = GAM - 1.
C
      JO = J
      JP = JO + 1
C
      Q9 = QINF
      Q0 = QINF
      Q1 = Q(1,JO)
C
C---- set scaling factors for 1st,2nd order dissipation from max d(Rho)
      DRF = 0.5
      IF(ICOUNT.GT.1) DRF = (DRMAX/RSTOUT)**2
C
      MUFMAX = 1.9
C
      MUCRAT = MUFMAX/ABS(MUCON)
      ARG = 20.0*DRF**2
      ARG = MIN(20.0 , ARG)
      EXPARG = EXP(-ARG)
      MUFAC1 = MUCRAT + (1.0 - MUCRAT) * EXPARG
      MUFAC2 = SQRT(EXPARG)
C
      QSTAR = SQRT(2.0*H(JO)/(2.0/GM1 + 1.0))
C
      DO 5 IO = 1, II-1
C
        IM = IO-1
        IP = IO+1
C
C------ set random shorthand junk
        SX2M = X(IP,JO) - X(IO,JO)
        SX2P = X(IP,JP) - X(IO,JP)
        SY2M = Y(IP,JO) - Y(IO,JO)
        SY2P = Y(IP,JP) - Y(IO,JP)
        SX2 = 0.5*( SX2M + SX2P )
        SY2 = 0.5*( SY2M + SY2P )
        S2INV = 1.0 / SQRT(SX2*SX2 + SY2*SY2)
        AX2 = 0.5*( (X(IP,JP)-X(IP,JO)) + (X(IO,JP)-X(IO,JO)) )
        AY2 = 0.5*( (Y(IP,JP)-Y(IP,JO)) + (Y(IO,JP)-Y(IO,JO)) )
        AN2 = (SX2*AY2 - SY2*AX2)*S2INV
C
C------ calculate speed
        R2 = R(IO,JO)
        Q2 = M(JO) / (AN2*R2)
        P2 = GCON * R2 * (H(JO) - 0.5*Q2*Q2)
        MSQ2 = Q2*Q2 / (GM1*(H(JO) - 0.5*Q2*Q2))
C
        Q(IO,JO) = Q2
C
C
C-------- disable 2nd-order dissipation in high gradients
          SNDOT = (SX2*AX2 + SY2*AY2)*S2INV**2
          FQWT = 5.0*(1.0 + ABS(SNDOT))
          IF(Q2.GE.Q1 .AND. Q1.GE.Q0 .AND. Q0.GE.Q9) FQWT = 1.0
          FQ    =  FQWT*(Q2-Q1)/QSTAR
C
C-------- set weighting factor MCF for 2nd order dissipation
          ARG = FQ**2
          ARG = MIN( 20.0 , ARG )
          MCF = MUFAC2*EXP(-ARG)
          IF(MUCON .LT. 0.0) MCF = 0.0
C
          MU = 0.0
          MSQMAX = AMAX1(MSQ1,MSQ2)
          IF(MSQMAX.GT.MSF) 
     &       MU = ABS(MUCON) * (MSQMAX-MSF) / (GAM*MSQMAX)
C
C-------- increase 1st order dissipation after high dRho residuals
C-        to speed up shock motion
          MUB = MUFAC1*MU
          MUC = MCF*MU
C
          QS2 = Q2 - (Q2-Q1)*MUB + (Q1-Q0)*MUC
C
        IF(IO.EQ.1) GO TO 51
C
        AXA = AX1*AY2 - AX2*AY1
        SXSM = (SX1M*SY2M - SY1M*SX2M)
        SXSP = (SX1P*SY2P - SY1P*SX2P)
C
        XS = 0.25*(X(IP,JP)+X(IP,JO) - X(IM,JP)-X(IM,JO))
        YS = 0.25*(Y(IP,JP)+Y(IP,JO) - Y(IM,JP)-Y(IM,JO))
        XN = 0.5*(AX1+AX2)
        YN = 0.5*(AY1+AY2)
        SXN = XS*YN - YS*XN
        SXNINV = 1.0 / SXN
C
        MSQ = 0.5 * (MSQ1 + MSQ2)
        PTMP = 0.25*PCWT
        PCORR = PTMP*(P1+P2)*GAM*MSQ*(SXSM-SXSP)*SXNINV
C
        G1 = (SX1*XN+SY1*YN)*S1INV*SXNINV
        G2 = (SX2*XN+SY2*YN)*S2INV*SXNINV
C
        PIDIF = M(JO)*QS1*G1 - M(JO)*QS2*G2   + PCORR*AXA*SXNINV
        PISUM = P1 + P2 + 2.0*PCORR
        PI(IO,JP) = 0.5*(PISUM + PIDIF)
        PI(IO,JO) = 0.5*(PISUM - PIDIF)
C
C------ set shorthand for next streamtube station
   51   SX1M = SX2M
        SX1P = SX2P
        SY1M = SY2M
        SY1P = SY2P
        SX1 = SX2
        SY1 = SY2
        AX1 = AX2
        AY1 = AY2
        AN1 = AN2
        S1INV = S2INV
C
        QS1 = QS2
        Q0 = Q1
        Q1 = Q2
        R1 = R2
        P1 = P2
        MSQ1 = MSQ2
C
  5   CONTINUE
C
c$openad DEPENDENT(pi)
c$openad DEPENDENT(q)
      RETURN
      END 


