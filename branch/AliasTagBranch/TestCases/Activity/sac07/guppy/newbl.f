      FUNCTION V2CP(V,MACH)
      REAL  MACH, MSQ
      DATA  GAMMA / 1.4 /
      GM1 = GAMMA - 1.
      GF  = 0.5*GM1
      GEXP = GAMMA / GM1
      MSQ = MACH*MACH
      V2CP = 2./GAMMA/MSQ * ( (1. + GF*MSQ*(1.-V*V))**GEXP - 1.)
      RETURN
      END

      FUNCTION CP2V(CP,MACH)
      REAL MACH, MSQ
      DATA GAMMA / 1.4 /
      GM1 = GAMMA - 1.
      GF = 0.5*GM1
      GEXP = GAMMA / GM1
      MSQ = MACH*MACH
      CP2V = SQRT(1.-CP)
c     CP2V = SQRT( 1. - 1./GF/MSQ * ( (1.+GF*CP)**(1./GEXP) - 1.) )
      RETURN
      END

      SUBROUTINE BL (MACH, UREN, STRN, AMPMAX, NP, S, UE,
     &               DSTAR, THETA, H12, CFE, AMPF, CDP, CFTOT, SSEP)
C
C***********************************************************************
C   Compressible Integral BL Computation
C***********************************************************************
C
C  Lockheed California Co. - Harold Youngren Dept.7232  Dec.1986
C
C     This code computes a compressible boundary layer given an input
C     velocity distribution (U/Uinf) and a free stream Mach number. 
C     The method is valid for incompressible flow (Mach=0.) to roughly 
C     Mach=3.
C
C     A hybrid method is used:
C       Laminar flow   - uses two equation momentum/energy method 
C       Turbulent flow - Green's lag entrainment formulation
C       Transition is computed using an (e**n) method, integrating the
C        disturbance amplification rate. Alternatively, Eppler's H23/RT 
C        correlation is used to calculate transition if AMPMAX is input
C        as 0. 
C
C   Assumptions for method:
C     Adiabatic wall conditions 
C     Prandtl number of unity (no weird wall temperatures)
C     Isentropic flow
C     Viscosity is evaluated by Sutherland's law.
C***********************************************************************
C
      REAL  MACH, MMIN, MINF, MINFSQ, M, M1, M2, MSQ, ME, 
     &      MUEMUI, NUENUI, NU, NU1, NU2, NE
C
      DIMENSION  S(200),      
     &           UE(200),     ME(200),     QE(200),      NE(200)
      DIMENSION  DSTAR(200),  THETA(200),  RTHETA(200), 
     &           H12(200),    CFE(200),    AMPF(200)
C
C
      DATA  GAM   / 1.4   /
      DATA  TRF   / .88   /
      DATA  MMIN  / .01   /
C
      DATA  HSBSTP, HSBSEP / .005, 1.51509 /
      DATA  HBSTP,  HBSEP  / .005, 2.8     /
      DATA  ERRFRC  / 0.2  /
      DATA  RTTRB  / 40. /
C
C
      GF    = 0.5 * (GAM-1.)
      GEXP1  = (GAM-1.) / GAM
      GEXP2  = 1. / (GAM-1.)
C
      MINF = MACH
      IF(MACH.LT.MMIN) MINF = MMIN
      MINFSQ = MINF*MINF
C
C
C
C...Compute stagnation temp. ratio - T0TINF
C
      T0TINF = 1.0 + GF*MINFSQ
C
C...Compute flow parameters at edge of boundary layer.
C    Parameters are referenced to free stream values.
C
      DO 50  I   = 1, NP
        USQ = UE(I)**2
	MSQ = USQ / ( GF*(1.0-USQ) + 1.0/MINFSQ )
C
        TETINF = T0TINF / (1.+GF*MSQ)
        RHOERI = TETINF**GEXP2
        MUEMUI = SQRT(TETINF)*(1.505/(1.+.505/TETINF))
        NUENUI = MUEMUI / RHOERI 
C
        ME(I) = SQRT(MSQ)
        QE(I) = RHOERI*UE(I)**2
        NE(I) = NUENUI / UREN
   50 CONTINUE
C   
C
      ITURB = 0
      ISEP  = 0
      SSEP  = S(NP)
      CFTOT = 0.
      AMP   = 0.
      STRAN = STRN
C
      TH = 0.
      RT = 0.
      H  = 2.236
      CF = 0.
      EPTRN = 1.
C
C
C
C***********************************************************************
C...CALCULATE THE BOUNDARY LAYER AT EACH STATION
C***********************************************************************
C 
      DO 700   I = 1, NP
C
        S2  = S(I)
        M2  = ME(I)
        U2  = UE(I)
        NU2 = NE(I)
        Q2  = QE(I)
C
        IF (I .EQ. 1)  GO TO 600
C
        IF(U2.LT..001*UMAX) U2 = .001*UMAX
        DELS = S2 - S1
        IF (DELS.LE.0.)  GO TO 600
C
C
        SBEG = S1
        SEND = S2
C
        IF (ITURB.NE.0)  GO TO 300
C
C
        IF (SBEG.GE.STRAN .AND. RT.GT.RTTRB)  GO TO 300
C
C***********************************************************************
C...Laminar BL (Two equation - momentum and energy method)
C***********************************************************************
C
C...Initialize BL. First point specially handled, calculate initial 
C    momentum thickness from stagnation value, else use flat plate BL.
C
      IF (I.EQ.2) THEN
        DS = .5*DELS
        SBEG = SBEG + DS
        CALL FLWTRP (SBEG,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
C
        IF (U1.LE.0. .OR. (U2-U1)/U1.GT.1.)  THEN
C...Stagnation
          TH = SQRT(.084*NU/DUDS)
          HSB = 1.61998
          THETA(1) = TH
          DSTAR(1) = TH*H
        ELSE
C...Flat plate 
          TH = .66411*SQRT(DS*NU/U)
          HSB = 1.57258
          H12(1) = 2.597*(1.+.113*M*M) + .290*M*M
        ENDIF
      ENDIF
C
C
C...Integrate the laminar BL from SBEG to SEND
C
      SINT = SBEG
C
  110 DS = SEND - SINT
      IF (DS.LE.0.)    GO TO 600
C
C...Set initial step size using change in shape factor
      CALL FLWTRP (SINT,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q0,NU)
      CALL LAMBL (M,U,DUDS,NU,TH,HSB,HS,HB,H,RT,CF0,
     &            DTHDX0,DHSDX0,DADX0)
      DHSB0 = DHSDX0*DS
C
      DELHSB = HSBSTP
      IF (HSB.LT.HSBSEP+2.*HSBSTP)  DELHSB = 0.1*DELHSB
      IF (ABS(DHSB0).GT.DELHSB)  DS = DS*ABS(DELHSB/DHSB0)
C
  120 DTH0  = DTHDX0*DS
      DHSB0 = DHSDX0*DS
C
      SSM  = SINT + 0.5*DS
      THM  = TH   + 0.5*DTH0
      HSBM = HSB  + 0.5*DHSB0
      CALL FLWTRP (SSM,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
      CALL LAMBL (M,U,DUDS,NU,THM,HSBM,HS,HB,H,RT,CF,
     &            DTHDXM,DHSDXM,DADXM)
      DHSB1 = DHSDXM*DS
      DTH1  = DTHDXM*DS
C
      ERRHSB = DHSB1 - DHSB0
      ERRTOL = ERRFRC*DELHSB
      IF (ABS(ERRHSB).GT.ERRTOL)  THEN
        DS = 0.5*DS
        GO TO 120
      ENDIF
C
      SS1  = SINT + DS
      HSB1 = HSB + DHSB1 
      TH1  = TH  + DTH1
      CALL FLWTRP (SS1,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
      CALL LAMBL (M,U,DUDS,NU,TH1,HSB1,HS,HB,H,RT,CF,
     &            DTHDX1,DHSDX1,DADX1)
C
C...Check for transition
C     triggered by laminar separation, 
C     Amplification ratio, or specified transition
C
      AMP1 = AMP + 0.5*DS*(DADX0 + DADX1)
      EPTRN1 = (18.4*HSB1 - 21.74) - ALOG(RT)
C
C
      FRAC1 = 1.
      FRAC2 = 1.
      FRAC3 = 1.
      FRAC4 = 1.
      IF (HSB1.LE.HSBSEP)  FRAC1 = (HSBSEP-HSB1)/DHSB1
      IF (AMPMAX.LE.0.)  THEN
        IF (EPTRN1.LT.0.)  FRAC2 = EPTRN/(EPTRN-EPTRN1)
      ELSE
        IF (AMP1.GE.AMPMAX)  FRAC3 = (AMPMAX-AMP)/(AMP1-AMP)
      ENDIF
      IF (STRAN-SS1.LT.0. .AND. RT.GT.RTTRB)  FRAC4 = (STRAN-SINT)/DS
C
      FRAC = FRAC1
      IF(FRAC.GT.FRAC2) FRAC = FRAC2
      IF(FRAC.GT.FRAC3) FRAC = FRAC3
      IF(FRAC.GT.FRAC4) FRAC = FRAC4
C
      DS  = FRAC*DS
      TH  = TH  + FRAC*DTH1
      HSB = HSB + FRAC*DHSB1 
      AMP = AMP + FRAC*(AMP1-AMP)
      EPTRN = EPTRN1
      CFTOT = CFTOT + 0.5*DS*(CF*Q + CF0*Q0)
      SINT = SINT + DS
C
      IF (FRAC.GE.1.)  GO TO 110
C
C...Did the laminar BL complete the step from S1 to S2 before transition?
      ITURB = -1
      IF (SINT.GE.S2)  GO TO 600
        SBEG = SINT
        SEND = S2
C***********************************************************************
C
C
C
C
C***********************************************************************
C...Turbulent B.L. - Green's lag entrainment formulation
C***********************************************************************
C
C...If this is the first turbulent BL step set initial values for 
C    turbulent BL before proceeding.
C
  300 IF (ITURB.LE.0)  THEN
        CALL FLWTRP (SBEG,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
        CALL TRBINI (M,U,DUDS,NU,TH,HB,CE,H,RT,CF)
        HBLIM = AMAX1(HB*1.1,HBSEP)
        ITURB = 1
        STRAN = SBEG
        IWAK = 0
        AMP  = 0.
      ENDIF
C
C
C...Integrate the turbulent BL from SBEG to SEND
C
      SINT = SBEG
C
C...Integrate the separated BL using an empirical relation
  410 IF (ISEP.NE.0)  THEN
        CALL FLWTRP (SINT,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
        HB = HBSEP
        HBLIM = HBSEP
        H  = (HB+1.)*(1.+0.2*TRF*M*M) - 1.
        UCHG = U/U2
        IF(UCHG.LT.1.) UCHG = 1.
        TH = TH * (UCHG)**(0.5*(5.+H-(M*M+MINFSQ)))
        CF = 0.
      ELSE
C
C...Integrate the attached BL
      DS = SEND - SINT
      IF (DS.LE.0.) GO TO 600
C
C...Set initial step size using change in shape factor or entrainment
      CALL FLWTRP (SINT,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q0,NU)
      CALL TRBBL (IWAK,M,U,DUDS,NU,TH,HB,CE,
     &            H,RT,CF0,DTHDX0,DHBDX0,DCEDX0)
      DHB0 = DHBDX0*DS
      STP1 = (HB-1.3)/.5
      DELHB = STP1
      if (DELHB .LT. 1.) DELHB = 1.
      DELHB = DELHB*HBSTP
      IF (ABS(DHB0).GT.DELHB)  DS = DS*ABS(DELHB/DHB0)
C
  420 DTH0 = DTHDX0*DS
      DHB0 = DHBDX0*DS
      DCE0 = DCEDX0*DS
C
      SSM = SINT + 0.5*DS
      THM = TH   + 0.5*DTH0
      HBM = HB   + 0.5*DHB0
      CEM = CE   + 0.5*DCE0
      CALL FLWTRP (SSM,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
      CALL TRBBL (IWAK,M,U,DUDS,NU,THM,HBM,CEM,
     &            H,RT,CF,DTHDXM,DHBDXM,DCEDXM)
      DTH1 = DTHDXM*DS
      DHB1 = DHBDXM*DS
      DCE1 = DCEDXM*DS
C
      ERRHB = DHB1 - DHB0
      ERRTOL = ERRFRC*DELHB
      IF (ABS(ERRHB).GT.ERRTOL)  THEN
        DS = 0.5*DS
        GO TO 420
      ENDIF
C
      SS1 = SINT + DS
      TH1 = TH + DTH1
      HB1 = HB + DHB1
      CE1 = CE + DCE1
      CALL FLWTRP (SS1,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
      CALL TRBBL (IWAK,M,U,DUDS,NU,TH1,HB1,CE1,
     &            H,RT,CF,DTHDX1,DHBDX1,DCEDX1)
C
      FRAC = 1.
      IF (HB1.GE.HBLIM) THEN
        ISEP = 1
        FRAC = (HBLIM-HB)/DHB1
        DS = FRAC*DS
        SSEP = SINT + DS
      ENDIF
      TH = TH + FRAC*DTH1
      HB = HB + FRAC*DHB1
      CE = CE + FRAC*DCE1
      CFTOT = CFTOT + 0.5*DS*(CF*Q + CF0*Q)
      SINT = SINT + DS
      GO TO 410
C
      ENDIF
C
C
C*********************************************************************
C...Find displacement thickness and skin friction
C*********************************************************************
C
  600 THETA (I) = TH
      RTHETA(I) = U2*TH/NU2
      H12   (I) = H
      DSTAR (I) = TH*H
      CFE   (I) = CF
      AMPF  (I) = AMP
C
      S1  = S2
      M1  = M2
      U1  = U2
      NU1 = NU2
      Q1  = Q2
C
  700 CONTINUE
C
C
C*********************************************************************
C...Profile drag is computed by applying a compressible version
C    of the Squire/Young formula using values of BL parameters
C    from the trailing edge .
C
      H12TE = AMIN1(2.5,H)
      XPOTE = 0.5 *(H12TE + 5.0 - (M2**2+MINFSQ))
      CDP   = 2.*TH * U2**XPOTE
C
C...Normalize the profile drag (CDP) and total skin friction (CFTOT) 
C    based on total S (chord length?) - note that this is only 
C    approximately the chord for a curved surface (exact for flat plate)
C
      SCHORD = S(NP) - S(1)
      CDP   = CDP   / SCHORD
      CFTOT = CFTOT / SCHORD
C
      RETURN
      END
C
      SUBROUTINE FLWTRP (X,
     &                   X1,M1,UE1,QUE1,NUE1,
     &                   X2,M2,UE2,QUE2,NUE2,
     &                   M, UE, DUEDX, QUE, NUE) 
C
C...Input    X1, M1, UE1, QUE1, NUE1,   !external flow data at 1 (beg)
C            X2, M2, UE2, QUE2, NUE2,   !external flow data at 2 (end)
C            X                          !independent variable
C
C...Output   M, UE, DUEDX, QUE, NUE     !external flow data at X
C
      REAL  M1, M2, M, NUE, NUE1, NUE2
C
C...Linear interpolation of flow conditions.
C
      DX   = X -X1
      DX21 = X2-X1
      M   = M1   + (M2-M1)    *DX/DX21
      UE  = UE1  + (UE2-UE1)  *DX/DX21
      QUE = QUE1 + (QUE2-QUE1)*DX/DX21
      NUE = NUE1 + (NUE2-NUE1)*DX/DX21
      DUEDX = (UE2-UE1)/DX21
C
      RETURN
      END
C
      SUBROUTINE LAMBL (M,UE,DUEDX,NUE,
     &                  THETA,HSK,
     &                  HS,HK,H,RT,CF,DTHDX,DHSKDX,DADX)
C
C...Input    M, UE, NUE, DUEDX     !external flow data
C            THETA, HSK            !BL state variable
C...Output   HS,HK,H,RT,CF         !BL parameters
C            DTHDX,DHSKDX,DADX     !derivatives of variables
C
C...Laminar boundary layer method
C
C...Calculate quantities for derivative evaluation
C
C This system has two first order equations...
C     independent variable is X
C       dependent variables are    THETA    HSK
C              with derivatives  (DTHDX)  (DHSKDX)  
C
C The amplification rate derivative (DADX) is calculated for use
C     as a transition estimate
C
      REAL  M, MSQ, NUE
C
      MSQ = M*M
C
      GRAD = THETA*DUEDX/UE
      RT   = UE*THETA / NUE
C
C---- Compressible correction for energy thickness ( Whitfield )
      HS = (HSK+0.028*MSQ) / (1.+0.014*MSQ) 
      DHSKHS = (1.+0.014*MSQ)
C
C---- Laminar HS/HK correlations  ( from Eppler )
      IF (HSK.LT.1.51509) THEN
       HK = 4.02922
       SF = 0.
      ELSE
       IF (HSK.LT.1.57258) THEN
        HK = 4.02922 - SQRT(HSK-1.51509)*
     &       (583.60182 - 724.55916*HSK + 227.1822*HSK**2)
        SF = 2.512589 - 1.686095*HK + .391541*HK**2 - .031729*HK**3
       ELSE
        HK = 79.870845 - 89.58214*HSK + 25.715786*HSK**2
        SF = 1.372391 -   4.22625*HSK +  2.221687*HSK**2
       ENDIF
      ENDIF
C
C---- Shape factor (compressible - Whitfield) 
      H  = HK*(1.+.113*MSQ) + .290*MSQ
C
C---- Laminar skin friction function  ( Cf )
      CF = 2.*SF / RT
C
C---- density thickness shape parameter  ( H** )
      HSS = M * (0.064/(HK-0.8) + 0.251)
C
C---- Laminar dissipation function (CD)
      DS = 7.853976 - 10.26055*HSK + 3.418898*HSK**2
      CD = DS / RT
C
C...Amplification rate equation (from Drela)
C
      HMI = 1. / (HK-1.)
      RTLOG = 0.
      IF (RT.GT.0.) RTLOG = ALOG10(RT)
      GRCRIT = (1.415*HMI-0.489)*TANH(20.*HMI-12.9) + 3.295*HMI + 0.44
      DADX = 0.
      IF (RTLOG.GT.GRCRIT)  THEN
        T1 = 2.4*HK - 3.7 + 2.5*TANH(1.5*(HK-3.1))
        DADRT = .01*SQRT(T1**2 + .25)
        TFSQ  = (6.54*HK-14.07)/(HK**2)
        BUH   = (0.058*(4.-HK)**2/(HK-1.) - 0.068) / TFSQ
        DADX  = DADRT/THETA * TFSQ*0.5*(BUH+1.)
      ENDIF
C
C
C...Momentum equation
      DTHDX = 0.5*CF - (H+2.-MSQ)*GRAD
C
C...Shape factor equation
      DHSDX = (2.*CD - 0.5*CF*HS - (2.*HSS/HS + 1.-H)*HS*GRAD)/THETA
      DHSKDX = DHSKHS*DHSDX
C
      RETURN
      END
C
      SUBROUTINE TRBINI (M,UE,DUEDX,NUE,
     &                   THETA,HB,CE,H,RT,CF)
C
C...Input    M, UE, DUEDX, NUE          !external flow data 
C            THETA                      !initial momentum thickness
C
C...Output   HB, CE                     !initial state variables
C            H, RT, CF                  !boundary layer parameters
C
C...Calculate initial quantities using equilibrium assumptions
C     a Newton-Raphson iteration is used to determine equilibrium
C     shape factor (HB) and entrainment (CE) corresponding to the 
C     initial conditions (THETA and DUEDX)
C
C     Uses Green's lag entrainment turbulent boundary layer method
C     follows R&M 3791
C
C
      REAL  M, MSQ, NUE
C
      DATA  TRF   / .88  /
      DATA  TOL   / .001 /
      DATA  ITER  / 30   /
      DATA  CEMIN / -.009/
      DATA  RTMIN / 40.  /
C
C
      MSQ = M*M
C
      GRAD = THETA*DUEDX/UE
      RT = UE*THETA / NUE
      IF(RT.LT.RTMIN) RT = RTMIN
C
      FA = 1 + 0.1*MSQ
      FB = 1 + 0.2*MSQ
      FC = SQRT(FB)
      FD = 1 + 0.04*MSQ
      FR = 1 + 0.056*MSQ
C
      CF0 = (0.01013/(ALOG10(FR*RT) - 1.02) - 0.00075) / FC
      HB0 = 1. / (1. - 6.55*SQRT(0.5*CF0*FD))
C
C...Initially assume HB(incompressible equiv. of H)=1.4
C
      HB = 1.4
C
C...Start iterative process to find HB and CEEQ
C
      DO 100 I = 1, ITER
C
      CF = CF0 * (0.9/(HB/HB0-0.4) -0.5)
      DCFDHB = -0.9*CF0 / (HB0*(HB/HB0-0.4)**2)
C
      H  = (HB+1.)*(1.+0.2*TRF*MSQ) - 1.
      DHDHB = (1.+0.2*TRF*MSQ)
      H1 = 3.15 + 1.72/(HB-1.) - 0.01*(HB-1.)**2
      DH1DHB =  - (1.72 + 0.02*(HB-1.)**3) / (HB-1.)**2 
C
      GRDEQ0 = 1.25/H * (0.5*CF - ((HB-1.)/(6.432*HB))**2/FD)
      DGRDHB = .625/H * DCFDHB 
     &         - 1.25/H**2 * (0.5*CF-((HB-1.)/(6.432*HB))**2/FD)*DHDHB
     &         + 2.5/(H*FD) * ( ((HB-1.)/(6.432*HB))**2/HB 
     &                         - (HB-1.)/(6.432*HB)**2 )
      CEEQ0  = H1*(0.5*CF - (H+1)*GRDEQ0)
      IF (CEEQ0 .LT. CEMIN)  CEEQ0 = CEMIN
C
      REZ = GRAD - GRDEQ0
      DQDHB = - DGRDHB
C
      DHB = -REZ/DQDHB
C
      DRATIO = DHB/HB
      RLX = 1.
      IF (DRATIO .LT. -.5)  RLX = -.5/DRATIO
      IF (DRATIO .GT. 1.0)  RLX = 1.0/DRATIO
      HB = HB + RLX*DHB
C
      IF (ABS(DHB) .LT. TOL)  GO TO 200
C
  100 CONTINUE
C
C--Iteration did not converge
      WRITE (6,110)
  110 FORMAT (//' Iteration for HB, CE at transition failed')
C      STOP
C--Iteration did converge
  200 CE = CEEQ0
      RETURN
      END
C
      SUBROUTINE TRBBL (IWAKE,
     &                  M,UE,DUEDX,NUE,
     &                  THETA,HB,CE,H,RT,CF,
     &                  DTHDX,DHBDX,DCEDX)
C
C...Input    M, UE, DUEDX, NUE     !external flow data at 1 (beg)
C            THETA, HB, CE         !BL state variable
C            IWAKE                 !wake flag (set to 0 for normal BL,
C                                  !           set to 1 for wake BL)
C...Output   H, RT, CF             !BL parameters
C            DTHDX,DHBDX,DCEDX     !derivatives of variables
C
C...Green's lag entrainment turbulent boundary layer method
C     follows R&M 3791
C
C
C...Calculate quantities for derivative evaluation
C
C This system has three first order equations...
C     independent variable is X
C       dependent variables are    THETA    HB       CE
C              with derivatives  (DTHDX)  (DHBDX)  (DCEDX)
C
      REAL  M, MSQ, NUE
C
      DATA  TRF   / .88  /
      DATA  CEMIN / -.009/
      DATA  RTMIN / 40.  /
C
C...Special treatment if on a wake 
C
      COEF = 1.
      IF (IWAKE .NE. 0)  COEF = 0.5
C
C...Limit CE to CEMIN to avoid excessive negative entrainment
C
      IF (CE .LT. CEMIN)  CE = CEMIN
C
      MSQ = M*M
C
      GRAD = THETA*DUEDX/UE
      RT = UE*THETA / NUE
      RT = AMAX1(RT,RTMIN)
C
      FA = 1 + 0.1*MSQ
      FB = 1 + 0.2*MSQ
      FC = SQRT(FB)
      FD = 1 + 0.04*MSQ
      FR = 1 + 0.056*MSQ
C
C
      CF0 = (0.01013/(ALOG10(FR*RT) - 1.02) - 0.00075) / FC
C
      IF (IWAKE .NE. 0)  CF0 = 0.
C
      HB0 = 1. / (1. - 6.55*SQRT(0.5*CF0*FD))
      CF = CF0 * (0.9/(HB/HB0-0.4) -0.5)
C
      H  = (HB+1.)*(1.+0.2*TRF*MSQ) - 1.
      H1 = 3.15 + 1.72/(HB-1.) - 0.01*(HB-1.)**2
      DHBDH1 = - (HB-1.)**2 / (1.72 + 0.02*(HB-1.)**3)
C
      CT = FA*(0.024*CE + 1.2*CE**2 + 0.32*CF0)
      F  = (0.02*CE + CE**2 + 0.8*CF0/3.) / (0.01 + CE)
C
      GRDEQ0 = 1.25/H * (0.5*CF - ((HB-1.)/(6.432*HB))**2/FD)
C
      CEEQ0  = H1*(0.5*CF - (H+1)*GRDEQ0)
      IF (CEEQ0 .LT. CEMIN)  CEEQ0 = CEMIN
C
      CTEQ0  = (0.024*CEEQ0 + 1.2*CEEQ0**2 + 0.32*CF0) * FA
      C = CTEQ0 / (FA*COEF**2) - 0.32*CF0
C
      CEEQ = SQRT(C/1.2 + .0001) - 0.01
      IF (CEEQ .LT. CEMIN)  CEEQ = CEMIN
      GRDEQ = (0.5*CF - CEEQ/H1) / (H + 1.)
C
C
C...Momentum equation
      DTHDX = 0.5*CF - (H+2.-MSQ)*GRAD
C
C...Shape factor equation
      DHBDX = DHBDH1*(CE - H1*(0.5*CF - (H+1)*GRAD)) / THETA
C
C...Entrainment equation
      DCEDX = F/THETA * ( 2.8/(H+H1) * (SQRT(CTEQ0)-COEF*SQRT(CT)) 
     &                   + GRDEQ 
     &                   - GRAD*(1.+.075*MSQ*FB/FA) )
C
      RETURN
      END
