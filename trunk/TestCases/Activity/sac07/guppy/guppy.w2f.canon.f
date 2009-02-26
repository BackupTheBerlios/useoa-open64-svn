C ***********************************************************
C Fortran file translated from WHIRL Mon Aug 28 14:03:55 2006
C ***********************************************************
C ***********************************************************

      PROGRAM main_
      use w2f__types
      IMPLICIT NONE
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) AMP(1 : 200)
      REAL(w2f__4) AMPMAX
      EXTERNAL bl
      REAL(w2f__4) CDP
      REAL(w2f__4) CF(1 : 200)
      REAL(w2f__4) CFTOT
      REAL(w2f__4) CP
      EXTERNAL cp2v
      REAL(w2f__4) cp2v
      REAL(w2f__4) DSTAR(1 : 200)
      CHARACTER(50) FILNAM
      REAL(w2f__4) HH(1 : 200)
      INTEGER(w2f__i4) I
      INTEGER(w2f__i4) LUNIN
      REAL(w2f__4) MACH
      INTEGER(w2f__i4) MODE
      INTEGER(w2f__i4) NBL
      INTEGER(w2f__i4) NP
      REAL(w2f__4) S(1 : 200)
      REAL(w2f__4) SFIX
      REAL(w2f__4) SSEP
      REAL(w2f__4) THETA(1 : 200)
      REAL(w2f__4) UE(1 : 200)
      REAL(w2f__4) UREN
      CHARACTER(4) t__8
      INTEGER(w2f__i4) t__9
      INTEGER(w2f__i4) t__10
      INTEGER(w2f__i4) t__11
      real(w2f__4) oad_ctmp0
C
C     **** Statements ****
C
      WRITE(*, '(//1X,"*** Strip Boundary Layer Analysis ***"/)')
C     10 WRITE (*,15) 
      WRITE(*, '(/1X,"Enter name of input S,Ue data file (^Z to ' //
     >  'quit)")')
      READ(*, '(A50)', END = 11) FILNAM
      LUNIN = 1
C     OPEN (UNIT=LUNIN, FILE=FILNAM, STATUS='OLD') 
      t__8 = 'OLD '
      OPEN(UNIT = LUNIN, STATUS = t__8(1_w2f__i8 : 3), FILE = FILNAM)
      READ(LUNIN, *) MACH, UREN, SFIX
      READ(LUNIN, *) NP, MODE
      t__9 = NP
      DO I = 1, t__9, 1
        READ(LUNIN, *, END = 10) S(I), UE(I)
8       CONTINUE
      END DO
10    CONTINUE
      NP = I + (-1)
14    CONTINUE
C     45 WRITE (*,48) 
      WRITE(*, '(" Enter Mach, Unit Ren #, AMPmax, S at transiti' //
     >  'on")')
      READ(*, *, END = 11, ERR = 14) MACH, UREN, AMPMAX, SFIX
      t__10 = NP
      DO I = 1, t__10, 1
        CP = UE(I)
        if ( MODE .eq. 0 ) then
          call oad_s_cp2v(UE(I),MACH,oad_ctmp0)
          UE(I) = oad_ctmp0
        ELSE
          UE(I) = ABS(UE(I))
        ENDIF
        WRITE(*, *) I, CP, UE(I)
12      CONTINUE
      END DO
      NBL = NP
      call bl(MACH,UREN,SFIX,AMPMAX,NBL,S,UE,DSTAR,THETA,HH,CF,AMP,CDP,C
     +FTOT,SSEP)

      WRITE(*, '(//" ** B.L. DATA"//        "  MACH =",F12.5,"  ' //
     >  ' UREN   =", G14.7,"  AMPMAX =",F10.5/          "' //
     >  '  SFIX =",F12.5,"   SSEP   =", F12.5/        "  ' //
     >  'CDP  =",F12.6,"   CFTOT  =", F12.6//        5X,"' //
     >  'S    ",5X,"UE   ",5X,"DSTAR",5X,"THETA",        ' //
     >  '5X,"H12  ",5X,"CF   ",5X,"AMP")') MACH, UREN, AMPMAX, SFIX,
     >  SSEP, CDP, CFTOT
      t__11 = NBL
      DO I = 1, t__11, 1
        WRITE(*, '(7F10.6)') S(I), UE(I), DSTAR(I), THETA(I), HH(I), CF
     > (I), AMP(I)
2       CONTINUE
      END DO
11    CONTINUE
      STOP
      
      END PROGRAM

      Function v2cp(V, MACH)
      use w2f__types
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      REAL(w2f__4) V
      REAL(w2f__4) MACH
      REAL(w2f__4) v2cp
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) GAMMA
      SAVE GAMMA
      REAL(w2f__4) GEXP
      REAL(w2f__4) GF
      REAL(w2f__4) GM1
      REAL(w2f__4) MSQ
C
C     **** Initializers ****
C
      DATA GAMMA / 1.3999999762 /
C
C     **** Statements ****
C
      GM1 = GAMMA + (-1.0)
      GF = GM1 * 5.0E-01
      GEXP = GAMMA / GM1
      MSQ = MACH * MACH
      v2cp = ((2.0 / GAMMA) / MSQ) * (((GF * MSQ * (1.0 - V * V) + 1.0) 
     +** GEXP) + (-1.0))

      RETURN
      RETURN
      END FUNCTION

      Function cp2v(CP, MACH)
      use w2f__types
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      REAL(w2f__4) CP
      REAL(w2f__4) MACH
      REAL(w2f__4) cp2v
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) GAMMA
      SAVE GAMMA
      REAL(w2f__4) GEXP
      REAL(w2f__4) GF
      REAL(w2f__4) GM1
      REAL(w2f__4) MSQ
C
C     **** Initializers ****
C
      DATA GAMMA / 1.3999999762 /
C
C     **** Statements ****
C
      GM1 = GAMMA + (-1.0)
      GF = GM1 * 5.0E-01
      GEXP = GAMMA / GM1
      MSQ = MACH * MACH
      cp2v = SQRT(1.0 - CP)
      RETURN
      RETURN
      END FUNCTION

      SUBROUTINE bl(MACH, UREN, STRN, AMPMAX, NP, S, UE, DSTAR, THETA,
     >  H12, CFE, AMPF, CDP, CFTOT, SSEP)
      use w2f__types
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      REAL(w2f__4) MACH
      REAL(w2f__4) UREN
      REAL(w2f__4) STRN
      REAL(w2f__4) AMPMAX
      INTEGER(w2f__i4) NP
      REAL(w2f__4) S(1 : 200)
      REAL(w2f__4) UE(1 : 200)
      REAL(w2f__4) DSTAR(1 : 200)
      REAL(w2f__4) THETA(1 : 200)
      REAL(w2f__4) H12(1 : 200)
      REAL(w2f__4) CFE(1 : 200)
      REAL(w2f__4) AMPF(1 : 200)
      REAL(w2f__4) CDP
      REAL(w2f__4) CFTOT
      REAL(w2f__4) SSEP
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) AMP
      REAL(w2f__4) AMP1
      REAL(w2f__4) CE
      REAL(w2f__4) CE1
      REAL(w2f__4) CEM
      REAL(w2f__4) CF
      REAL(w2f__4) CF0
      REAL(w2f__4) DADX0
      REAL(w2f__4) DADX1
      REAL(w2f__4) DADXM
      REAL(w2f__4) DCE0
      REAL(w2f__4) DCE1
      REAL(w2f__4) DCEDX0
      REAL(w2f__4) DCEDX1
      REAL(w2f__4) DCEDXM
      REAL(w2f__4) DELHB
      REAL(w2f__4) DELHSB
      REAL(w2f__4) DELS
      REAL(w2f__4) DHB0
      REAL(w2f__4) DHB1
      REAL(w2f__4) DHBDX0
      REAL(w2f__4) DHBDX1
      REAL(w2f__4) DHBDXM
      REAL(w2f__4) DHSB0
      REAL(w2f__4) DHSB1
      REAL(w2f__4) DHSDX0
      REAL(w2f__4) DHSDX1
      REAL(w2f__4) DHSDXM
      REAL(w2f__4) DS
      REAL(w2f__4) DTH0
      REAL(w2f__4) DTH1
      REAL(w2f__4) DTHDX0
      REAL(w2f__4) DTHDX1
      REAL(w2f__4) DTHDXM
      REAL(w2f__4) DUDS
      REAL(w2f__4) EPTRN
      REAL(w2f__4) EPTRN1
      REAL(w2f__4) ERRFRC
      SAVE ERRFRC
      REAL(w2f__4) ERRHB
      REAL(w2f__4) ERRHSB
      REAL(w2f__4) ERRTOL
      EXTERNAL flwtrp
      REAL(w2f__4) FRAC
      REAL(w2f__4) FRAC1
      REAL(w2f__4) FRAC2
      REAL(w2f__4) FRAC3
      REAL(w2f__4) FRAC4
      REAL(w2f__4) GAM
      SAVE GAM
      REAL(w2f__4) GEXP1
      REAL(w2f__4) GEXP2
      REAL(w2f__4) GF
      REAL(w2f__4) H
      REAL(w2f__4) H12TE
      REAL(w2f__4) HB
      REAL(w2f__4) HB1
      REAL(w2f__4) HBLIM
      REAL(w2f__4) HBM
      REAL(w2f__4) HBSEP
      SAVE HBSEP
      REAL(w2f__4) HBSTP
      SAVE HBSTP
      REAL(w2f__4) HS
      REAL(w2f__4) HSB
      REAL(w2f__4) HSB1
      REAL(w2f__4) HSBM
      REAL(w2f__4) HSBSEP
      SAVE HSBSEP
      REAL(w2f__4) HSBSTP
      SAVE HSBSTP
      INTEGER(w2f__i4) I
      INTEGER(w2f__i4) ISEP
      INTEGER(w2f__i4) ITURB
      INTEGER(w2f__i4) IWAK
      EXTERNAL lambl
      REAL(w2f__4) M
      REAL(w2f__4) M1
      REAL(w2f__4) M2
      REAL(w2f__4) ME(1 : 200)
      REAL(w2f__4) MINF
      REAL(w2f__4) MINFSQ
      REAL(w2f__4) MMIN
      SAVE MMIN
      REAL(w2f__4) MSQ
      REAL(w2f__4) MUEMUI
      REAL(w2f__4) NE(1 : 200)
      REAL(w2f__4) NU
      REAL(w2f__4) NU1
      REAL(w2f__4) NU2
      REAL(w2f__4) NUENUI
      REAL(w2f__4) Q
      REAL(w2f__4) Q0
      REAL(w2f__4) Q1
      REAL(w2f__4) Q2
      REAL(w2f__4) QE(1 : 200)
      REAL(w2f__4) RHOERI
      REAL(w2f__4) RT
      REAL(w2f__4) RTHETA(1 : 200)
      REAL(w2f__4) RTTRB
      SAVE RTTRB
      REAL(w2f__4) S1
      REAL(w2f__4) S2
      REAL(w2f__4) SBEG
      REAL(w2f__4) SCHORD
      REAL(w2f__4) SEND
      REAL(w2f__4) SINT
      REAL(w2f__4) SS1
      REAL(w2f__4) SSM
      REAL(w2f__4) STP1
      REAL(w2f__4) STRAN
      REAL(w2f__4) T0TINF
      REAL(w2f__4) TETINF
      REAL(w2f__4) TH
      REAL(w2f__4) TH1
      REAL(w2f__4) THM
      EXTERNAL trbbl
      EXTERNAL trbini
      REAL(w2f__4) TRF
      SAVE TRF
      REAL(w2f__4) U
      REAL(w2f__4) U1
      REAL(w2f__4) U2
      REAL(w2f__4) UCHG
      REAL(w2f__4) UMAX
      REAL(w2f__4) USQ
      REAL(w2f__4) XPOTE
      INTEGER(w2f__i4) t__12
      INTEGER(w2f__i4) t__13
C
C     **** Initializers ****
C
      DATA ERRFRC / 2.0000000298E-01 /
      DATA GAM / 1.3999999762 /
      DATA HBSEP / 2.7999999523 /
      DATA HBSTP / 4.9999998882E-03 /
      DATA HSBSEP / 1.5150899887 /
      DATA HSBSTP / 4.9999998882E-03 /
      DATA MMIN / 9.9999997765E-03 /
      DATA RTTRB / 4.0E+01 /
      DATA TRF / 8.7999999523E-01 /
      real oad_ctmp0
      real(w2f__4) oad_ctmp1
      real(w2f__4) oad_ctmp2
      real oad_ctmp3
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(AMPMAX)
C$OPENAD INDEPENDENT(UE)
C$OPENAD INDEPENDENT(MACH)
C$OPENAD INDEPENDENT(UREN)
C$OPENAD DEPENDENT(DSTAR)
C$OPENAD DEPENDENT(CDP)
C
C     **** Statements ****
C
      GF = (GAM + (-1.0)) * 5.0E-01
      GEXP1 = (GAM + (-1.0)) / GAM
      GEXP2 = 1E00 / (GAM + (-1.0))
      MINF = MACH
      if ( MACH .lt. MMIN ) then
        MINF = MMIN
      ENDIF
      MINFSQ = MINF * MINF
      T0TINF = GF * MINFSQ + 1.0
      t__12 = NP
      DO I = 1, t__12, 1
        USQ = UE(I) ** 2
        MSQ = USQ / (GF * (1.0 - USQ) + 1E00 / MINFSQ)
        TETINF = T0TINF / (GF * MSQ + 1.0)
        RHOERI = TETINF ** GEXP2
        MUEMUI = SQRT(TETINF) * (1.5049999952 / ((5.0499999523E-01 / TET
     +INF) + 1.0))

        NUENUI = MUEMUI / RHOERI
        ME(I) = SQRT(MSQ)
        QE(I) = (RHOERI * (UE(I) ** 2))
        NE(I) = (NUENUI / UREN)
6       CONTINUE
      END DO
      ITURB = 0
      ISEP = 0
      SSEP = S(NP)
      CFTOT = 0.0
      AMP = 0.0
      STRAN = STRN
      TH = 0.0
      RT = 0.0
      H = 2.236000061
      CF = 0.0
      EPTRN = 1.0
      t__13 = NP
      DO I = 1, t__13, 1
        S2 = S(I)
        M2 = ME(I)
        U2 = UE(I)
        NU2 = NE(I)
        Q2 = QE(I)
        IF(I .eq. 1) GO TO 8
        if ( U2 .lt. (UMAX * 1.0000000475E-03) ) then
          U2 = UMAX * 1.0000000475E-03
        ENDIF
        DELS = S2 - S1
        IF(DELS .LE. 0.0) GO TO 8
        SBEG = S1
        SEND = S2
        IF(ITURB .ne. 0) GO TO 3
        IF((SBEG .GE. STRAN) .AND.(RT .GT. RTTRB)) GO TO 3
        if ( I .eq. 2 ) then
          DS = DELS * 5.0E-01
          SBEG = DS + SBEG
          call flwtrp(SBEG,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU
     +)

          if ( (((U2 - U1) / U1) .gt. 1.0) .or. (U1 .le. 0.0) ) then
            TH = SQRT((NU * 8.3999998868E-02) / DUDS)
            HSB = 1.6199799776
            THETA(1) = TH
            DSTAR(1) = (H * TH)
          ELSE
            TH = SQRT((DS * NU) / U) * 6.641100049E-01
            HSB = 1.5725799799
            H12(1) = (M * M * 2.8999999166E-01 + (M * M * 1.1299999803E-
     +01 + 1.0) * 2.5969998837)

          ENDIF
        ENDIF
        SINT = SBEG
1       CONTINUE
        DS = SEND - SINT
        IF(DS .LE. 0.0) GO TO 8
        call flwtrp(SINT,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q0,NU)
        call lambl(M,U,DUDS,NU,TH,HSB,HS,HB,H,RT,CF0,DTHDX0,DHSDX0,DADX0
     +)

        DHSB0 = DHSDX0 * DS
        DELHSB = HSBSTP
        if ( HSB .lt. (HSBSEP + HSBSTP * 2.0) ) then
          DELHSB = DELHSB * 1.0000000149E-01
        ENDIF
        if ( DELHSB .lt. ABS(DHSB0) ) then
          DS = DS * ABS(DELHSB / DHSB0)
        ENDIF
2       CONTINUE
        DTH0 = DS * DTHDX0
        DHSB0 = DHSDX0 * DS
        SSM = SINT + DS * 5.0E-01
        THM = TH + DTH0 * 5.0E-01
        HSBM = HSB + DHSB0 * 5.0E-01
        call flwtrp(SSM,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
        call lambl(M,U,DUDS,NU,THM,HSBM,HS,HB,H,RT,CF,DTHDXM,DHSDXM,DADX
     +M)

        DHSB1 = DHSDXM * DS
        DTH1 = DS * DTHDXM
        ERRHSB = DHSB1 - DHSB0
        ERRTOL = DELHSB * ERRFRC
        if ( ERRTOL .lt. ABS(ERRHSB) ) then
          DS = DS * 5.0E-01
          GO TO 2
        ENDIF
        SS1 = DS + SINT
        HSB1 = DHSB1 + HSB
        TH1 = DTH1 + TH
        call flwtrp(SS1,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
        call lambl(M,U,DUDS,NU,TH1,HSB1,HS,HB,H,RT,CF,DTHDX1,DHSDX1,DADX
     +1)

        AMP1 = AMP + DS * 5.0E-01 * (DADX0 + DADX1)
        call oad_s_ALOG(RT,oad_ctmp0)
        EPTRN1 = (HSB1 * 1.8399999619E+01 + (-2.1739999771E+01)) - oad_c
     +tmp0

        FRAC1 = 1.0
        FRAC2 = 1.0
        FRAC3 = 1.0
        FRAC4 = 1.0
        if ( HSB1 .le. HSBSEP ) then
          FRAC1 = (HSBSEP - HSB1) / DHSB1
        ENDIF
        if ( AMPMAX .le. 0.0 ) then
          if ( EPTRN1 .lt. 0.0 ) then
            FRAC2 = EPTRN / (EPTRN - EPTRN1)
          ENDIF
        ELSE
          if ( AMPMAX .le. AMP1 ) then
            FRAC3 = (AMPMAX - AMP) / (AMP1 - AMP)
          ENDIF
        ENDIF
        if ( (RT .gt. RTTRB) .and. ((STRAN - SS1) .lt. 0.0) ) then
          FRAC4 = (STRAN - SINT) / DS
        ENDIF
        FRAC = FRAC1
        if ( FRAC .gt. FRAC2 ) then
          FRAC = FRAC2
        ENDIF
        if ( FRAC .gt. FRAC3 ) then
          FRAC = FRAC3
        ENDIF
        if ( FRAC .gt. FRAC4 ) then
          FRAC = FRAC4
        ENDIF
        DS = DS * FRAC
        TH = TH + DTH1 * FRAC
        HSB = HSB + DHSB1 * FRAC
        AMP = AMP + FRAC * (AMP1 - AMP)
        EPTRN = EPTRN1
        CFTOT = CFTOT + DS * 5.0E-01 * (CF * Q + CF0 * Q0)
        SINT = DS + SINT
        IF(FRAC .GE. 1.0) GO TO 1
        ITURB = -1
        IF(S2 .LE. SINT) GO TO 8
        SBEG = SINT
        SEND = S2
3       CONTINUE
        if ( ITURB .le. 0 ) then
          call flwtrp(SBEG,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU
     +)

          call trbini(M,U,DUDS,NU,TH,HB,CE,H,RT,CF)
          oad_ctmp2 = HB * 1.1000000238
          call oad_s_MAX_r(HBSEP,oad_ctmp2,oad_ctmp1)
          HBLIM = oad_ctmp1
          ITURB = 1
          STRAN = SBEG
          IWAK = 0
          AMP = 0.0
        ENDIF
        SINT = SBEG
4       CONTINUE
        if ( ISEP .ne. 0 ) then
          call flwtrp(SINT,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU
     +)

          HB = HBSEP
          HBLIM = HBSEP
          H = (HB + 1.0) * (M * M * TRF * 2.0000000298E-01 + 1.0) + (-1.
     +0)

          UCHG = U / U2
          if ( UCHG .lt. 1.0 ) then
            UCHG = 1.0
          ENDIF
          TH = TH * ((UCHG) ** ((H + 5.0 - (MINFSQ + M * M)) * 5.0E-01))
          CF = 0.0
        ELSE
          DS = SEND - SINT
          IF(DS .LE. 0.0) GO TO 8
          call flwtrp(SINT,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q0,N
     +U)

          call trbbl(IWAK,M,U,DUDS,NU,TH,HB,CE,H,RT,CF0,DTHDX0,DHBDX0,DC
     +EDX0)

          DHB0 = DHBDX0 * DS
          STP1 = (HB + (-1.2999999523)) * 2.0
          DELHB = STP1
          if ( DELHB .lt. 1.0 ) then
            DELHB = 1.0
          ENDIF
          DELHB = DELHB * HBSTP
          if ( DELHB .lt. ABS(DHB0) ) then
            DS = DS * ABS(DELHB / DHB0)
          ENDIF
5         CONTINUE
          DTH0 = DS * DTHDX0
          DHB0 = DHBDX0 * DS
          DCE0 = DCEDX0 * DS
          SSM = SINT + DS * 5.0E-01
          THM = TH + DTH0 * 5.0E-01
          HBM = HB + DHB0 * 5.0E-01
          CEM = CE + DCE0 * 5.0E-01
          call flwtrp(SSM,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
          call trbbl(IWAK,M,U,DUDS,NU,THM,HBM,CEM,H,RT,CF,DTHDXM,DHBDXM,
     +DCEDXM)

          DTH1 = DS * DTHDXM
          DHB1 = DHBDXM * DS
          DCE1 = DCEDXM * DS
          ERRHB = DHB1 - DHB0
          ERRTOL = DELHB * ERRFRC
          if ( ERRTOL .lt. ABS(ERRHB) ) then
            DS = DS * 5.0E-01
            GO TO 5
          ENDIF
          SS1 = DS + SINT
          TH1 = DTH1 + TH
          HB1 = DHB1 + HB
          CE1 = CE + DCE1
          call flwtrp(SS1,S1,M1,U1,Q1,NU1,S2,M2,U2,Q2,NU2,M,U,DUDS,Q,NU)
          call trbbl(IWAK,M,U,DUDS,NU,TH1,HB1,CE1,H,RT,CF,DTHDX1,DHBDX1,
     +DCEDX1)

          FRAC = 1.0
          if ( HB1 .ge. HBLIM ) then
            ISEP = 1
            FRAC = (HBLIM - HB) / DHB1
            DS = DS * FRAC
            SSEP = DS + SINT
          ENDIF
          TH = TH + DTH1 * FRAC
          HB = HB + DHB1 * FRAC
          CE = CE + DCE1 * FRAC
          CFTOT = CFTOT + DS * 5.0E-01 * (CF * Q + CF0 * Q)
          SINT = DS + SINT
          GO TO 4
        ENDIF
8       CONTINUE
        THETA(I) = TH
        RTHETA(I) = ((TH * U2) / NU2)
        H12(I) = H
        DSTAR(I) = (H * TH)
        CFE(I) = CF
        AMPF(I) = AMP
        S1 = S2
        M1 = M2
        U1 = U2
        NU1 = NU2
        Q1 = Q2
9       CONTINUE
      END DO
      call oad_s_AMIN(2.5,H,oad_ctmp3)
      H12TE = oad_ctmp3
      XPOTE = (H12TE + 5.0 - (MINFSQ + (M2 ** 2))) * 5.0E-01
      CDP = (U2 ** XPOTE) * TH * 2.0
      SCHORD = S(NP) - S(1)
      CDP = CDP / SCHORD
      CFTOT = CFTOT / SCHORD
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE flwtrp(X, X1, M1, UE1, QUE1, NUE1, X2, M2, UE2, QUE2,
     >  NUE2, M, UE, DUEDX, QUE, NUE)
      use w2f__types
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      REAL(w2f__4) X
      REAL(w2f__4) X1
      REAL(w2f__4) M1
      REAL(w2f__4) UE1
      REAL(w2f__4) QUE1
      REAL(w2f__4) NUE1
      REAL(w2f__4) X2
      REAL(w2f__4) M2
      REAL(w2f__4) UE2
      REAL(w2f__4) QUE2
      REAL(w2f__4) NUE2
      REAL(w2f__4) M
      REAL(w2f__4) UE
      REAL(w2f__4) DUEDX
      REAL(w2f__4) QUE
      REAL(w2f__4) NUE
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) DX
      REAL(w2f__4) DX21
C
C     **** Statements ****
C
      DX = X - X1
      DX21 = X2 - X1
      M = M1 + ((DX * (M2 - M1)) / DX21)
      UE = UE1 + ((DX * (UE2 - UE1)) / DX21)
      QUE = QUE1 + ((DX * (QUE2 - QUE1)) / DX21)
      NUE = NUE1 + ((DX * (NUE2 - NUE1)) / DX21)
      DUEDX = (UE2 - UE1) / DX21
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE lambl(M, UE, DUEDX, NUE, THETA, HSK, HS, HK, H, RT, CF
     > , DTHDX, DHSKDX, DADX)
      use w2f__types
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      REAL(w2f__4) M
      REAL(w2f__4) UE
      REAL(w2f__4) DUEDX
      REAL(w2f__4) NUE
      REAL(w2f__4) THETA
      REAL(w2f__4) HSK
      REAL(w2f__4) HS
      REAL(w2f__4) HK
      REAL(w2f__4) H
      REAL(w2f__4) RT
      REAL(w2f__4) CF
      REAL(w2f__4) DTHDX
      REAL(w2f__4) DHSKDX
      REAL(w2f__4) DADX
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) BUH
      REAL(w2f__4) CD
      REAL(w2f__4) DADRT
      REAL(w2f__4) DHSDX
      REAL(w2f__4) DHSKHS
      REAL(w2f__4) DS
      REAL(w2f__4) GRAD
      REAL(w2f__4) GRCRIT
      REAL(w2f__4) HMI
      REAL(w2f__4) HSS
      REAL(w2f__4) MSQ
      REAL(w2f__4) RTLOG
      REAL(w2f__4) SF
      REAL(w2f__4) T1
      REAL(w2f__4) TFSQ
      real oad_ctmp0
C
C     **** Statements ****
C
      MSQ = M * M
      GRAD = (DUEDX * THETA) / UE
      RT = (UE * THETA) / NUE
      HS = (HSK + MSQ * 2.8000000864E-02) / (MSQ * 1.4000000432E-02 + 1.
     +0)

      DHSKHS = (MSQ * 1.4000000432E-02 + 1.0)
      if ( HSK .lt. 1.5150899887 ) then
        HK = 4.0292201042
        SF = 0.0
      ELSE
        if ( HSK .lt. 1.5725799799 ) then
          HK = 4.0292201042 - SQRT(HSK + (-1.5150899887)) * ((HSK ** 2) 
     +* 2.271822052E+02 + 5.8360180664E+02 - HSK * 7.2455914307E+02)

          SF = (HK ** 2) * 3.9154100418E-01 + 2.5125889778 - HK * 1.6860
     +949993 - (HK ** 3) * 3.1729001552E-02

        ELSE
          HK = (HSK ** 2) * 2.571578598E+01 + 7.987084198E+01 - HSK * 8.
     +9582138062E+01

          SF = (HSK ** 2) * 2.2216870785 + 1.3723909855 - HSK * 4.226250
     +1717

        ENDIF
      ENDIF
      H = HK * (MSQ * 1.1299999803E-01 + 1.0) + MSQ * 2.8999999166E-01
      CF = (SF * 2.0) / RT
      HSS = M * ((6.400000304E-02 / (HK + (-8.0000001192E-01))) + 2.5099
     +998713E-01)

      DS = (HSK ** 2) * 3.4188981056 + 7.8539757729 - HSK * 1.0260549545
     +E+01

      CD = DS / RT
      HMI = 1E00 / (HK + (-1.0))
      RTLOG = 0.0
      if ( RT .gt. 0.0 ) then
        call oad_s_ALOG10(RT,oad_ctmp0)
        RTLOG = oad_ctmp0
      ENDIF
      GRCRIT = HMI * 3.2950000763 + TANH(HMI * 2.0E+01 + (-1.2899999619E
     ++01)) * (HMI * 1.4149999619 + (-4.8899999261E-01)) + 4.3999999762E
     +-01

      DADX = 0.0
      if ( GRCRIT .lt. RTLOG ) then
        T1 = HK * 2.4000000954 + (-3.7000000477) + TANH((HK + (-3.099999
     +9046)) * 1.5) * 2.5

        DADRT = SQRT((T1 ** 2) + 2.5E-01) * 9.9999997765E-03
        TFSQ = (HK * 6.5399999619 + (-1.4069999695E+01)) / ((HK ** 2))
        BUH = (((((4.0 - HK) ** 2) * 5.7999998331E-02) / (HK + (-1.0))) 
     ++ (-6.8000003695E-02)) / TFSQ

        DADX = TFSQ * (DADRT / THETA) * 5.0E-01 * (BUH + 1.0)
      ENDIF
      DTHDX = CF * 5.0E-01 - GRAD * (H + 2.0 - MSQ)
      DHSDX = (CD * 2.0 - HS * CF * 5.0E-01 - GRAD * HS * (((HSS * 2.0) 
     +/ HS) + 1.0 - H)) / THETA

      DHSKDX = DHSDX * DHSKHS
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE trbini(M, UE, DUEDX, NUE, THETA, HB, CE, H, RT, CF)
      use w2f__types
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      REAL(w2f__4) M
      REAL(w2f__4) UE
      REAL(w2f__4) DUEDX
      REAL(w2f__4) NUE
      REAL(w2f__4) THETA
      REAL(w2f__4) HB
      REAL(w2f__4) CE
      REAL(w2f__4) H
      REAL(w2f__4) RT
      REAL(w2f__4) CF
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) CEEQ0
      REAL(w2f__4) CEMIN
      SAVE CEMIN
      REAL(w2f__4) CF0
      REAL(w2f__4) DCFDHB
      REAL(w2f__4) DGRDHB
      REAL(w2f__4) DH1DHB
      REAL(w2f__4) DHB
      REAL(w2f__4) DHDHB
      REAL(w2f__4) DQDHB
      REAL(w2f__4) DRATIO
      REAL(w2f__4) FA
      REAL(w2f__4) FB
      REAL(w2f__4) FC
      REAL(w2f__4) FD
      REAL(w2f__4) FR
      REAL(w2f__4) GRAD
      REAL(w2f__4) GRDEQ0
      REAL(w2f__4) H1
      REAL(w2f__4) HB0
      INTEGER(w2f__i4) I
      INTEGER(w2f__i4) ITER
      SAVE ITER
      REAL(w2f__4) MSQ
      REAL(w2f__4) REZ
      REAL(w2f__4) RLX
      REAL(w2f__4) RTMIN
      SAVE RTMIN
      REAL(w2f__4) TOL
      SAVE TOL
      REAL(w2f__4) TRF
      SAVE TRF
      INTEGER(w2f__i4) t__15
C
C     **** Initializers ****
C
      DATA CEMIN / -8.9999996126E-03 /
      DATA ITER / 30 /
      DATA RTMIN / 4.0E+01 /
      DATA TOL / 1.0000000475E-03 /
      DATA TRF / 8.7999999523E-01 /
      real oad_ctmp0
      real(w2f__4) oad_ctmp1
C
C     **** Statements ****
C
      MSQ = M * M
      GRAD = (DUEDX * THETA) / UE
      RT = (UE * THETA) / NUE
      if ( RT .lt. RTMIN ) then
        RT = RTMIN
      ENDIF
      FA = MSQ * 1.0000000149E-01 + 1.0
      FB = MSQ * 2.0000000298E-01 + 1.0
      FC = SQRT(FB)
      FD = MSQ * 3.9999999106E-02 + 1.0
      FR = MSQ * 5.6000001729E-02 + 1.0
      oad_ctmp1 = RT * FR
      call oad_s_ALOG10(oad_ctmp1,oad_ctmp0)
      CF0 = ((1.0130000301E-02 / (oad_ctmp0 + (-1.0199999809))) + (-7.50
     +00000652E-04)) / FC

      HB0 = 1E00 / (1.0 - SQRT(FD * CF0 * 5.0E-01) * 6.5500001907)
      HB = 1.3999999762
      t__15 = ITER
      DO I = 1, t__15, 1
        CF = CF0 * ((8.9999997616E-01 / ((HB / HB0) + (-4.0000000596E-01
     +))) + (-5.0E-01))

        DCFDHB = -((CF0 * 8.9999997616E-01) / (HB0 * (((HB / HB0) + (-4.
     +0000000596E-01)) ** 2)))

        H = (HB + 1.0) * (MSQ * TRF * 2.0000000298E-01 + 1.0) + (-1.0)
        DHDHB = (MSQ * TRF * 2.0000000298E-01 + 1.0)
        H1 = (1.7200000286 / (HB + (-1.0))) + 3.1500000954 - ((HB + (-1.
     +0)) ** 2) * 9.9999997765E-03

        DH1DHB = -((((HB + (-1.0)) ** 3) * 1.9999999553E-02 + 1.72000002
     +86) / ((HB + (-1.0)) ** 2))

        GRDEQ0 = (1.25 / H) * (CF * 5.0E-01 - ((((HB + (-1.0)) / (HB * 6
     +.4320001602)) ** 2) / FD))

        DGRDHB = (2.5 / (H * FD)) * (((((HB + (-1.0)) / (HB * 6.43200016
     +02)) ** 2) / HB) - ((HB + (-1.0)) / ((HB * 6.4320001602) ** 2))) +
     + DCFDHB * (6.25E-01 / H) - DHDHB * (1.25 / (H ** 2)) * (CF * 5.0E-
     +01 - ((((HB + (-1.0)) / (HB * 6.4320001602)) ** 2) / FD))

        CEEQ0 = H1 * (CF * 5.0E-01 - GRDEQ0 * (H + 1.0))
        if ( CEEQ0 .lt. CEMIN ) then
          CEEQ0 = CEMIN
        ENDIF
        REZ = GRAD - GRDEQ0
        DQDHB = -DGRDHB
        DHB = -(REZ / DQDHB)
        DRATIO = DHB / HB
        RLX = 1.0
        if ( DRATIO .lt. (-5.0E-01) ) then
          RLX = (-5.0E-01) / DRATIO
        ENDIF
        if ( DRATIO .gt. 1.0 ) then
          RLX = 1E00 / DRATIO
        ENDIF
        HB = HB + DHB * RLX
        IF(TOL .GT. ABS(DHB)) GO TO 4
1       CONTINUE
      END DO
      WRITE(6, '(//" Iteration for HB, CE at transition failed")')
4     CONTINUE
      CE = CEEQ0
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE trbbl(IWAKE, M, UE, DUEDX, NUE, THETA, HB, CE, H, RT,
     >  CF, DTHDX, DHBDX, DCEDX)
      use w2f__types
      IMPLICIT NONE
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) IWAKE
      REAL(w2f__4) M
      REAL(w2f__4) UE
      REAL(w2f__4) DUEDX
      REAL(w2f__4) NUE
      REAL(w2f__4) THETA
      REAL(w2f__4) HB
      REAL(w2f__4) CE
      REAL(w2f__4) H
      REAL(w2f__4) RT
      REAL(w2f__4) CF
      REAL(w2f__4) DTHDX
      REAL(w2f__4) DHBDX
      REAL(w2f__4) DCEDX
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) C
      REAL(w2f__4) CEEQ
      REAL(w2f__4) CEEQ0
      REAL(w2f__4) CEMIN
      SAVE CEMIN
      REAL(w2f__4) CF0
      REAL(w2f__4) COEF
      REAL(w2f__4) CT
      REAL(w2f__4) CTEQ0
      REAL(w2f__4) DHBDH1
      REAL(w2f__4) F
      REAL(w2f__4) FA
      REAL(w2f__4) FB
      REAL(w2f__4) FC
      REAL(w2f__4) FD
      REAL(w2f__4) FR
      REAL(w2f__4) GRAD
      REAL(w2f__4) GRDEQ
      REAL(w2f__4) GRDEQ0
      REAL(w2f__4) H1
      REAL(w2f__4) HB0
      REAL(w2f__4) MSQ
      REAL(w2f__4) RTMIN
      SAVE RTMIN
      REAL(w2f__4) TRF
      SAVE TRF
C
C     **** Initializers ****
C
      DATA CEMIN / -8.9999996126E-03 /
      DATA RTMIN / 4.0E+01 /
      DATA TRF / 8.7999999523E-01 /
      real(w2f__4) oad_ctmp0
      real oad_ctmp1
      real(w2f__4) oad_ctmp2
C
C     **** Statements ****
C
      COEF = 1.0
      if ( IWAKE .ne. 0 ) then
        COEF = 5.0E-01
      ENDIF
      if ( CE .lt. CEMIN ) then
        CE = CEMIN
      ENDIF
      MSQ = M * M
      GRAD = (DUEDX * THETA) / UE
      RT = (UE * THETA) / NUE
      call oad_s_MAX_r(RT,RTMIN,oad_ctmp0)
      RT = oad_ctmp0
      FA = MSQ * 1.0000000149E-01 + 1.0
      FB = MSQ * 2.0000000298E-01 + 1.0
      FC = SQRT(FB)
      FD = MSQ * 3.9999999106E-02 + 1.0
      FR = MSQ * 5.6000001729E-02 + 1.0
      oad_ctmp2 = RT * FR
      call oad_s_ALOG10(oad_ctmp2,oad_ctmp1)
      CF0 = ((1.0130000301E-02 / (oad_ctmp1 + (-1.0199999809))) + (-7.50
     +00000652E-04)) / FC

      if ( IWAKE .ne. 0 ) then
        CF0 = 0.0
      ENDIF
      HB0 = 1E00 / (1.0 - SQRT(FD * CF0 * 5.0E-01) * 6.5500001907)
      CF = CF0 * ((8.9999997616E-01 / ((HB / HB0) + (-4.0000000596E-01))
     +) + (-5.0E-01))

      H = (HB + 1.0) * (MSQ * TRF * 2.0000000298E-01 + 1.0) + (-1.0)
      H1 = (1.7200000286 / (HB + (-1.0))) + 3.1500000954 - ((HB + (-1.0)
     +) ** 2) * 9.9999997765E-03

      DHBDH1 = -(((HB + (-1.0)) ** 2) / (((HB + (-1.0)) ** 3) * 1.999999
     +9553E-02 + 1.7200000286))

      CT = FA * (CE * 2.4000000209E-02 + (CE ** 2) * 1.2000000477 + CF0 
     +* 3.1999999285E-01)

      F = ((CE ** 2) + CE * 1.9999999553E-02 + ((CF0 * 8.0000001192E-01)
     + / 3.0)) / (CE + 9.9999997765E-03)

      GRDEQ0 = (1.25 / H) * (CF * 5.0E-01 - ((((HB + (-1.0)) / (HB * 6.4
     +320001602)) ** 2) / FD))

      CEEQ0 = H1 * (CF * 5.0E-01 - GRDEQ0 * (H + 1.0))
      if ( CEEQ0 .lt. CEMIN ) then
        CEEQ0 = CEMIN
      ENDIF
      CTEQ0 = FA * (CEEQ0 * 2.4000000209E-02 + (CEEQ0 ** 2) * 1.20000004
     +77 + CF0 * 3.1999999285E-01)

      C = (CTEQ0 / (FA * (COEF ** 2))) - CF0 * 3.1999999285E-01
      CEEQ = SQRT((C / 1.2000000477) + 9.9999997474E-05) + (-9.999999776
     +5E-03)

      if ( CEEQ .lt. CEMIN ) then
        CEEQ = CEMIN
      ENDIF
      GRDEQ = (CF * 5.0E-01 - (CEEQ / H1)) / (H + 1.0)
      DTHDX = CF * 5.0E-01 - GRAD * (H + 2.0 - MSQ)
      DHBDX = (DHBDH1 * (CE - H1 * (CF * 5.0E-01 - GRAD * (H + 1.0)))) /
     + THETA

      DCEDX = (F / THETA) * (GRDEQ + (2.7999999523 / (H + H1)) * (SQRT(C
     +TEQ0) - COEF * SQRT(CT)) - GRAD * (((FB * MSQ * 7.500000298E-02) /
     + FA) + 1.0))

      RETURN
      RETURN
      END SUBROUTINE
