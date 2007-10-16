C...STRIP BOUNDARY LAYER ANALYSIS 
C
      LOGICAL PLT
      REAL  MACH
      CHARACTER*50  FILNAM
      DIMENSION S(200), Q(200), UE(200),
     *          DSTAR(200), THETA(200), HH(200), CF(200), AMP(200)
C
C
      WRITE (*,5) 
    5 FORMAT (//1X,'*** Strip Boundary Layer Analysis ***'/)
C
   10 WRITE (*,15) 
      READ (*,20,END=300) FILNAM
C
   15 FORMAT (/1X,'Enter name of input S,Ue data file (^Z to quit)')
   20 FORMAT (A50) 
C 
C...READ IN THE INPUT DATA.
C
      LUNIN = 1
      OPEN (UNIT=LUNIN, FILE=FILNAM, STATUS='OLD') 
      READ (LUNIN,*) MACH, UREN, SFIX
      READ (LUNIN,*) NP, MODE
C
C...READ IN S/Q DEFINITION
      DO 25 I = 1, NP
         READ (LUNIN,*,END=30)  S(I),UE(I)
   25 CONTINUE
   30 NP = I-1
C
C
   45 WRITE (*,48) 
      READ (*,*,END=300,ERR=45) MACH,UREN,AMPMAX,SFIX
   48 FORMAT (' Enter Mach, Unit Ren #, AMPmax, S at transition')
   49 FORMAT (' Enter number of BL points desired')
C
C...CONVERT CP TO UE IF CP IS INPUT  (MODE=1 IS U INPUT, MODE=0 IS CP INPUT)
      DO 40 I = 1, NP
         cp = ue(i)
         IF(MODE.EQ.0) THEN
           UE(I) = CP2V(UE(I),MACH)
          ELSE
           UE(I) = ABS(UE(I))
         ENDIF
         write(*,*) i, cp, ue(i)
   40 CONTINUE
C
      NBL = NP
C
C
C
C...CALCULATE THE BOUNDARY LAYER
C
      CALL BL (MACH, UREN, SFIX, AMPMAX, NBL, S, UE,
     *         DSTAR, THETA, HH, CF, AMP, CDP, CFTOT, SSEP)
C
      WRITE (*, 230)  MACH, UREN, AMPMAX, SFIX, SSEP, CDP, CFTOT
      DO 100 I = 1, NBL
         WRITE (*, 240)  S(I), UE(I), 
     *                        DSTAR(I), THETA(I), HH(I), CF(I), AMP(I)
  100 CONTINUE
C
C
  230 FORMAT (//' ** B.L. DATA'//
     *        '  MACH =',F12.5,'   UREN   =', G14.7,'  AMPMAX =',F10.5/  
     *        '  SFIX =',F12.5,'   SSEP   =', F12.5/
     *        '  CDP  =',F12.6,'   CFTOT  =', F12.6//
     *        5X,'S    ',5X,'UE   ',5X,'DSTAR',5X,'THETA',
     *        5X,'H12  ',5X,'CF   ',5X,'AMP')
  240 FORMAT (7F10.6)
C 
  300 STOP
      END
