C...STRIP BOUNDARY LAYER ANALYSIS 
C
      INTEGER g$pmax$,g$p$,g$i$,g$j$
      PARAMETER (g$pmax$ = 210)
      LOGICAL PLT
      REAL  MACH
      CHARACTER*50  FILNAM
      DIMENSION S(200), CP(200), Q(200), UE(200),
     *          DSTAR(200), THETA(200), HH(200), CF(200), AMP(200)

      REAL     g$mach(g$pmax$), g$uren(g$pmax$),
     *         g$sfix(g$pmax$), g$ampmax(g$pmax$),
     *         g$ue(g$pmax$,200), g$dstar(g$pmax$,200),
     *         g$hh(g$pmax$,200), g$cdp(g$pmax$), g$cftot(g$pmax$)

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
   46 WRITE (*,49) 
      READ (*,*,END=300,ERR=46) NBL
   48 FORMAT (' Enter Mach, Unit Ren #, AMPmax, S at transition')
   49 FORMAT (' Enter number of BL points desired')
C
C...CONVERT CP TO UE IF CP IS INPUT  (MODE=1 IS U INPUT, MODE=0 IS CP INPUT)
      DO 40 I = 1, NP
         IF(MODE.EQ.0) UE(I) = CP2V(UE(I),MACH)
   40 CONTINUE
C
      NBL = NP
C
C
C
C...CALCULATE THE BOUNDARY LAYER
C
C      CALL BL (MACH, UREN, SFIX, AMPMAX, NBL, S, UE,
C      *         DSTAR, THETA, HH, CF, AMP, CDP, CFTOT, SSEP)

      do 43 g$i$ = 1,200
        do 44 g$j$ = 1,203
          g$ue(g$j$,g$i$) = 0.0
 44     continue
        g$ue(g$i$,g$i$) = 1.0
 43   continue

      do 42 g$i$ = 1,203
         g$mach(g$i$) = 0.0
         g$uren(g$i$) = 0.
         g$ampmax(g$i$) = 0.
 42   continue
      g$mach(201) = 1.
      g$uren(202) = 1.
      g$ampmax(203) = 1.
      g$p$ = 203

      CALL g_BL(g$p$,MACH,g$mach,g$pmax$, UREN, g$uren, g$pmax$,
     *         sfix, AMPMAX, g$ampmax, g$pmax$, NBL, S, UE,
     *         g$ue, g$pmax$, DSTAR, g$dstar, g$pmax$, THETA, HH,
     *         CF, AMP, CDP, g$cdp, g$pmax$, CFTOT, SSEP)

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
