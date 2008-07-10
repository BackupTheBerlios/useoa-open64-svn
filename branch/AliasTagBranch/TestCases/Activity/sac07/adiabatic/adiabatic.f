      PROGRAM FLASH


C***************************************************************
C MAIN CALLING PROGRAM FOR ADIABATIC FLASH EXAMPLE 
C SIX COMPONENT HYDROCARBON (IDEAL) MIXTURE. THIS IS A 
C SMALL PROBLEM THAT FUNCTIONS AS THE BUILDING 
C BLOCK FOR STEADY STATE PROCESS SIMULATION.
C
C WRITTEN BY L.T. BIEGLER, AUGUST 14, 1991
C
C*************************************************************** 
      DIMENSION DATA(40)
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCPS,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
      COMMON/QP/ CSXX(12,28),ANAMC(12,6)
      real pipe(6),vapor(6),liquid(6),pressure(5)

      CALL AIFL(1,pipe,pressure,vapor,liquid)

      END
      SUBROUTINE AIFL (KF,pipe,pressure,vapor,liquid)
C     ********************
C
C         SR CALLED BY AFL AND IFL TO CHECK DATA, COMBINE FEEDS,
C         CALL CORRECT FLASH CALCULATION, AND CALC. EFFLUENT PROPERTIES.
C         KF = 1 FOR ADIABATIC FLASH, KF = 2 FOR ISOTHERMAL FLASH
C         WRITTEN BY R.R. HUGHES        EES IDENT  SP/AIFL
C         MODIFIED BY L. T. BIEGLER, AUGUST, 1991
C
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCPS,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
       real pipe(*), pressure(*), vapor(*), liquid(*)
C#include "common.h"
      COMMON /FLOS/ FV(12),FL(12),FW(12)
C      EQUIVALENCE (PFL,EN(2)), (TFL,EN(3)),(EN(18),EN(18))
C      EQUIVALENCE (HIN,EN(19)),(TIN,EN(20))
      DIMENSION NT(2,2)
      DATA NT/3HADI,3HAB.,3HISO,3HTH./
      DATA WTOL/1.E-5/,HTOL/1.E-3/
C
C          DETERMINE THE NUMBER OF OUTLET STREAMS
C          THIS DEPENDS ON THE NUMBER OF PHASES
C          INVOLVED IN THE FLASH CALCULATION
C
*      initialization of wten that was before in main program
*      changed to accommodate possibility of multiple pipes -- pdh

* these lines copied from below
c$openad INDEPENDENT(pipe)
      nin = ifix(en(1))
      nout = 2
      num = nin + nout

* loop over all inlets (before did only for j==3)
      do j = nout1, num

         DO II = 1, NCPS
            WEN(j,II) = PIPE(II)
            WTEN(j) = WTEN(j) + WEN(j,II)
         ENDDO

      enddo
******


       NIN=IFIX(EN(1))
5      NOUT=2
25     CONTINUE
C         CHECK FEED DATA, AND SUM INLET FLOWS AND ENTHALPY
       NUM=NIN+NOUT
       WTEN(1)=0.
       EN(19)=0.
       NOUT1=NOUT+1


******************************************************************
*      initialization of ptpen that previously was in main program
*
      DO II = 1, NUM
         PTPEN(II,1) = PRESSURE(II)
      ENDDO
*
******************************************************************


C
      DO 110 J = NOUT1,NUM
      IF (PTPEN(J,1).LT.EN(2)) WRITE(JOUT,8110) J
      IF (WTEN(J).LE.WTOL) CALL WTMOL(J)
      IF (KPROPS) 800,800,801
800   IF (PTPEN(J,3).LE.HTOL) CALL HMX (J,-1.)
801   WTEN(1) = WTEN(1) + WTEN(J)
      EN(19) = EN(19) + PTPEN(J,3)*WTEN(J)
110   CONTINUE
      write(jout,1987)(ptpen(j,3),j=nout1,num)
1987  format(2x,'aifl:',F10.4)
        PTPEN(NOUT1,1) = EN(2)
        PTPEN(NOUT1,2) = EN(3)
      IF (WTEN(1).LE.WTOL) GO TO 180
C
C         COMBINE FEEDS - STORE TOTAL AS POINT NO. (NOUT1)
C
      IF (NUM.LE.NOUT1) GO TO 130
      DO 125 J = NOUT1+1,NUM
      DO 120 I = 1,NCPS
  120   WEN(NOUT1,I) = WEN(NOUT1,I) + WEN(J,I)
  125 CONTINUE
C
C         CALCULATE FLASH
  130   WTEN(NOUT1) = 0.
      CALL WTMOL(NOUT1)
      IF ((ABS(WTEN(NOUT1)-WTEN(1))).GT.WTOL) WRITE(JOUT,8130)
      PTPEN(NOUT1,3) = EN(19)/WTEN(NOUT1)
      IF (KFLAG.LE.1) GO TO 131
      WRITE(JOUT,9130) (NT(K,KF),K=1,2),(PTPEN(NOUT1,J),J=1,2)
      IF (KF.LE.1) WRITE(JOUT,9131)
      WRITE(JOUT,9132) PTPEN(NOUT1,3),WTEN(NOUT1),
     1    (WEN(NOUT1,I),I=1,NCPS)
  131 CALL TSET (NOUT1,-1.)
      GO TO (135,132), KF
  132 EN(20) = PTPEN(NOUT1,2)
      PTPEN(NOUT1,2) = EN(3)
      CALL HMX(NOUT1,-1.)
C
  135   EN(18) = PTPEN(NOUT1,4)
      DO 140 I = 1,NCPS
        WEN(1,I) = FV(I)
        WEN(2,I) = FL(I)
	IF(NOUT.GE.3)WEN(3,I)=FW(I)
140	CONTINUE
C         VAPOR AND LIQUID PROPERTIES
      DO 150 J = 1,NOUT
        PTPEN(J,1) = PTPEN(NOUT1,1)
        PTPEN(J,2) = PTPEN(NOUT1,2)
        WTEN(J) = 0.
      CALL WTMOL(J)
        FLAG = AMAX1(FLOAT(2-J),0.)
      CALL HMX(J,FLAG)
      CALL DNSTY(J,FLAG)
  150 CONTINUE
C
      GO TO 900
C
C
C         ZERO-FLOW RETURN
  180 WRITE(JOUT,8180) WTOL
      DO 186 J = 1,NOUT
        PTPEN(J,1) = PTPEN(NOUT1,1)
        PTPEN(J,2) = PTPEN(NOUT1,2)
        WTEN(J) = 0.
      DO 182 I = 1,NCPS
  182   WEN(J,I) = 0.
      DO 184 I = 3,6
  184   PTPEN(J,I) = 0.
  186 CONTINUE
C

  900 continue
      do II = 1,NCPS
         vapor(II) = WEN(1,II)
         liquid(II) = WEN(2,II)
      enddo
c$openad DEPENDENT(vapor)
c$openad DEPENDENT(liquid)
      RETURN
C
 8110 FORMAT (23H0**** PRESSURE AT INLET, I3,30H LESS THAN FLASH PRESSUR
     1E **** )
 8130 FORMAT (54H0**** COMPONENT FLOW TOTAL DIFFERS FROM TOTAL FLOW ***)
 8180 FORMAT (27H0**** COMBINED FEED FLOW LT , E12.3,5H **** )
 9130 FORMAT (1X,2A3,10HFLASH, AT ,F10.4,10H PSIA, AND ,F10.4,3H DF )
 9131 FORMAT (1H+,50X,16H(STARTING TRIAL) )
 9132 FORMAT (2X,17HCOMBINED FEEDS -- /5X,F12.3,12H BTU/LB MOL ,5X,
     1  F12.4,11H LB MOLS/HR / 5X,17HCOMPONENT FLOWS - / (5X,4F12.4))
      END

      SUBROUTINE DNSTY(JW,FLAG)
C     *************************
C
C         SETS PTPEN(JW,5) = STREAM DENSITY (LB/CU FT) FOR PROCESS
C         POINT JW (IN UNIT LIST). ASSUMES TEMP IN PTPEN(JW,2), FLOWS
C         IN WEN(JW,I) AND --
C           IF FLAG = -1.0 - VAPOR-LIQUID MIX AT PRESSURE IN PTPEN(JW,1)
C              FLAG =  0.0 - SATURATED LIQUID
C              FLAG =  1.0 - VAPOR, AT PRESSURE IN PTPEN(JW,1)
C
C         WRITTEN BY R.R. HUGHES             EES IDENT  SP/DNS
C              LAST REVISION APRIL 26, 1974
C
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCPS,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
      COMMON /FLOS/ FV(12),FL(12),FW(12)
      COMMON/QP/ CSXX(12,28),ANAMC(12,6)
      DATA WTOL/1.E-10/,VTOL/1.E-6/
C+@+@+@+@@@@@@@+@+@+@+@+@@+@+@@+@@@@@@@@@@@@@@@@@@@@@@@@
	REAL GASRES,LIQRES
C
C         CHECK FLOWS,- IF ALL ARE ZERO, SET DENSITY AT ZERO AND RETURN
      IF (WTEN(JW).GT.WTOL.AND.PTPEN(JW,6).GT.WTOL) GO TO 10
      CALL WTMOL(JW)
      IF (WTEN(JW).GT.WTOL) GO TO 10
        PTPEN(JW,5) = 0.
      GO TO 900
C
C         SET UP PHASE CONTROL
   10   KLAG = IFIX(FLAG+2.1)
        KLAG = MIN0(3,MAX0(1,KLAG))
C              DETERMINE THE STATE OF THE STREAM
20    GO TO (100,200,300), KLAG
C
C         GENERAL CASE:  VAPOR + LIQUID
100   CALL IFLASH(JW)
      IF (PTPEN(JW,4).LT.VTOL) GO TO 200
      IF (PTPEN(JW,4).GT.(1.-VTOL)) GO TO 300
C+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@
      CALL VMGAS(FV,PTPEN(JW,2),PTPEN(JW,1),GASRES)
      CALL VMLIQ(FL,PTPEN(JW,2),LIQRES)
      VM = GASRES * PTPEN(JW,4) +
     1       LIQRES * (1.-PTPEN(JW,4))
       PTPEN(JW,5) = PTPEN(JW,6)/VM
      GO TO 900
C
C         ALL LIQUID
200    DO 260 I = 1,NCPS
  260   FL(I) = WEN(JW,I)
C+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@@+@@+@+@+@
       CALL VMLIQ(FL,PTPEN(JW,2),LIQRES)
       PTPEN(JW,5) = PTPEN(JW,6)/ LIQRES               
      GO TO 900
C
C         ALL VAPOR
300     DO 360 I = 1,NCPS
  360   FV(I) = WEN(JW,I)
C+@+@+@+@+@+@++@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@
	CALL VMGAS(FV,PTPEN(JW,2),PTPEN(JW,1),GASRES)
	 PTPEN(JW,5) = PTPEN(JW,6)/ GASRES                            
C
  900 RETURN
      END

      SUBROUTINE HDEL(I,T,hdres)
C  USED TO BE FUNCTION HDEL(I,T) 
C     ******************
C
C         HEAT OF VAPORIZATION (BTU/LB MOL) OF COMPONENT I AT TEMPERATURE T (F)
C         WRITTEN BY R.R.HUGHES         EES IDENT SP/HDEL
C              LAST REVISION SEP 27, 1973
C
c
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCP,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCP,KTLN(15)
      COMMON /QP/ CSXX(12,28),ANAMC(12,6)
      REAL LAMDA
      real hdres
      DIMENSION TCRIT(12),TBOIL(12),LAMDA(12),EXPN(12)
C      EQUIVALENCE (CSXX(1,1),TCRIT(1)),(CSXX(1,8),TBOIL(1)),(CSXX(1,19),
C     1LAMDA(1)),(CSXX(1,20),EXPN(1))
C
      TBOILI = CSXX(I,8)+459.67
      TABS=T+459.67
C                   WATSON CORRELATION
      A=(CSXX(I,1)-TABS)/(CSXX(I,1)-TBOILI)
      IF(A.LE.0)A=1.E-20
      hdres = CSXX(I,19)*A**CSXX(I,20)
      RETURN
      END
C
C
      SUBROUTINE HMX (JW,FLAG)
C     ************************
C
C         SETS PTPEN(JW,3) = MIXTURE ENTHALPY (BTU/LB MOL) AND
C         PTPEN(JW,4) = MOL FR VAPZD, AT PROCESS POINT JW (IN UNIT LIST)
C         ASSUMES TEMP IN PTPEN(JW,2), FLOWS IN WEN(JW,I) AND --
C           IF FLAG = -1.0 - VAPOR-LIQUID MIX AT PRESSURE IN PTPEN(JW,1)
C              FLAG =  0.0 - SATURATED LIQUID
C              FLAG =  1.0 - VAPOR, AT LOW PRESSURES
C
C         WRITTEN BY R.R. HUGHES                  EES IDENT  SP/HMX
C              LAST REVISION AUGUST 25, 1973
C
      COMMON/QP/ CSXX(12,28),ANAMC(12,6)
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCPS,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
      COMMON /FLOS/ FV(12),FL(12),FW(12)
      COMMON/SKIPER/KIP1,KIP2
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
      DIMENSION X(12)
      DATA WTOL/1.E-10/,VZLO/1.E-6/,VZHI/0.999999/
C+@++@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+
      REAL HDRES,HVRES
C
C         CHECK FLOWS,- IF ALL = 0, SET ENTH AND VPZN = 0, AND RETURN
      IF (WTEN(JW).GT.WTOL) GO TO 10
      CALL WTMOL(JW)
      IF (WTEN(JW).GT.WTOL)  GO TO 10
        PTPEN(JW,3) = 0.
        PTPEN(JW,4) = 0.
      GO TO 900
C
C         SET UP PHASE CONTROL AND FIND VAPORIZATION
   10   KLAG = IFIX (FLAG + 2.1)
        KLAG = MIN0(3,MAX0(1,KLAG))
        T = PTPEN(JW,2)
      GO TO (100,200,300), KLAG
C           VAPOR-LIQUID MIX, DETERMINE THE PROPER OPTION
 100  CALL IFLASH(JW)
      IF (PTPEN(JW,4).LT.VZLO) KLAG = 2
      IF (PTPEN(JW,4).GT.VZHI) KLAG = 3
       GO TO 400
C            SATURATED LIQUID
  200   PTPEN(JW,4) = 0.
        DO 201 I=1,NCPS
        FL(I)=WEN(JW,I)
201	CONTINUE
	GO TO 400
C              VAPOR, AT LOW PRESSURES
  300   PTPEN(JW,4) = 1.
	do 301 i=1,ncps
	fv(i)=wen(jw,i)
301	continue
C         ENTHALPY CALCULATION
  400   H = 0.
      DO 450 I = 1,NCPS
C+@+@+@+@+@+@+@+@+@++@@+@+@+@@+@+@+@+@+
       CALL HVAP(I,T,HVRES)
	HV = HVRES      
        CALL HDEL(I,T,HDRES)
	HL = HV - HDRES      
        GO TO (410,420,430), KLAG
  410   H = H + FL(I)*HL + FV(I)*HV
      GO TO 450
  420   H = H + HL*WEN(JW,I)
      GO TO 450
  430   H = H + HV*WEN(JW,I)
  450 CONTINUE
C
C         STORE RESULT
        PTPEN(JW,3) = H/WTEN(JW)
  900 RETURN
      END
          SUBROUTINE HVAP(I,T,HVRES)
C
C USED TO BE    FUNCTION HVAP(I,T)
C     ******************

C         VAPOR ENTHALPY (BTU/LB MOL) OF COMPONENT I, AT TEMPERATURE T (F)
C              BASIS IS LIQUID AT 0 F
C         WRITTEN BY R.R.HUGHES         EES IDENT SP/HVAP
C              LAST REVISION SEPT 7, 1973
C
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCP,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCP,KTLN(15)
      COMMON /QP/ CSXX(12,28),ANAMC(12,6)
      DIMENSION H(2)
c  subroutine decs
      REAL HVRES,XXXX,hdres
c
      DATA TZ/459.67/
C
      XXXX=0.0
      TA = TZ
C                 IDEAL GAS ENTHALPY EQUATION
      DO 10 J = 1,2
         H(J)=TA*CSXX(I,9)+TA**2*CSXX(I,10)+TA**3*CSXX(I,11)+
     1   TA**4*CSXX(I,12)+TA**5*CSXX(I,13)+TA**6*CSXX(I,14)+
     2   TA**7*CSXX(I,15)
         TA=TZ+T
10    CONTINUE
C                 COMPUTE THE ENTHALPY OF THE VAPOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++
      call hdel(I,XXXX,hdres)
      HVRES = H(2)-H(1) + hdres                
C
      RETURN
      END
      SUBROUTINE IFLASH(JW)
C     *********************
C
C         ISOTHERMAL FLASH OF STREAM JW AT PRESSURE AND TEMP PRESET IN
C         PTPEN(JW,...). USES PVAP TO CALCULATE EQUILIBRIUM CONSTANTS
C         RETURNS MOL FRACTION VAPOR IN PTPEN(JW,4), COMPONENT VAPOR
C         FLOWS IN FV(I) AND COMPONENT LIQUID FLOWS IN FL(I).
C
C         WRITTEN BY R.R. HUGHES             EES IDENT  SP/IFLASH
C              LAST REVISION MAY 9, 1974
C
      REAL NUMER,PVRES
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCPS,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
      COMMON /FLOS/ FV(12),FL(12),FW(12)
      COMMON /QP/ CSXX(12,28),ANAMC(12,6)
      DIMENSION  RK(12)
      DATA RKMIN/1.E-15/
C
      DO 10 I = 1,NCPS
C@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+
       CALL PVAP(I,PTPEN(JW,2),PVRES)
       RK(I) = PVRES / PTPEN(JW,1)
      IF(RK(I).GE.RKMIN) GO TO 10
      WRITE(JOUT,8010) I
        RK(I)=RKMIN
  10  CONTINUE
C
C              FIRST TRY METHOD FOR LOW VAPORIZATION
        MCAL = 1
        PSI = 1.
C
C         START BASIC ALGORITHMS
   20 ASSIGN 35 TO MDO
C
      DO 50 K = 1,20
        TEST = -WTEN (JW)
        DERIV = 0.
C
      DO 30 I = 1,NCPS
C
      GO TO (24,26), MCAL
   24   NUMER = (1.-1./RK(I))
        DENOM = 1. - PSI * NUMER
      GO TO 28
   26   NUMER = RK(I) - 1.
        DENOM = RK(I) - PSI * NUMER
C
   28   TEST = TEST + WEN(JW,I)/DENOM
   30   DERIV = DERIV + WEN(JW,I)*NUMER/DENOM**2
C
      GO TO MDO , (35,40)
   35 IF (TEST.LE.0.) GO TO (200,300), MCAL
C              (SINGLE PHASE, - OUTSIDE BUBBLE PRESSURE/DEW PRES. RANGE)
      ASSIGN 40 TO MDO
   40   DEL = TEST/DERIV
        PSI = PSI - DEL
      IF (ABS(DEL).LE.1.E-6) GO TO 100
C
C         FOR LARGE VAPORIZATION, SWITCH TO ALTERNATIVE CALCULATION
      IF (MCAL.EQ.1.AND.PSI.LT.0.4) GO TO 60
   50 CONTINUE
C
      WRITE(JOUT,8050) MCAL,DEL,PSI
      GO TO 100
C
C         SECOND METHOD USES DIFFERENT TEST FUNCTION
   60   MCAL = 2
        PSI = 0.
      GO TO 20
C
C         PERCENT VAPORIZED ESTABLISHED - CALCULATION DEPENDS ON DEGREE
C         OF VAPORIZATION
  100 GO TO (110,130), MCAL
  110 IF (PSI.GT.0.999999) GO TO 200
        R = PSI/(1.-PSI)
      DO 120 I = 1,NCPS
        FV(I) = WEN(JW,I)/(1.+R/RK(I))
  120   FL(I) = WEN(JW,I) - FV(I)
      GO TO 900
C
  130 IF (PSI.LT.1.E-6)GO TO 300
        R =(1.-PSI)/PSI
      DO 140 I = 1,NCPS
        FL(I) = WEN(JW,I)/(1.+R*RK(I))
  140   FV(I) = WEN(JW,I) - FL(I)
      GO TO 900
C
C         LIQUID ONLY, - AT OR ABOVE THE BUBBLE PRESSURE
  200 DO 210 I = 1,NCPS
        FL(I) = WEN(JW,I)
  210   FV(I) = 0.
      GO TO 900
C
C         VAPOR ONLY, - AT OR BELOW DEW PRESSURE
  300 DO 310 I = 1,NCPS
        FV(I) = WEN(JW,I)
  310   FL(I) = 0.
C
  900   PTPEN(JW,4) = 1.-PSI
      IF (KFLAG.LE.1) GO TO 910
      WRITE(JOUT,8910) (RK(I),FV(I),FL(I),
     1 (ANAMC(I,JJ),JJ=1,6),I=1,NCPS)
  910 RETURN
C
 8010 FORMAT(13H0** COMPONENT ,I4,23H SHOWS NEGL. VOLATILITY )
 8050 FORMAT (22H0*** FLASH CALCN, TYPE, I3,31H, FAILED TO CNVRGE IN 20
     1ITER -/ 10X,5HDEL =, G13.3,11H, FOR PSI =,G13.6 )
 8910 FORMAT (10X,4HK(I),8X,11HVAPOR FLOWS,5X,9HLIQ FLOWS,
     1 3X,'COMPONENT',/,(5X,3G15.5,3X,6A3 ))
C
      END
      SUBROUTINE PVAP(I,T,PVRES)
C
C USED TO BE     FUNCTION PVAP(I,T)
C     ******************
C
C         VAPOR PRESSURE (K(I)*P OF COMPONENT I AT TEMPERATURE T (F)
C         WRITTEN BY R.R.HUGHES         EES IDENT SP/PVAP
C              LAST REVISION MAY 9, 1974
C
       COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCP,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCP,KTLN(15)
      COMMON /QP/ CSXX(12,28),ANAMC(12,6)
      DIMENSION A(12),B(12),C(12)
C      EQUIVALENCE (CSXX(1,24),A(1)),(CSXX(1,25),B(1)),(CSXX(1,26),C(1))
C+++++++++++++++++++++++++++++++++++++++++++++++++
      REAL PVRES
      DATA PLMIN/-38./,PLMAX/38./
C
        TC = (T-32.)/1.8
C              ANTOINE EQUATION
      PLIQ=CSXX(I,24)-CSXX(I,25)/(TC+CSXX(I,26))
      IF(PLIQ.LT.PLMAX)GO TO 15
      PVRES=1.E10
      GO TO 90
15    IF(PLIQ.GT.PLMIN) GO TO 10
        PVRES=0.
      GO TO 90
C            CONVERT UNITS AND CALCULATE THE VAPOR PRESSURE
  10    PVRES= 0.019336842*10.**PLIQ
  90  RETURN
      END
      SUBROUTINE TBTD (KS,JW,P,TB,TD,HB,HD)
C     *******************************
C
C         CALCULATES BUBBLE AND DEW-POINT TEMPERATURES (TB AND TD) FOR
C         STREAM ASSOCIATED WITH POINT JW, AT PRESSURE P, USING VALUES
C         PRESET IN TB AND TD AS INITIAL ESTIMATES.  KS INDICATES OPTION
C              KS = 1  -  BUBBLE-POINT ONLY
C              KS = 2  -  DEW-POINT ONLY
C              KS = 3  -  BOTH BUBBLE- AND DEW-POINT
C         IF NON-CONDENSABLES OR NON-VOLATILES ARE FOUND
C         TB OR TD CALCULATED ON THE BASIS OF EQLM. VAP. FR.
C         OR EQLM. LIQ. FR. DEFINED AS VFAC*(TOTAL MOLES OF
C         NON-CONDENSABLES OR NON-VOLATILES AS THE CASE MAY BE)
C
C         WRITTEN BY R.R. HUGHES & R.K. MALIK     EES IDENT SP/TBTD
C              LAST REVISION SEPTEMBER 9,1974
C
C+@+@+@+@+@+@+@@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@@+
      REAL PVRES
      COMMON/QP/ CSXX(12,28),ANAMC(12,6)
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCPS,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
      DIMENSION NMOPT(2),KJ(12),ZF(12),AKE(12)
      DATA VFAC/0.1/, LIM2/50/
      DATA NMOPT/3HBBL,3HDEW/
      DATA  TZ/-459.67/,PTOL/1.E-10/,WTOL/1.E-3/,TLIM/5000./
      DATA EPS/1.E-3/, LIM/50/, FR/0.02/, DL/-30./, DH/30./
C
C         CHECK DATA, INITALIZE, AND SET GATES
2     IF (P.LE.PTOL.OR.WTEN(JW).LE.WTOL) GO TO 500
      IF(TB.LT.TZ.OR.TB.GT.TLIM)TB=0.
      IF(TD.LT.TZ.OR.TD.GT.TLIM)TD=0.
C
C         CHECK IF NON CONDENSABLES ARE PRESENT
C
        AKBARL = 0.
      DO 5 I = 1,NCPS
C+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+
      CALL PVAP(I,TB,PVRES)
      AKE(I) = PVRES/P
    5 AKBARL = AKBARL + WEN(JW,I)/WTEN(JW)*LOG(AKE(I)+1.E-10)/LOG(10.0)
        AKBAR = 10.**AKBARL
      IF (KS.EQ.2) GO TO 7
      DO 6 I= 1,NCPS
        RATIOK = AKE(I)/AKBAR
      IF (RATIOK.GE.100..AND.WEN(JW,I).GT.0.) GO TO 351
    6 CONTINUE
C
    9   T = TB
        D = P * WTEN(JW)
        MS = 1
      GO TO 20
C
C         CHECK IF NON-VOLATILES ARE PRESENT
C
    7 DO 8 I= 1,NCPS
        RATIOK = AKE(I)/AKBAR
      IF (RATIOK.LE.0.01.AND.WEN(JW,I).GT.0.) GO TO 400
    8 CONTINUE
C
C
   10   T = TD
        D = WTEN(JW)/P
        MS = 2
C
   20 IF (T.LT.TZ.OR.T.GT.TLIM) T=0.
      ASSIGN 110 TO MB
C
C         BEGIN MAJOR ITERATION LOOP FOR TEMPERATURE ADJUSTMENT
      DO 200 J = 1,LIM
        DEL = D
C
C         BEGIN MINOR ITERATION LOOP FOR COMPONENT SUMMATION
      DO 100 I = 1,NCPS
        W = WEN(JW,I)
      IF (W.LE.WTOL) GO TO 100
C+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@@+@+@+@+@+@+@+@+@+@+@+@+
       CALL PVAP(I,T,PVRES)
       PV = PVRES      
      GO TO (70,80), MS
C
   70   DEL = DEL - W*PV
      GO TO 100
C
   80 IF (PV.LE.PTOL) GO TO 150
        DEL = DEL - W/PV
  100 CONTINUE
C
C         CHECK FOR COMPLETION
        DEL = DEL/D
      IF (ABS(DEL).LE.EPS) GO TO 300
C
C         SET UP NEXT ITERATION
      GO TO MB, (110,120)
C
C              FIRST TIME - ARBITRARY DT
  110 ASSIGN 120 TO MB
        DT = SIGN((FR*(T-TZ)),DEL)
      IF (MS.EQ.2) DT = -DT
      GO TO 130
C
C              LATER TRIES - USE NEWTONS METHOD
 120    DELP=(DELP-DEL)/DEL
      IF(ABS(DELP).GT.EPS) GO TO 128
      GO TO 110
 128  DT=DT/DELP
C
  130   DT = AMAX1(DL,MIN(DH,DT))
        DELP = DEL
      GO TO 200
C
C         FOR LOW VOLATILITY IN DEW POINT CALCULATION, RAISE TEMPERATURE
C         ARBITRARILY
  150   DT = DH
      ASSIGN 110 TO MB
C
  200   T = T + DT
C
C
C         PRINT ERROR FLAG IF DO-LOOP HAS NORMAL EXIT
210     NMP = NMOPT(MS)
       WRITE(JOUT,9200) NMP,LIM,P,TB,TD,DT,DEL,DELP,EPS,WTEN(JW),
     1  (WEN(JW,K),K = 1,NCPS)
C
C         ONE CALCULATION COMPLETE, - STORE RESULT AND CHECK SPECS FOR 2ND
  300 GO TO (310,320), MS
C
  310   TB = T
      IF (KS.EQ.3) GO TO 7
      GO TO 330
  320   TD = T
330   IF(TB.LT.TZ.OR.TB.GT.TLIM)GO TO 335
      IF(TD.GT.TZ.AND.TD.LT.TLIM)GO TO 340
335   WRITE(JOUT,9355) TB,TD
      STOP
  340 IF (KFLAG.LE.1) GO TO 350
      WRITE(JOUT,9330) P
      IF (KS.NE.2) WRITE(JOUT,9331) TB
      IF (KS.GT.1) WRITE(JOUT,9332) TD
  350 RETURN
C
C         CALCULATION OF BUBBLEPOINT - NONCONDENSABLES CASE
351    CONTINUE
        J = 0
        SUMZ = 0.
      DO 360 I = 1,NCPS
        RATIOK = AKE(I)/AKBAR
      IF (RATIOK.LT.100.) GO TO 360
        J = J+1
        ZF(J) = WEN(JW,I)/WTEN(JW)
        KJ(J) = I
        SUMZ = SUMZ + ZF(J)
  360 CONTINUE
        VF = (1.+VFAC)*SUMZ
      VF=AMAX1(0.001,VF)
      WRITE(JOUT,9350) VF
C         USE NEWTON'S METHOD
        T = TB
        I1 = 1
      DO 370 I = 1,LIM2
        SUMD = 0.
      DO 365 K = 1,NCPS
      DO 366 KK = 1,J
  366 IF (K.EQ.KJ(KK)) GO TO 365
C+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@+@@+@+@+@+@+@++@+@+@+@+@
       CALL PVAP(K,T,PVRES)
       PVK=PVRES/P
      SUMD=SUMD+(WEN(JW,K)/WTEN(JW))*(PVK-1.)/(VF*PVK+1.-VF)
  365 CONTINUE
        FUNXON = SUMD + 1./(1.+VFAC)
      IF(ABS(FUNXON).LT.EPS)GO TO 310
      IF(I.GT.1)GO TO 368
      DFUNC=0.
      GO TO 369
368   DFUNC=DT/(FUNXON-FUNCS)
369   DT=-FUNXON*SIGN(AMAX1(1.E-2,ABS(DFUNC)),DFUNC)
      DT=AMAX1(DL,MIN(DH,DT))
      IF(ABS(DT).LE.EPS)GO TO 310
      FUNCS=FUNXON
371   IF((T+DT).GT.TZ.AND.(T+DT).LT.TLIM)GO TO 370
      DT=DT/2.
      GO TO 371
370   T=T+DT
      MS=1
      GO TO 210
C
C         CALCULATION OF DEW POINT NON-VOLATILES CASE
C
  400 IF (KS.EQ.1) GO TO 9
        J = 0
        SUMZ = 0.
      DO 410 I = 1,NCPS
      RATIOK = AKE(I)/AKBAR
      IF (RATIOK.GT..01) GO TO 410
        J = J + 1
        ZF(J) = WEN(JW,I)/WTEN(JW)
        KJ(J) = I
        SUMZ = SUMZ + ZF(J)
  410 CONTINUE
        ALF = (1.+VFAC)*SUMZ
      ALF=AMAX1(0.001,ALF)
        VF = 1. - ALF
      WRITE(JOUT,9400) ALF
C         USE NEWTON'S METHOD
        T = TD
        I1 = 1
      DO 420 I = 1,LIM2
        SUMD = 0.
      DO 418 K = 1,NCPS
      DO 419 KK = 1,J
  419 IF (K.EQ.KJ(KK)) GO TO 418
C+@+@+@+@+@++@+@+@+@@@@@@@@@++++++++++++++++++++++++
	CALL PVAP(K,T,PVRES)
      PVK=PVRES/P
      SUMD=SUMD+WEN(JW,K)/WTEN(JW)*(PVK-1.)/(VF*PVK+1.-VF)
  418 CONTINUE
        FUNXON = SUMD + 1./(1.+VFAC)
      IF(ABS(FUNXON).LT.EPS)GO TO 320
      IF(I.GT.1)GO TO 412
      DFUNC=0.
      GO TO 413
412   DFUNC=DT/(FUNXON-FUNCS)
413   DT=-FUNXON*SIGN(AMAX1(1.E-2,ABS(DFUNC)),DFUNC)
      DT=AMAX1(DL,MIN(DH,DT))
      IF(ABS(DT).LT.EPS)GO TO 320
      FUNCS=FUNXON
414   IF((T+DT).GT.TZ.AND.(T+DT).LT.TLIM)GO TO 420
      DT=DT/2.
      GO TO 414
420   T=T+DT
      MS=2
      GO TO 210
C
  500 WRITE(JOUT,9500) KS,JW, P,TB,TD,WTEN(JW)
      RETURN
C
C
 9200 FORMAT ( 6H0**** ,A3,34H-POINT CALCN HAS NOT CONVERGED IN ,I3,
     1 13H ITERATIONS -/6X,22HDATA AND RESULTS ARE - / 6X,
     2  3HP = , F10.4, 6H, TB =, F10.4, 6H, TD =, F10.4, 6H, DT =,F10.4/
     3  6X,5HDEL =,F12.8,14H, DEL(PREV.) =,F12.8, 7H, EPS =, F12.8 /
     4  6X,29HFLOWS (LB MOLS/HR) - TOTAL = ,F12.3/ (3X,6F11.3) )
C
 9330 FORMAT (3H AT, F10.5,6H PSIA, )
 9331 FORMAT (1H+,20X,9HBBLE PT = ,F10.5,4H DF, )
 9332 FORMAT (1H+,44X,8HDEW PT = ,F10.5)
 9350 FORMAT (57H0****NON-CONDENSABLES PRESENT,BUBBLE POINT CAL. WITH VF
     1= ,F6.3)
 9355 FORMAT(/,10X,'BUBBLE AND/OR DEW POINT TEMPS OUT OF BOUNDS',2E20.4)
 9400 FORMAT (51H0****NON-VOLATILES PRESENT,DEW POINT CAL. WITH LF= ,
     1F6.3)
C
 9500 FORMAT (32H0**** TBTD HAS BAD DATA --- KS =,I3, 6H, JW =, I3 / 6X,
     1    3HP =,F10.4, 6H, TB =,F10.4,6H, TD =, F10.4,6H, WT =, F12.3 )
C
      END

      SUBROUTINE TSET (JW,FLAG)                                         
C     *************************                                         
C                                                                       
C         SETS PTPEN(JW,2) = TEMPERATURE AT PROCESS POINT JW (IN UNIT   
C         LIST), TO MATCH PRESSURE IN PTPEN(JW,1) AND MOLAL ENTHALPY    
C         IN PTPEN(JW,3). USES HMX TO CALCULATE ENTHALPIES FOR          
C         ESTIMATED TEMPERATURES, ASSUMING --                           
C           IF FLAG = -1.0 - VAPOR-LIQUID MIX AT PRESSURE IN PTPEN(JW,1)
C              FLAG =  0.0 - SATURATED LIQUID                           
C              FLAG =  1.0 - VAPOR, AT LOW PRESSURES                    
C                                                                       
C         WRITTEN BY R.R. HUGHES             EES IDENT SP/TSET          
C              LAST REVISION MAY 2, 1974                                
C                                                                       
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCPS,NPTP,NCST,NREC,NEN,WTEN(5),   
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
      COMMON /FLOS/ FV(12),FL(12),FW(12)          
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
      DIMENSION T(2),H(2)                         
      DATA  DTOL/1.E-5/,NLIM/20/,DTS/10./,DTB/1.E8/
C
        HS = PTPEN(JW,3) 
       IF (KFLAG.GT.1) WRITE(JOUT,9000) HS  
C                                           
C         CHOOSE OPTION, AND IDENTIFY PHASES IF NECESSARY 
      IF (FLAG.GE.0.) GO TO 16                            
        P = PTPEN (JW,1)                                  
      CALL TBTD (3,JW,P,T(1),T(2),H(1),H(2))
c      write(jout,1200)H(1),H(2)
c1200  format(2x,'hbub= ',F10.4,'hdew= ',F10.4)
      DO 5 J = 1,2                                        
      IF(KPROPS.GT.0)GO TO 3
      PTPEN(JW,2)=T(J)
      FLG=FLOAT(J-1)
      CALL HMX(JW,FLG)
      H(J)=PTPEN(JW,3)
3     IF (((H(J)-HS)*(-1.)**J).LE.0.) GO TO 14            
    5 CONTINUE                                            
C                                                         
C         TWO PHASES PRESENT                              
        X = (HS-H(1))/(H(2)-H(1))                         
      IF ((T(2)-T(1)).LT.DTOL) GO TO 100                  
        DT = X*(T(2)-T(1))                                
        H2 = H(1)                                         
        PTPEN (JW,2) = T(1) + DT                          
      GO TO 18                                            
C                                                         
C         ONLY ONE PHASE PRESENT,- J=1 FOR LIQUID, J=2 FOR VAPOR  
   14   H2 = H(J)                                                 
        DT = DTS*(-1.)**J                                         
        PTPEN (JW,2) = T(J) + DT                                  
        IF(J.EQ.1) FLG=0.
        IF(J.EQ.2) FLG=1.
	GO TO 20              
C                             
C         SINGLE PHASE SPECIFIED BY FLAG      
   16 IF (ABS(PTPEN(JW,2)).LT.DTOL) PTPEN(JW,2) = SIGN (DTS,HS) 
        DT = PTPEN(JW,2)                                        
        H2 = 0.                                                 
   18   FLG = FLAG                                              
C                                                               
C         ADJUST TEMPERATURE TO MATCH ENTHALPY SPEC, USING NEWTON'S METH
   20 DO 50 I = 1,NLIM                                                  
C      write(jout,1234)flg
C1234  format(2x,'tset: flg=',F5.2)
      CALL HMX (JW,FLG)                                                 
        H1 = PTPEN(JW,3)                                                
        DT = (HS - H1)*DT/(H1 - H2)                                     
        DT = SIGN (MIN(ABS(DT),(DTB**(1./FLOAT(I)))),DT)              
      IF (KFLAG.LE.1) GO TO 25                                          
       WRITE(JOUT,9025) I,H1,PTPEN(JW,2),DT                             
   25   H2 = H1                                                         
        PTPEN(JW,2) = PTPEN(JW,2) + DT                                  
      IF (ABS(DT).LE.DTOL) GO TO 80                                     
   50 CONTINUE                                                          
C                                                                       
      WRITE(JOUT,9050) NLIM , DT                                       
   80   PTPEN (JW,3) = HS                                              
      GO TO 900                                                        
C                                                                      
C         SINGLE COMPONENT, TWO PHASES                                 
  100   PTPEN(JW,3) = HS                                               
        PTPEN (JW,4) = X                                               
  900 RETURN                                                           
C                                                                      
 9050 FORMAT (  6H0AFTER, I8,25H ITERATIONS, TEMP DIFF IS,  G11.5,     
     1   9H DEG FAHR )                                                 
 9000 FORMAT (30H FIND TEMP. TO MATCH ENTHY. OF ,F12.3)                
 9025 FORMAT (5H ITER,I4,3X,4HH = ,F12.3,6H, T = ,F12.3,               
     1  12H, NEXT DT = ,F12.5 )                                        
C                                                                      
      END                                                              
	SUBROUTINE VMGAS(FV,T,P,GASRES)
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

      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCP,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCP,KTLN(15)
      COMMON /QP/ CSXX(12,28),ANAMC(12,6)
      DIMENSION FV(12)
C      EQUIVALENCE (TC,TR),(SUM,PR),(VC,PC),(ZC,Z)
C++++++++++++++++++++++++++++++++++++++++++++++++++
      REAL GASRES
C
        SUM=0.0
        TC=0.0
        VC=0.0
        ZC=0.0
        TA=T+459.67
C            SUM THE CRITICAL PROPERTIES
        DO 100 I=1,NCP
           SUM=SUM+FV(I)
           TC=TC+FV(I)*CSXX(I,1)
           VC=VC+FV(I)*CSXX(I,22)/62.43
           ZC=ZC+FV(I)*CSXX(I,21)
100     CONTINUE
C            WEIGHTED AVERAGE CRITICAL PROPERTIES
           TC=TC/SUM
           VC=VC/SUM
           ZC=ZC/SUM
C            GENERALIZED CORRELATION
           VC=10.7335*TC*ZC/VC
           TC=TA/TC
           SUM=P/VC
           ZA=1.0-19.38*SUM*EXP(-4.0*TC)
           BZ=-4.0928E-3
           ZB=BZ*SUM**2+0.15402*SUM+BZ/10
           ZC=AMAX1(ZA,ZB)
           GASRES=10.7335*TA*ZC/P
C
           RETURN
           END
C
      SUBROUTINE VMLIQ (FL,T,LIQRES)
C     *********************
C
C         CALCULATES MOLAL VOLUME (CU FT/LB MOL) OF STREAM WITH MOLAL
C              FLOWS, FL, AS SATURATED LIQUID AT T DEG FAHR.
C
C         WRITTEN BY R.R.HUGHES         EES IDENT SP/VML
C              LAST REVISION AUGUST 27, 1973
C
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCP,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCP,KTLN(15)
      COMMON /QP/ CSXX(12,28),ANAMC(12,6)
      DIMENSION FL(12)
C+++++++++++++++++++++++++++++++++++++++++++++++++
	REAL LIQRES,SUM
C
        TS  = T/1000.
        SUM = 0.
        LIQRES= 0.
C
      DO 100 I = 1,NCP
C               LIQUID DENSITY CORRELATION
          DLI = AMAX1(1.0,(CSXX(I,16)+TS*(CSXX(I,17)+ TS*CSXX(I,18))))
        LIQRES= LIQRES+ CSXX(I,6)*FL(I)/DLI
100     SUM = SUM + FL(I)
	LIQRES= LIQRES/ SUM
C
      RETURN
      END
C
      SUBROUTINE WTMOL(JW)
C     ********************
C
C        SPAD SUBROUTINE FOR CALCULATING AVG MOL WT OF A STREAM WITH
C        COMPONENT MOLAL FLOWS WEN(JW,I) AND TOTAL MOLAL FLOW WTEN(JW)
C        (WTEN WILL BE CALCULATED IF ENTRY IS ZERO)
C                                      EES IDENT SP/WTMOL
C        WRITTEN BY R. R. HUGHES
C             LAST REVISION AUG. 1 , 1973
C
      LOGICAL MDO
      COMMON /UNPT/ JOUT,KNTRL,KFLAG,NCPS,NPTP,NCST,NREC,NEN,WTEN(5),
     1  WEN(5,12),PTPEN(5,6),COST(5),EN(100),KTLN(15)
c     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
c     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
      COMMON/QP/CSXX(12,28),ANAMC(12,6)
C            CHECK THE TOTAL FLOW
      IF(WTEN(JW).LE.1.0E-10) GO TO 30
C
      MDO=.FALSE.
      GO TO 40
30    MDO=.TRUE.
      WTEN(JW)=0.
40    WM=0.
C
      DO 70 I=1,NCPS
C           CALCULATE THE TOTAL FLOW, IF NECESSARY
      IF(MDO)WTEN(JW)=WTEN(JW)+WEN(JW,I)
C           SUM THE MOLECULAR WEIGHTS OF EACH COMPONENT
      WM=WM+WEN(JW,I)*CSXX(I,6)
70    CONTINUE
C
      IF(WTEN(JW).LE.0.)GO TO 80
C           CALCULATE THE AVERAGE MOLECULAR WEIGHT OF STREAM JW
      PTPEN(JW,6)=WM/WTEN(JW)
75    RETURN
C           ZERO FLOW MEANS ZERO MOLECULAR WEIGHT
80    PTPEN (JW,6)=0.
      GO TO 75
C
      END
