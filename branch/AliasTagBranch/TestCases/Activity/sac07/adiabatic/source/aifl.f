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
