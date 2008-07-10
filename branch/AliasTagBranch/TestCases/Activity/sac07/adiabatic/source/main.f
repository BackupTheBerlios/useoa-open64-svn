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

      COMMON/QP/ CSXX(12,28), ANAMC(12,6)

*
*     local vars for vapor, liquid, pipe, pressure
*
      real vapor(6), liquid(6), pipe(6), pressure(5)

3      NCPS = 6
      NPTP = 6
      NCST = 1
      JOUT = 6
      KFLAG = 2
      EN(1) = 1
      EN(2) = 10.
      EN(3) = 100.

      DO I = 1, 5
        WTEN(I) = 0.
        pressure(i) = 0.
        DO J = 1,6
           WEN(I,J) = 0.
           WEN(I,J+6) = 0.
           PTPEN(I,J) = 0.
        ENDDO
      ENDDO


C
C   THESE ARE THE INPUTS (WEN(3,*) AND PTPEN(3,*), FEED FLOW RATE
C
* wten(3,*) is initialized in aifl using pipe and ptpen(*,1) using pressure
*
      DO II = 1, NCPS
       pipe(ii) = 100.
      ENDDO

cpdh      PTPEN(3,1) = 100.
      pressure(3) = 100.

      PTPEN(3,2) = 100.
      
      DO JC = 1, NCPS
      READ(23,8055,END=80) (DATA(IK),IK=1,6)      
8055  FORMAT(6A3)
      DO II = 1,6
         ANAMC(JC,II) = DATA(II)
      ENDDO
      READ(23,*) (DATA(IK),IK=7,40)
      DO 57 ICK=1,28
      CSXX(JC,ICK)=DATA(ICK+6)
57    CONTINUE
      ENDDO


C

c
c      fmt = '(6(6(e12.6,2x)/)'

c1111  format(6(6(e12.6,2x)/))
c1112  format(1x,'vapor :',e12.6.2x,'liquid :',e12.6/)
c1113  format(1x,'ADIFOR : ',f18.7,2x,'DIV DIFF : ',f18.7)
******************************

      CALL AIFL(1,pipe,pressure,vapor,liquid)

      open (UNIT=38,FILE='out.vapor')
      open (UNIT=39,FILE='out.liquid')
      
      do i = 1,6
         write (38,*) vapor(i)
         write (39,*) liquid(i)
      enddo
      
      close (UNIT=38)
      close (UNIT=39)


******************************
      
C
C     THESE ARE THE OUTPUTS
C
      WRITE(JOUT,8083)
      DO I = 1, NCPS
      WRITE(6,8081) (ANAMC(I,JJ),JJ=1,6), WEN(2,I), WEN(1,I)
      ENDDO
      WRITE(JOUT,8084)
      DO I = 1, 6
      WRITE(6,8082) PTPEN(2,I), PTPEN(1,I)
      ENDDO
 8081 FORMAT(5X,6A3,2F10.4)
 8082 FORMAT(23X,2F10.4)
 8083 FORMAT(//,5X,'***LIQUID AND VAPOR FLOWRATES***',/)
 8084 FORMAT(/,5X,'***LIQUID AND VAPOR PROPERTIES***',/)

      STOP
C
   80 WRITE(JOUT,8080)
8080  FORMAT(5X, 'MISSING COMPONENTS IN FILE')
      STOP
      END


