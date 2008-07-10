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
