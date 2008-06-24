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
