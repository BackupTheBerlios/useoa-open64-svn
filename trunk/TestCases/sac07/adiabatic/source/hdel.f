
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
