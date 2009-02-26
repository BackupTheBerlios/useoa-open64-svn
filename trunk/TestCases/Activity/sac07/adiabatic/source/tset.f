
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
