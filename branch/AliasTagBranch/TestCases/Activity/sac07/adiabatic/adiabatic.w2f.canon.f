        module w2f__types

        integer :: w2f__4, w2f__8, w2f__16
        parameter (w2f__4 = kind(0.0))
        parameter (w2f__8 = kind(0.0d0))
        parameter (w2f__16 = selected_real_kind(p=30))

        integer :: w2f__i1, w2f__i2, w2f__i4,w2f__i8
        parameter (w2f__i1 = selected_int_kind(r=2))
        parameter (w2f__i2 = selected_int_kind(r=3))
        parameter (w2f__i4 = selected_int_kind(r=8))
        parameter (w2f__i8 = selected_int_kind(r=16))

        end module w2f__types
C ***********************************************************
C Fortran file translated from WHIRL Tue Aug 22 14:22:02 2006
C ***********************************************************
C ***********************************************************

      PROGRAM flash
      use w2f__types
      IMPLICIT NONE
C
C     **** Local Variables and Functions ****
C
      EXTERNAL aifl
      REAL(w2f__4) LIQUID(1 : 6)
      REAL(w2f__4) PIPE(1 : 6)
      REAL(w2f__4) PRESSURE(1 : 5)
      REAL(w2f__4) VAPOR(1 : 6)
      integer oad_ctmp0
C
C     **** Statements ****
C
      oad_ctmp0 = (1)
      call aifl(oad_ctmp0,PIPE,PRESSURE,VAPOR,LIQUID)
      
      END PROGRAM

      SUBROUTINE aifl(KF, PIPE, PRESSURE, VAPOR, LIQUID)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /unpt/
      COMMON /unpt/ JOUT, KNTRL, KFLAG, NCPS, NPTP, NCST, NREC, NEN,
     >  WTEN, WEN, PTPEN, COST, EN, KTLN
      INTEGER(w2f__i4) JOUT
      INTEGER(w2f__i4) KNTRL
      INTEGER(w2f__i4) KFLAG
      INTEGER(w2f__i4) NCPS
      INTEGER(w2f__i4) NPTP
      INTEGER(w2f__i4) NCST
      INTEGER(w2f__i4) NREC
      INTEGER(w2f__i4) NEN
      REAL(w2f__4) WTEN(1 : 5)
      REAL(w2f__4) WEN(1 : 5, 1 : 12)
      REAL(w2f__4) PTPEN(1 : 5, 1 : 6)
      REAL(w2f__4) COST(1 : 5)
      REAL(w2f__4) EN(1 : 100)
      INTEGER(w2f__i4) KTLN(1 : 15)
      SAVE /flos/
      COMMON /flos/ FV, FL, FW
      REAL(w2f__4) FV(1 : 12)
      REAL(w2f__4) FL(1 : 12)
      REAL(w2f__4) FW(1 : 12)
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) KF
      REAL(w2f__4) PIPE(1 : *)
      REAL(w2f__4) PRESSURE(1 : *)
      REAL(w2f__4) VAPOR(1 : *)
      REAL(w2f__4) LIQUID(1 : *)
C
C     **** Local Variables and Functions ****
C
      EXTERNAL dnsty
      REAL(w2f__4) FLAG
      EXTERNAL hmx
      REAL(w2f__4) HTOL
      SAVE HTOL
      INTEGER(w2f__i4) I
      INTEGER(w2f__i4) II
      INTEGER(w2f__i4) J
      INTEGER(w2f__i4) K
      INTEGER(w2f__i4) KPROPS
      INTEGER(w2f__i4) NIN
      INTEGER(w2f__i4) NOUT
      INTEGER(w2f__i4) NOUT1
      
      INTEGER(w2f__i4) t__8(1 : 4)
      INTEGER(w2f__i4) NT(1 : 2, 1 : 2)
      EQUIVALENCE(tmp0(0), t__8)
      EQUIVALENCE(tmp0(0), NT)
      INTEGER(w2f__i4) t__8(1 : 4)
      SAVE t__8
      INTEGER(w2f__i4) NT(1 : 2, 1 : 2)
      SAVE NT
      INTEGER(w2f__i4) NUM
      EXTERNAL tset
      EXTERNAL wtmol
      REAL(w2f__4) WTOL
      SAVE WTOL
      INTEGER(w2f__i4) t__9
      INTEGER(w2f__i4) t__10
      INTEGER(w2f__i4) t__11
      INTEGER(w2f__i4) t__12
      INTEGER(w2f__i4) t__13
      INTEGER(w2f__i4) t__14
      INTEGER(w2f__i4) t__15
      INTEGER(w2f__i4) t__16
      INTEGER(w2f__i4) t__17
      INTEGER(w2f__i4) t__18
      INTEGER(w2f__i4) t__19
      INTEGER(w2f__i4) t__20
      INTEGER(w2f__i4) t__21
      INTEGER(w2f__i4) t__22
      INTEGER(w2f__i4) t__23
C
C     **** Temporary Variables ****
C
      INTEGER(w2f__i1) tmp0(1 : 16)
      INTEGER(w2f__i8) tmp1
C
C     **** Initializers ****
C
      DATA HTOL / 1.0000000475E-03 /
      DATA(t__8(tmp1), tmp1 = 1, 4, 1) / 541672513, 539902529,
     >  542069577, 539904084 /
      DATA WTOL / 9.9999997474E-06 /
      integer oad_ctmp0
      real(w2f__4) oad_ctmp1
      integer oad_ctmp2
      real(w2f__4) oad_ctmp3
      integer(w2f__i4) oad_ctmp4
      integer(w2f__i4) oad_ctmp5
      real oad_ctmp6
      real oad_ctmp7
      real oad_ctmp8
      integer(w2f__i4) oad_ctmp9
      real oad_ctmp10
      real oad_ctmp11
      integer(w2f__i4) oad_ctmp12
      integer(w2f__i4) oad_ctmp13
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(PIPE)
C$OPENAD DEPENDENT(VAPOR)
C$OPENAD DEPENDENT(LIQUID)
C
C     **** Statements ****
C
      call oad_s_IFIX(EN(1),oad_ctmp0)
      NIN = oad_ctmp0
      NOUT = 2
      NUM = NIN + NOUT
      t__9 = NOUT1
      t__10 = NUM
      DO J = t__9, t__10, 1
        t__11 = NCPS
        DO II = 1, t__11, 1
          call oad_s_PIPE(II,oad_ctmp1)
          WEN(J,II) = oad_ctmp1
          WTEN(J) = (WTEN(J) + WEN(J,II))
        END DO
      END DO
      call oad_s_IFIX(EN(1),oad_ctmp2)
      NIN = oad_ctmp2
      NOUT = 2
      NUM = NIN + NOUT
      WTEN(1) = 0.0
      EN(19) = 0.0
      NOUT1 = NOUT + 1
      t__12 = NUM
      DO II = 1, t__12, 1
        call oad_s_PRESSURE(II,oad_ctmp3)
        PTPEN(II,1) = oad_ctmp3
      END DO
      t__13 = NOUT1
      t__14 = NUM
      DO J = t__13, t__14, 1
        if ( EN(2) .gt. PTPEN(J,1) ) then
C         IF (PTPEN(J,1).LT.EN(2)) WRITE(JOUT,8110) J
          WRITE(JOUT,
     >  '(23H0**** PRESSURE AT INLET, I3,30H LESS THAN FL' //
     >  'ASH PRESSUR                                     ' //
     >  '                       E **** )') J
        ENDIF
        if ( WTEN(J) .le. WTOL ) then
          oad_ctmp4 = (J)
          call wtmol(oad_ctmp4)
        ENDIF
        IF(KPROPS .LE. 0) GO TO 28
        GO TO 29
28      CONTINUE
        if ( PTPEN(J,3) .le. HTOL ) then
          oad_ctmp5 = (J)
          oad_ctmp6 = (-1.0)
          call hmx(oad_ctmp5,oad_ctmp6)
        ENDIF
29      CONTINUE
        WTEN(1) = (WTEN(J) + WTEN(1))
        EN(19) = (EN(19) + WTEN(J) * PTPEN(J,3))
1       CONTINUE
      END DO
      WRITE(JOUT, '(2x,"aifl:",F10.4)')(PTPEN(J, 3), J = NOUT1, NUM, 1)
      PTPEN(NOUT1,1) = EN(2)
      PTPEN(NOUT1,2) = EN(3)
      IF(WTEN(1) .LE. WTOL) GO TO 16
      IF(NOUT1 .GE. NUM) GO TO 7
      t__15 = NOUT1 + 1
      t__16 = NUM
      DO J = t__15, t__16, 1
        t__17 = NCPS
        DO I = 1, t__17, 1
3         CONTINUE
          WEN(NOUT1,I) = (WEN(J,I) + WEN(NOUT1,I))
        END DO
5       CONTINUE
      END DO
7     CONTINUE
      WTEN(NOUT1) = 0.0
      call wtmol(NOUT1)
      if ( WTOL .lt. (ABS(WTEN(NOUT1) - WTEN(1))) ) then
C       IF ((ABS(WTEN(NOUT1)-WTEN(1))).GT.WTOL) WRITE(JOUT,8130)
        WRITE(JOUT, '(54H0**** COMPONENT FLOW TOTAL DIFFERS FROM TOTA'
     >  // 'L FLOW ***)')
      ENDIF
      PTPEN(NOUT1,3) = (EN(19) / WTEN(NOUT1))
      IF(KFLAG .LE. 1) GO TO 8
      WRITE(JOUT, '(1X,2A3,10HFLASH, AT ,F10.4,10H PSIA, AND ,F10.4' //
     >  ',3H DF )')(NT(K, KF), K = 1, 2, 1), (PTPEN(NOUT1, J), J = 1, 2
     > , 1)
      if ( KF .le. 1 ) then
C       IF (KF.LE.1) WRITE(JOUT,9131)
        WRITE(JOUT, '(1H+,50X,16H(STARTING TRIAL) )')
      ENDIF
      WRITE(JOUT, '(2X,17HCOMBINED FEEDS -- /5X,F12.3,12H BTU/LB MO' //
     >  'L ,5X,  F12.4,11H LB MOLS/HR / 5X,17HCOMPONENT F' //
     >  'LOWS - / (5X,4F12.4))') PTPEN(NOUT1, 3), WTEN(NOUT1), (WEN(
     > NOUT1, I), I = 1, NCPS, 1)
8     CONTINUE
      oad_ctmp7 = (-1.0)
      call tset(NOUT1,oad_ctmp7)
      t__18 = KF
      IF(t__18 .eq. 1) GO TO 10
      IF(t__18 .eq. 2) GO TO 9
9     CONTINUE
      EN(20) = PTPEN(NOUT1,2)
      PTPEN(NOUT1,2) = EN(3)
      oad_ctmp8 = (-1.0)
      call hmx(NOUT1,oad_ctmp8)
10    CONTINUE
      EN(18) = PTPEN(NOUT1,4)
      t__19 = NCPS
      DO I = 1, t__19, 1
        WEN(1,I) = FV(I)
        WEN(2,I) = FL(I)
        if ( NOUT .ge. 3 ) then
          WEN(3,I) = FW(I)
        ENDIF
11      CONTINUE
      END DO
      t__20 = NOUT
      DO J = 1, t__20, 1
        PTPEN(J,1) = PTPEN(NOUT1,1)
        PTPEN(J,2) = PTPEN(NOUT1,2)
        WTEN(J) = 0.0
        oad_ctmp9 = (J)
        call wtmol(oad_ctmp9)
        oad_ctmp11 = FLOAT(2 - J)
        call oad_s_MAX_r(oad_ctmp11,0.0,oad_ctmp10)
        FLAG = oad_ctmp10
        oad_ctmp12 = (J)
        call hmx(oad_ctmp12,FLAG)
        oad_ctmp13 = (J)
        call dnsty(oad_ctmp13,FLAG)
13      CONTINUE
      END DO
      GO TO 34
16    CONTINUE
C     180 WRITE(JOUT,8180) WTOL
      WRITE(JOUT, '(27H0**** COMBINED FEED FLOW LT , E12.3,5H **** ' //
     >  ')') WTOL
      t__21 = NOUT
      DO J = 1, t__21, 1
        PTPEN(J,1) = PTPEN(NOUT1,1)
        PTPEN(J,2) = PTPEN(NOUT1,2)
        WTEN(J) = 0.0
        t__22 = NCPS
        DO I = 1, t__22, 1
17        CONTINUE
          WEN(J,I) = 0.0
        END DO
        DO I = 3, 6, 1
19        CONTINUE
          PTPEN(J,I) = 0.0
        END DO
21      CONTINUE
      END DO
34    CONTINUE
      t__23 = NCPS
      DO II = 1, t__23, 1
        VAPOR(II) = WEN(1, II)
        LIQUID(II) = WEN(2, II)
      END DO
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE dnsty(JW, FLAG)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /unpt/
      COMMON /unpt/ JOUT, KNTRL, KFLAG, NCPS, NPTP, NCST, NREC, NEN,
     >  WTEN, WEN, PTPEN, COST, EN, KTLN
      INTEGER(w2f__i4) JOUT
      INTEGER(w2f__i4) KNTRL
      INTEGER(w2f__i4) KFLAG
      INTEGER(w2f__i4) NCPS
      INTEGER(w2f__i4) NPTP
      INTEGER(w2f__i4) NCST
      INTEGER(w2f__i4) NREC
      INTEGER(w2f__i4) NEN
      REAL(w2f__4) WTEN(1 : 5)
      REAL(w2f__4) WEN(1 : 5, 1 : 12)
      REAL(w2f__4) PTPEN(1 : 5, 1 : 6)
      REAL(w2f__4) COST(1 : 5)
      REAL(w2f__4) EN(1 : 100)
      INTEGER(w2f__i4) KTLN(1 : 15)
      SAVE /flos/
      COMMON /flos/ FV, FL, FW
      REAL(w2f__4) FV(1 : 12)
      REAL(w2f__4) FL(1 : 12)
      REAL(w2f__4) FW(1 : 12)
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) JW
      REAL(w2f__4) FLAG
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) GASRES
      INTEGER(w2f__i4) I
      EXTERNAL iflash
      INTEGER(w2f__i4) KLAG
      REAL(w2f__4) LIQRES
      REAL(w2f__4) VM
      EXTERNAL vmgas
      EXTERNAL vmliq
      REAL(w2f__4) VTOL
      SAVE VTOL
      EXTERNAL wtmol
      REAL(w2f__4) WTOL
      SAVE WTOL
      INTEGER(w2f__i4) t__24
      INTEGER(w2f__i4) t__25
      INTEGER(w2f__i4) t__26
C
C     **** Initializers ****
C
      DATA VTOL / 9.9999999748E-07 /
      DATA WTOL / 1.0000000134E-10 /
      integer oad_ctmp0
      real(w2f__4) oad_ctmp1
      integer(w2f__i4) oad_ctmp2
      integer(w2f__i4) oad_ctmp3
C
C     **** Statements ****
C
      IF((WTEN(JW) .GT. WTOL) .AND.(PTPEN(JW, 6) .GT. WTOL)) GO TO 1
      call wtmol(JW)
      IF(WTEN(JW) .GT. WTOL) GO TO 1
      PTPEN(JW,5) = 0.0
      GO TO 10
1     CONTINUE
      oad_ctmp1 = FLAG + 2.0999999046
      call oad_s_IFIX(oad_ctmp1,oad_ctmp0)
      KLAG = oad_ctmp0
      call oad_s_MAX_i(KLAG,1,oad_ctmp3)
      call oad_s_MIN_i(oad_ctmp3,3,oad_ctmp2)
      KLAG = oad_ctmp2
      t__24 = KLAG
      IF(t__24 .eq. 1) GO TO 2
      IF(t__24 .eq. 2) GO TO 4
      IF(t__24 .eq. 3) GO TO 7
2     CONTINUE
      call iflash(JW)
      IF(PTPEN(JW, 4) .LT. VTOL) GO TO 4
      IF(PTPEN(JW, 4) .GT.(1.0 - VTOL)) GO TO 7
      call vmgas(FV,PTPEN(JW,2),PTPEN(JW,1),GASRES)
      call vmliq(FL,PTPEN(JW,2),LIQRES)
      VM = PTPEN(JW,4) * GASRES + LIQRES * (1.0 - PTPEN(JW,4))
      PTPEN(JW,5) = (PTPEN(JW,6) / VM)
      GO TO 10
4     CONTINUE
      t__25 = NCPS
      DO I = 1, t__25, 1
5       CONTINUE
        FL(I) = WEN(JW,I)
      END DO
      call vmliq(FL,PTPEN(JW,2),LIQRES)
      PTPEN(JW,5) = (PTPEN(JW,6) / LIQRES)
      GO TO 10
7     CONTINUE
      t__26 = NCPS
      DO I = 1, t__26, 1
8       CONTINUE
        FV(I) = WEN(JW,I)
      END DO
      call vmgas(FV,PTPEN(JW,2),PTPEN(JW,1),GASRES)
      PTPEN(JW,5) = (PTPEN(JW,6) / GASRES)
10    CONTINUE
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE hdel(I, T, HDRES)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /qp/
      COMMON /qp/ CSXX, ANAMC
      REAL(w2f__4) CSXX(1 : 12, 1 : 28)
      REAL(w2f__4) ANAMC(1 : 12, 1 : 6)
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) I
      REAL(w2f__4) T
      REAL(w2f__4) HDRES
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) A
      REAL(w2f__4) TABS
      REAL(w2f__4) TBOILI
C
C     **** Statements ****
C
      TBOILI = CSXX(I,8) + 4.5967001343E+02
      TABS = T + 4.5967001343E+02
      A = (CSXX(I,1) - TABS) / (CSXX(I,1) - TBOILI)
      if ( A .le. 0.0 ) then
        A = 9.9999996827E-21
      ENDIF
      HDRES = CSXX(I,19) * (A ** CSXX(I,20))
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE hmx(JW, FLAG)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /unpt/
      COMMON /unpt/ JOUT, KNTRL, KFLAG, NCPS, NPTP, NCST, NREC, NEN,
     >  WTEN, WEN, PTPEN, COST, EN, KTLN
      INTEGER(w2f__i4) JOUT
      INTEGER(w2f__i4) KNTRL
      INTEGER(w2f__i4) KFLAG
      INTEGER(w2f__i4) NCPS
      INTEGER(w2f__i4) NPTP
      INTEGER(w2f__i4) NCST
      INTEGER(w2f__i4) NREC
      INTEGER(w2f__i4) NEN
      REAL(w2f__4) WTEN(1 : 5)
      REAL(w2f__4) WEN(1 : 5, 1 : 12)
      REAL(w2f__4) PTPEN(1 : 5, 1 : 6)
      REAL(w2f__4) COST(1 : 5)
      REAL(w2f__4) EN(1 : 100)
      INTEGER(w2f__i4) KTLN(1 : 15)
      SAVE /flos/
      COMMON /flos/ FV, FL, FW
      REAL(w2f__4) FV(1 : 12)
      REAL(w2f__4) FL(1 : 12)
      REAL(w2f__4) FW(1 : 12)
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) JW
      REAL(w2f__4) FLAG
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) H
      EXTERNAL hdel
      REAL(w2f__4) HDRES
      REAL(w2f__4) HL
      REAL(w2f__4) HV
      EXTERNAL hvap
      REAL(w2f__4) HVRES
      INTEGER(w2f__i4) I
      EXTERNAL iflash
      INTEGER(w2f__i4) KLAG
      REAL(w2f__4) T
      REAL(w2f__4) VZHI
      SAVE VZHI
      REAL(w2f__4) VZLO
      SAVE VZLO
      EXTERNAL wtmol
      REAL(w2f__4) WTOL
      SAVE WTOL
      INTEGER(w2f__i4) t__27
      INTEGER(w2f__i4) t__28
      INTEGER(w2f__i4) t__29
      INTEGER(w2f__i4) t__30
C
C     **** Initializers ****
C
      DATA VZHI / 9.9999898672E-01 /
      DATA VZLO / 9.9999999748E-07 /
      DATA WTOL / 1.0000000134E-10 /
      integer oad_ctmp0
      real(w2f__4) oad_ctmp1
      integer(w2f__i4) oad_ctmp2
      integer(w2f__i4) oad_ctmp3
      integer(w2f__i4) oad_ctmp4
      integer(w2f__i4) oad_ctmp5
C
C     **** Statements ****
C
      IF(WTEN(JW) .GT. WTOL) GO TO 1
      call wtmol(JW)
      IF(WTEN(JW) .GT. WTOL) GO TO 1
      PTPEN(JW,3) = 0.0
      PTPEN(JW,4) = 0.0
      GO TO 15
1     CONTINUE
      oad_ctmp1 = FLAG + 2.0999999046
      call oad_s_IFIX(oad_ctmp1,oad_ctmp0)
      KLAG = oad_ctmp0
      call oad_s_MAX_i(KLAG,1,oad_ctmp3)
      call oad_s_MIN_i(oad_ctmp3,3,oad_ctmp2)
      KLAG = oad_ctmp2
      T = PTPEN(JW,2)
      t__27 = KLAG
      IF(t__27 .eq. 1) GO TO 2
      IF(t__27 .eq. 2) GO TO 3
      IF(t__27 .eq. 3) GO TO 6
2     CONTINUE
      call iflash(JW)
      if ( PTPEN(JW,4) .lt. VZLO ) then
        KLAG = 2
      ENDIF
      if ( PTPEN(JW,4) .gt. VZHI ) then
        KLAG = 3
      ENDIF
      GO TO 9
3     CONTINUE
      PTPEN(JW,4) = 0.0
      t__28 = NCPS
      DO I = 1, t__28, 1
        FL(I) = WEN(JW,I)
4       CONTINUE
      END DO
      GO TO 9
6     CONTINUE
      PTPEN(JW,4) = 1.0
      t__29 = NCPS
      DO I = 1, t__29, 1
        FV(I) = WEN(JW,I)
7       CONTINUE
      END DO
9     CONTINUE
      H = 0.0
      t__30 = NCPS
      DO I = 1, t__30, 1
        oad_ctmp4 = (I)
        call hvap(oad_ctmp4,T,HVRES)
        HV = HVRES
        oad_ctmp5 = (I)
        call hdel(oad_ctmp5,T,HDRES)
        HL = HV - HDRES
        t__27 = KLAG
        IF(t__27 .eq. 1) GO TO 10
        IF(t__27 .eq. 2) GO TO 11
        IF(t__27 .eq. 3) GO TO 12
10      CONTINUE
        H = H + FL(I) * HL + FV(I) * HV
        GO TO 13
11      CONTINUE
        H = H + WEN(JW,I) * HL
        GO TO 13
12      CONTINUE
        H = H + WEN(JW,I) * HV
13      CONTINUE
      END DO
      PTPEN(JW,3) = (H / WTEN(JW))
15    CONTINUE
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE hvap(I, T, HVRES)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /qp/
      COMMON /qp/ CSXX, ANAMC
      REAL(w2f__4) CSXX(1 : 12, 1 : 28)
      REAL(w2f__4) ANAMC(1 : 12, 1 : 6)
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) I
      REAL(w2f__4) T
      REAL(w2f__4) HVRES
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) H(1 : 2)
      EXTERNAL hdel
      REAL(w2f__4) HDRES
      INTEGER(w2f__i4) J
      REAL(w2f__4) TA
      REAL(w2f__4) TZ
      SAVE TZ
      REAL(w2f__4) XXXX
C
C     **** Initializers ****
C
      DATA TZ / 4.5967001343E+02 /
C
C     **** Statements ****
C
      XXXX = 0.0
      TA = TZ
      DO J = 1, 2, 1
        H(J) = (CSXX(I,9) * TA + CSXX(I,10) * (TA ** 2) + CSXX(I,11) * (
     +TA ** 3) + CSXX(I,12) * (TA ** 4) + CSXX(I,13) * (TA ** 5) + CSXX(
     +I,14) * (TA ** 6) + CSXX(I,15) * (TA ** 7))

        TA = T + TZ
1       CONTINUE
      END DO
      call hdel(I,XXXX,HDRES)
      HVRES = HDRES + H(2) - H(1)
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE iflash(JW)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /qp/
      COMMON /qp/ CSXX, ANAMC
      REAL(w2f__4) CSXX(1 : 12, 1 : 28)
      REAL(w2f__4) ANAMC(1 : 12, 1 : 6)
      SAVE /unpt/
      COMMON /unpt/ JOUT, KNTRL, KFLAG, NCPS, NPTP, NCST, NREC, NEN,
     >  WTEN, WEN, PTPEN, COST, EN, KTLN
      INTEGER(w2f__i4) JOUT
      INTEGER(w2f__i4) KNTRL
      INTEGER(w2f__i4) KFLAG
      INTEGER(w2f__i4) NCPS
      INTEGER(w2f__i4) NPTP
      INTEGER(w2f__i4) NCST
      INTEGER(w2f__i4) NREC
      INTEGER(w2f__i4) NEN
      REAL(w2f__4) WTEN(1 : 5)
      REAL(w2f__4) WEN(1 : 5, 1 : 12)
      REAL(w2f__4) PTPEN(1 : 5, 1 : 6)
      REAL(w2f__4) COST(1 : 5)
      REAL(w2f__4) EN(1 : 100)
      INTEGER(w2f__i4) KTLN(1 : 15)
      SAVE /flos/
      COMMON /flos/ FV, FL, FW
      REAL(w2f__4) FV(1 : 12)
      REAL(w2f__4) FL(1 : 12)
      REAL(w2f__4) FW(1 : 12)
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) JW
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) DEL
      REAL(w2f__4) DENOM
      REAL(w2f__4) DERIV
      INTEGER(w2f__i4) I
      INTEGER(w2f__i4) JJ
      INTEGER(w2f__i4) K
      INTEGER(w2f__i4) MCAL
      INTEGER(w2f__i4) MDO
      REAL(w2f__4) NUMER
      REAL(w2f__4) PSI
      EXTERNAL pvap
      REAL(w2f__4) PVRES
      REAL(w2f__4) R
      REAL(w2f__4) RK(1 : 12)
      REAL(w2f__4) RKMIN
      SAVE RKMIN
      REAL(w2f__4) TEST
      INTEGER(w2f__i4) t__34
      INTEGER(w2f__i4) t__35
      INTEGER(w2f__i4) t__36
      INTEGER(w2f__i4) t__37
      INTEGER(w2f__i4) t__38
      INTEGER(w2f__i4) t__39
      INTEGER(w2f__i4) t__40
C
C     **** Initializers ****
C
      DATA RKMIN / 1.0000000036E-15 /
      integer(w2f__i4) oad_ctmp0
C
C     **** Statements ****
C
      t__34 = NCPS
      DO I = 1, t__34, 1
        oad_ctmp0 = (I)
        call pvap(oad_ctmp0,PTPEN(JW,2),PVRES)
        RK(I) = (PVRES / PTPEN(JW,1))
        IF(RK(I) .GE. RKMIN) GO TO 1
        WRITE(JOUT, '(13H0** COMPONENT ,I4,23H SHOWS NEGL. VOLATILITY'
     >  // ' )') I
        RK(I) = RKMIN
1       CONTINUE
      END DO
      MCAL = 1
      PSI = 1.0
10    CONTINUE
      MDO = 0
      DO K = 1, 20, 1
        TEST = -WTEN(JW)
        DERIV = 0.0
        t__35 = NCPS
        DO I = 1, t__35, 1
          t__36 = MCAL
          IF(t__36 .eq. 1) GO TO 14
          IF(t__36 .eq. 2) GO TO 15
14        CONTINUE
          NUMER = (1.0 - 1E00 / (RK(I)))
          DENOM = 1.0 - NUMER * PSI
          GO TO 16
15        CONTINUE
          NUMER = RK(I) + (-1.0)
          DENOM = RK(I) - NUMER * PSI
16        CONTINUE
          TEST = TEST + (WEN(JW,I) / DENOM)
17        CONTINUE
          DERIV = DERIV + ((WEN(JW,I) * NUMER) / (DENOM ** 2))
        END DO
        IF(MDO .eq. 0) GO TO 22
        IF(MDO .eq. 1) GO TO 23
22      CONTINUE
        if ( TEST .le. 0.0 ) then
          t__36 = MCAL
          IF(t__36 .eq. 1) GO TO 11
          IF(t__36 .eq. 2) GO TO 18
        ENDIF
        MDO = 1
23      CONTINUE
        DEL = TEST / DERIV
        PSI = PSI - DEL
        IF(ABS(DEL) .LE. 9.9999999748E-07) GO TO 2
        IF((MCAL .eq. 1) .AND.(PSI .LT. 4.0000000596E-01)) GO TO 26
24      CONTINUE
      END DO
      WRITE(JOUT, '(22H0*** FLASH CALCN, TYPE, I3,31H, FAILED TO CN' //
     >  'VRGE IN 20                                      ' //
     >  '                       ITER -/ 10X,5HDEL =, G13.' //
     >  '3,11H, FOR PSI =,G13.6 )') MCAL, DEL, PSI
      GO TO 2
26    CONTINUE
      MCAL = 2
      PSI = 0.0
      GO TO 10
2     CONTINUE
      t__36 = MCAL
      IF(t__36 .eq. 1) GO TO 4
      IF(t__36 .eq. 2) GO TO 7
4     CONTINUE
      IF(PSI .GT. 9.9999898672E-01) GO TO 11
      R = PSI / (1.0 - PSI)
      t__37 = NCPS
      DO I = 1, t__37, 1
        FV(I) = (WEN(JW,I) / ((R / RK(I)) + 1.0))
5       CONTINUE
        FL(I) = (WEN(JW,I) - FV(I))
      END DO
      GO TO 30
7     CONTINUE
      IF(PSI .LT. 9.9999999748E-07) GO TO 18
      R = (1.0 - PSI) / PSI
      t__38 = NCPS
      DO I = 1, t__38, 1
        FL(I) = (WEN(JW,I) / (RK(I) * R + 1.0))
8       CONTINUE
        FV(I) = (WEN(JW,I) - FL(I))
      END DO
      GO TO 30
11    CONTINUE
      t__39 = NCPS
      DO I = 1, t__39, 1
        FL(I) = WEN(JW,I)
12      CONTINUE
        FV(I) = 0.0
      END DO
      GO TO 30
18    CONTINUE
      t__40 = NCPS
      DO I = 1, t__40, 1
        FV(I) = WEN(JW,I)
20      CONTINUE
        FL(I) = 0.0
      END DO
30    CONTINUE
      PTPEN(JW,4) = (1.0 - PSI)
      IF(KFLAG .LE. 1) GO TO 31
      WRITE(JOUT, '(10X,4HK(I),8X,11HVAPOR FLOWS,5X,9HLIQ FLOWS, 3X' //
     >  ',"COMPONENT",/,(5X,3G15.5,3X,6A3 ))')(RK(I), FV(I), FL(I), (
     > ANAMC(I, JJ), JJ = 1, 6, 1), I = 1, NCPS, 1)
31    CONTINUE
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE pvap(I, T, PVRES)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /qp/
      COMMON /qp/ CSXX, ANAMC
      REAL(w2f__4) CSXX(1 : 12, 1 : 28)
      REAL(w2f__4) ANAMC(1 : 12, 1 : 6)
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) I
      REAL(w2f__4) T
      REAL(w2f__4) PVRES
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) PLIQ
      REAL(w2f__4) PLMAX
      SAVE PLMAX
      REAL(w2f__4) PLMIN
      SAVE PLMIN
      REAL(w2f__4) TC
C
C     **** Initializers ****
C
      DATA PLMAX / 3.8E+01 /
      DATA PLMIN / -3.8E+01 /
C
C     **** Statements ****
C
      TC = (T + (-3.2E+01)) / 1.7999999523
      PLIQ = CSXX(I,24) - (CSXX(I,25) / (CSXX(I,26) + TC))
      IF(PLIQ .LT. PLMAX) GO TO 2
      PVRES = 1.0E+10
      GO TO 3
2     CONTINUE
      IF(PLIQ .GT. PLMIN) GO TO 1
      PVRES = 0.0
      GO TO 3
1     CONTINUE
      PVRES = (1.0E+01 ** PLIQ) * 1.9336842E-02
3     CONTINUE
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE tbtd(KS, JW, P, TB, TD, HB, HD)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /unpt/
      COMMON /unpt/ JOUT, KNTRL, KFLAG, NCPS, NPTP, NCST, NREC, NEN,
     >  WTEN, WEN, PTPEN, COST, EN, KTLN
      INTEGER(w2f__i4) JOUT
      INTEGER(w2f__i4) KNTRL
      INTEGER(w2f__i4) KFLAG
      INTEGER(w2f__i4) NCPS
      INTEGER(w2f__i4) NPTP
      INTEGER(w2f__i4) NCST
      INTEGER(w2f__i4) NREC
      INTEGER(w2f__i4) NEN
      REAL(w2f__4) WTEN(1 : 5)
      REAL(w2f__4) WEN(1 : 5, 1 : 12)
      REAL(w2f__4) PTPEN(1 : 5, 1 : 6)
      REAL(w2f__4) COST(1 : 5)
      REAL(w2f__4) EN(1 : 100)
      INTEGER(w2f__i4) KTLN(1 : 15)
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) KS
      INTEGER(w2f__i4) JW
      REAL(w2f__4) P
      REAL(w2f__4) TB
      REAL(w2f__4) TD
      REAL(w2f__4) HB
      REAL(w2f__4) HD
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) AKBAR
      REAL(w2f__4) AKBARL
      REAL(w2f__4) AKE(1 : 12)
      REAL(w2f__4) ALF
      REAL(w2f__4) D
      REAL(w2f__4) DEL
      REAL(w2f__4) DELP
      REAL(w2f__4) DFUNC
      REAL(w2f__4) DH
      SAVE DH
      REAL(w2f__4) DL
      SAVE DL
      REAL(w2f__4) DT
      REAL(w2f__4) EPS
      SAVE EPS
      REAL(w2f__4) FR
      SAVE FR
      REAL(w2f__4) FUNCS
      REAL(w2f__4) FUNXON
      INTEGER(w2f__i4) I
      INTEGER(w2f__i4) I1
      INTEGER(w2f__i4) J
      INTEGER(w2f__i4) K
      INTEGER(w2f__i4) KJ(1 : 12)
      INTEGER(w2f__i4) KK
      INTEGER(w2f__i4) LIM
      SAVE LIM
      INTEGER(w2f__i4) LIM2
      SAVE LIM2
      INTEGER(w2f__i4) MB
      INTEGER(w2f__i4) MS
      INTEGER(w2f__i4) NMOPT(1 : 2)
      SAVE NMOPT
      INTEGER(w2f__i4) NMP
      REAL(w2f__4) PTOL
      SAVE PTOL
      REAL(w2f__4) PV
      EXTERNAL pvap
      REAL(w2f__4) PVK
      REAL(w2f__4) PVRES
      REAL(w2f__4) RATIOK
      REAL(w2f__4) SUMD
      REAL(w2f__4) SUMZ
      REAL(w2f__4) T
      REAL(w2f__4) TLIM
      SAVE TLIM
      REAL(w2f__4) TZ
      SAVE TZ
      REAL(w2f__4) VF
      REAL(w2f__4) VFAC
      SAVE VFAC
      REAL(w2f__4) W
      REAL(w2f__4) WTOL
      SAVE WTOL
      REAL(w2f__4) ZF(1 : 12)
      INTEGER(w2f__i4) t__49
      INTEGER(w2f__i4) t__50
      INTEGER(w2f__i4) t__51
      INTEGER(w2f__i4) t__52
      INTEGER(w2f__i4) t__53
      INTEGER(w2f__i4) t__54
      INTEGER(w2f__i4) t__55
      INTEGER(w2f__i4) t__56
      INTEGER(w2f__i4) t__57
      INTEGER(w2f__i4) t__58
      INTEGER(w2f__i4) t__59
      INTEGER(w2f__i4) t__60
      INTEGER(w2f__i4) t__61
      INTEGER(w2f__i4) t__62
C
C     **** Temporary Variables ****
C
      INTEGER(w2f__i8) tmp0
C
C     **** Initializers ****
C
      DATA DH / 3.0E+01 /
      DATA DL / -3.0E+01 /
      DATA EPS / 1.0000000475E-03 /
      DATA FR / 1.9999999553E-02 /
      DATA LIM / 50 /
      DATA LIM2 / 50 /
      DATA(NMOPT(tmp0), tmp0 = 1, 2, 1) / 541868610, 542590276 /
      DATA PTOL / 1.0000000134E-10 /
      DATA TLIM / 5.0E+03 /
      DATA TZ / -4.5967001343E+02 /
      DATA VFAC / 1.0000000149E-01 /
      DATA WTOL / 1.0000000475E-03 /
      integer(w2f__i4) oad_ctmp0
      integer oad_ctmp1
      integer oad_ctmp2
      real(w2f__4) oad_ctmp3
      integer(w2f__i4) oad_ctmp4
      real(w2f__4) oad_ctmp5
      real(w2f__4) oad_ctmp6
      real(w2f__4) oad_ctmp7
      integer(w2f__i4) oad_ctmp8
      real oad_ctmp9
      real oad_ctmp10
      real(w2f__4) oad_ctmp11
      real(w2f__4) oad_ctmp12
      real(w2f__4) oad_ctmp13
      integer(w2f__i4) oad_ctmp14
      real oad_ctmp15
      real oad_ctmp16
      real(w2f__4) oad_ctmp17
      real(w2f__4) oad_ctmp18
C
C     **** Statements ****
C
      IF((WTEN(JW) .LE. WTOL) .OR.(P .LE. PTOL)) GO TO 46
      if ( (TB .gt. TLIM) .or. (TB .lt. TZ) ) then
        TB = 0.0
      ENDIF
      if ( (TD .gt. TLIM) .or. (TD .lt. TZ) ) then
        TD = 0.0
      ENDIF
      AKBARL = 0.0
      t__49 = NCPS
      DO I = 1, t__49, 1
        oad_ctmp0 = (I)
        call pvap(oad_ctmp0,TB,PVRES)
        AKE(I) = (PVRES / P)
45      CONTINUE
        oad_ctmp3 = AKE(I) + 1.0000000134E-10
        call oad_s_LOG(oad_ctmp3,oad_ctmp1)
        call oad_s_LOG(1.0E+01,oad_ctmp2)
        AKBARL = AKBARL + ((oad_ctmp1 * (WEN(JW,I) / WTEN(JW))) / oad_ct
     +mp2)

      END DO
      AKBAR = 1.0E+01 ** AKBARL
      IF(KS .eq. 2) GO TO 50
      t__50 = NCPS
      DO I = 1, t__50, 1
        RATIOK = AKE(I) / AKBAR
        IF((RATIOK .GE. 1.0E+02) .AND.(WEN(JW, I) .GT. 0.0)) GO TO 21
48      CONTINUE
      END DO
55    CONTINUE
      T = TB
      D = WTEN(JW) * P
      MS = 1
      GO TO 10
50    CONTINUE
      t__51 = NCPS
      DO I = 1, t__51, 1
        RATIOK = AKE(I) / AKBAR
        IF((WEN(JW, I) .GT. 0.0) .AND.(RATIOK .LE. 9.9999997765E-03))
     >  GO TO 33
52      CONTINUE
      END DO
      T = TD
      D = WTEN(JW) / P
      MS = 2
10    CONTINUE
      if ( (T .gt. TLIM) .or. (T .lt. TZ) ) then
        T = 0.0
      ENDIF
      MB = 0
      t__52 = LIM
      DO J = 1, t__52, 1
        DEL = D
        t__53 = NCPS
        DO I = 1, t__53, 1
          W = WEN(JW,I)
          IF(W .LE. WTOL) GO TO 2
          oad_ctmp4 = (I)
          call pvap(oad_ctmp4,T,PVRES)
          PV = PVRES
          t__54 = MS
          IF(t__54 .eq. 1) GO TO 51
          IF(t__54 .eq. 2) GO TO 53
51        CONTINUE
          DEL = DEL - PV * W
          GO TO 2
53        CONTINUE
          IF(PTOL .GE. PV) GO TO 8
          DEL = DEL - (W / PV)
2         CONTINUE
        END DO
        DEL = DEL / D
        IF(EPS .GE. ABS(DEL)) GO TO 14
        IF(MB .eq. 0) GO TO 4
        IF(MB .eq. 1) GO TO 5
4       CONTINUE
        MB = 1
        DT = SIGN((FR * (T - TZ)),DEL)
        if ( MS .eq. 2 ) then
          DT = -DT
        ENDIF
        GO TO 7
5       CONTINUE
        DELP = (DELP - DEL) / DEL
        IF(EPS .LT. ABS(DELP)) GO TO 6
        GO TO 4
6       CONTINUE
        DT = DT / DELP
7       CONTINUE
        call oad_s_MIN_r(DH,DT,oad_ctmp6)
        call oad_s_MAX_r(DL,oad_ctmp6,oad_ctmp5)
        DT = oad_ctmp5
        DELP = DEL
        GO TO 11
8       CONTINUE
        DT = DH
        MB = 0
11      CONTINUE
        T = DT + T
      END DO
13    CONTINUE
      NMP = NMOPT(MS)
      WRITE(JOUT, '( 6H0**** ,A3,34H-POINT CALCN HAS NOT CONVERGED ' //
     >  'IN ,I3, 13H ITERATIONS -/6X,22HDATA AND RESULTS ' //
     >  'ARE - / 6X,  3HP = , F10.4, 6H, TB =, F10.4, 6H,' //
     >  ' TD =, F10.4, 6H, DT =,F10.4/  6X,5HDEL =,F12.8,' //
     >  '14H, DEL(PREV.) =,F12.8, 7H, EPS =, F12.8 /  6X,' //
     >  '29HFLOWS (LB MOLS/HR) - TOTAL = ,F12.3/ (3X,6F11' // '.3) )')
     >  NMP, LIM, P, TB, TD, DT, DEL, DELP, EPS, WTEN(JW), (WEN(JW, K),
     >  K = 1, NCPS, 1)
14    CONTINUE
      t__54 = MS
      IF(t__54 .eq. 1) GO TO 15
      IF(t__54 .eq. 2) GO TO 16
15    CONTINUE
      TB = T
      IF(KS .eq. 3) GO TO 50
      GO TO 17
16    CONTINUE
      TD = T
17    CONTINUE
      IF((TB .GT. TLIM) .OR.(TB .LT. TZ)) GO TO 18
      IF((TD .GT. TZ) .AND.(TD .LT. TLIM)) GO TO 19
18    CONTINUE
C     335   WRITE(JOUT,9355) TB,TD
      WRITE(JOUT, '(/,10X,"BUBBLE AND/OR DEW POINT TEMPS OUT OF BOU' //
     >  'NDS",2E20.4)') TB, TD
      STOP
      RETURN
19    CONTINUE
      IF(KFLAG .LE. 1) GO TO 20
      WRITE(JOUT, '(3H AT, F10.5,6H PSIA, )') P
      if ( KS .ne. 2 ) then
C       IF (KS.NE.2) WRITE(JOUT,9331) TB
        WRITE(JOUT, '(1H+,20X,9HBBLE PT = ,F10.5,4H DF, )') TB
      ENDIF
      if ( KS .gt. 1 ) then
C       IF (KS.GT.1) WRITE(JOUT,9332) TD
        WRITE(JOUT, '(1H+,44X,8HDEW PT = ,F10.5)') TD
      ENDIF
20    CONTINUE
      RETURN
21    CONTINUE
      J = 0
      SUMZ = 0.0
      t__55 = NCPS
      DO I = 1, t__55, 1
        RATIOK = AKE(I) / AKBAR
        IF(RATIOK .LT. 1.0E+02) GO TO 22
        J = J + 1
        ZF(J) = (WEN(JW,I) / WTEN(JW))
        KJ(J) = I
        SUMZ = ZF(J) + SUMZ
22      CONTINUE
      END DO
      VF = SUMZ * (VFAC + 1.0)
      call oad_s_MAX_r(VF,1.0000000475E-03,oad_ctmp7)
      VF = oad_ctmp7
      WRITE(JOUT, '(57H0****NON-CONDENSABLES PRESENT,BUBBLE POINT C' //
     >  'AL. WITH VF                                     ' //
     >  '                       = ,F6.3)') VF
      T = TB
      I1 = 1
      t__56 = LIM2
      DO I = 1, t__56, 1
        SUMD = 0.0
        t__57 = NCPS
        DO K = 1, t__57, 1
          t__58 = J
          DO KK = 1, t__58, 1
26          CONTINUE
            IF(KJ(KK) .eq. K) GO TO 24
          END DO
          oad_ctmp8 = (K)
          call pvap(oad_ctmp8,T,PVRES)
          PVK = PVRES / P
          SUMD = SUMD + (((PVK + (-1.0)) * (WEN(JW,K) / WTEN(JW))) / (PV
     +K * VF + 1.0 - VF))

24        CONTINUE
        END DO
        FUNXON = SUMD + 1E00 / (VFAC + 1.0)
        IF(EPS .GT. ABS(FUNXON)) GO TO 15
        IF(I .GT. 1) GO TO 28
        DFUNC = 0.0
        GO TO 29
28      CONTINUE
        DFUNC = DT / (FUNXON - FUNCS)
29      CONTINUE
        oad_ctmp10 = ABS(DFUNC)
        call oad_s_MAX_r(oad_ctmp10,9.9999997765E-03,oad_ctmp9)
        DT = -(FUNXON * SIGN(oad_ctmp9,DFUNC))
        call oad_s_MIN_r(DH,DT,oad_ctmp12)
        call oad_s_MAX_r(DL,oad_ctmp12,oad_ctmp11)
        DT = oad_ctmp11
        IF(EPS .GE. ABS(DT)) GO TO 15
        FUNCS = FUNXON
32      CONTINUE
        IF((TLIM .GT.(DT + T)) .AND.(TZ .LT.(DT + T))) GO TO 30
        DT = DT * 5.0E-01
        GO TO 32
30      CONTINUE
        T = DT + T
      END DO
      MS = 1
      GO TO 13
33    CONTINUE
      IF(KS .eq. 1) GO TO 55
      J = 0
      SUMZ = 0.0
      t__59 = NCPS
      DO I = 1, t__59, 1
        RATIOK = AKE(I) / AKBAR
        IF(RATIOK .GT. 9.9999997765E-03) GO TO 34
        J = J + 1
        ZF(J) = (WEN(JW,I) / WTEN(JW))
        KJ(J) = I
        SUMZ = ZF(J) + SUMZ
34      CONTINUE
      END DO
      ALF = SUMZ * (VFAC + 1.0)
      call oad_s_MAX_r(ALF,1.0000000475E-03,oad_ctmp13)
      ALF = oad_ctmp13
      VF = 1.0 - ALF
      WRITE(JOUT, '(51H0****NON-VOLATILES PRESENT,DEW POINT CAL. WI' //
     >  'TH LF= ,F6.3)') ALF
      T = TD
      I1 = 1
      t__60 = LIM2
      DO I = 1, t__60, 1
        SUMD = 0.0
        t__61 = NCPS
        DO K = 1, t__61, 1
          t__62 = J
          DO KK = 1, t__62, 1
41          CONTINUE
            IF(KJ(KK) .eq. K) GO TO 39
          END DO
          oad_ctmp14 = (K)
          call pvap(oad_ctmp14,T,PVRES)
          PVK = PVRES / P
          SUMD = SUMD + (((WEN(JW,K) / WTEN(JW)) * (PVK + (-1.0))) / (PV
     +K * VF + 1.0 - VF))

39        CONTINUE
        END DO
        FUNXON = SUMD + 1E00 / (VFAC + 1.0)
        IF(EPS .GT. ABS(FUNXON)) GO TO 16
        IF(I .GT. 1) GO TO 36
        DFUNC = 0.0
        GO TO 37
36      CONTINUE
        DFUNC = DT / (FUNXON - FUNCS)
37      CONTINUE
        oad_ctmp16 = ABS(DFUNC)
        call oad_s_MAX_r(oad_ctmp16,9.9999997765E-03,oad_ctmp15)
        DT = -(FUNXON * SIGN(oad_ctmp15,DFUNC))
        call oad_s_MIN_r(DH,DT,oad_ctmp18)
        call oad_s_MAX_r(DL,oad_ctmp18,oad_ctmp17)
        DT = oad_ctmp17
        IF(EPS .GT. ABS(DT)) GO TO 16
        FUNCS = FUNXON
38      CONTINUE
        IF((TLIM .GT.(DT + T)) .AND.(TZ .LT.(DT + T))) GO TO 43
        DT = DT * 5.0E-01
        GO TO 38
43      CONTINUE
        T = DT + T
      END DO
      MS = 2
      GO TO 13
46    CONTINUE
C     500 WRITE(JOUT,9500) KS,JW, P,TB,TD,WTEN(JW)
      WRITE(JOUT, '(32H0**** TBTD HAS BAD DATA --- KS =,I3, 6H, JW ' //
     >  '=, I3 / 6X,    3HP =,F10.4, 6H, TB =,F10.4,6H, T' //
     >  'D =, F10.4,6H, WT =, F12.3 )') KS, JW, P, TB, TD, WTEN(JW)
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE tset(JW, FLAG)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /unpt/
      COMMON /unpt/ JOUT, KNTRL, KFLAG, NCPS, NPTP, NCST, NREC, NEN,
     >  WTEN, WEN, PTPEN, COST, EN, KTLN
      INTEGER(w2f__i4) JOUT
      INTEGER(w2f__i4) KNTRL
      INTEGER(w2f__i4) KFLAG
      INTEGER(w2f__i4) NCPS
      INTEGER(w2f__i4) NPTP
      INTEGER(w2f__i4) NCST
      INTEGER(w2f__i4) NREC
      INTEGER(w2f__i4) NEN
      REAL(w2f__4) WTEN(1 : 5)
      REAL(w2f__4) WEN(1 : 5, 1 : 12)
      REAL(w2f__4) PTPEN(1 : 5, 1 : 6)
      REAL(w2f__4) COST(1 : 5)
      REAL(w2f__4) EN(1 : 100)
      INTEGER(w2f__i4) KTLN(1 : 15)
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) JW
      REAL(w2f__4) FLAG
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) DT
      REAL(w2f__4) DTB
      SAVE DTB
      REAL(w2f__4) DTOL
      SAVE DTOL
      REAL(w2f__4) DTS
      SAVE DTS
      REAL(w2f__4) FLG
      REAL(w2f__4) H(1 : 2)
      REAL(w2f__4) H1
      REAL(w2f__4) H2
      EXTERNAL hmx
      REAL(w2f__4) HS
      INTEGER(w2f__i4) I
      INTEGER(w2f__i4) J
      INTEGER(w2f__i4) KPROPS
      INTEGER(w2f__i4) NLIM
      SAVE NLIM
      REAL(w2f__4) P
      REAL(w2f__4) T(1 : 2)
      EXTERNAL tbtd
      REAL(w2f__4) X
      LOGICAL(w2f__i4) t__66
      INTEGER(w2f__i4) t__67
C
C     **** Initializers ****
C
      DATA DTB / 1.0E+08 /
      DATA DTOL / 9.9999997474E-06 /
      DATA DTS / 1.0E+01 /
      DATA NLIM / 20 /
      integer oad_ctmp0
      real(w2f__4) oad_ctmp1
      real oad_ctmp2
      real(w2f__4) oad_ctmp3
C
C     **** Statements ****
C
      HS = PTPEN(JW,3)
      if ( KFLAG .gt. 1 ) then
C       IF (KFLAG.GT.1) WRITE(JOUT,9000) HS  
        WRITE(JOUT, '(30H FIND TEMP. TO MATCH ENTHY. OF ,F12.3)') HS
      ENDIF
      IF(FLAG .GE. 0.0) GO TO 3
      P = PTPEN(JW,1)
      oad_ctmp0 = (3)
      call tbtd(oad_ctmp0,JW,P,T(1),T(2),H(1),H(2))
      DO J = 1, 2, 1
        IF(KPROPS .GT. 0) GO TO 7
        PTPEN(JW,2) = T(J)
        FLG = FLOAT(J + (-1))
        call hmx(JW,FLG)
        H(J) = PTPEN(JW,3)
7       CONTINUE
        t__66 = (((-1.0) ** J) * (H(J) - HS)) .le. 0.0
        IF(t__66) GO TO 2
8       CONTINUE
      END DO
      X = (HS - H(1)) / (H(2) - H(1))
      IF(DTOL .GT.(T(2) - T(1))) GO TO 1
      DT = X * (T(2) - T(1))
      H2 = H(1)
      PTPEN(JW,2) = (T(1) + DT)
      GO TO 4
2     CONTINUE
      H2 = H(J)
      DT = DTS * ((-1.0) ** J)
      PTPEN(JW,2) = (T(J) + DT)
      if ( J .eq. 1 ) then
        FLG = 0.0
      ENDIF
      if ( J .eq. 2 ) then
        FLG = 1.0
      ENDIF
      GO TO 5
3     CONTINUE
      if ( DTOL .gt. ABS(PTPEN(JW,2)) ) then
        PTPEN(JW,2) = SIGN(DTS,HS)
      ENDIF
      DT = PTPEN(JW,2)
      H2 = 0.0
4     CONTINUE
      FLG = FLAG
5     CONTINUE
      t__67 = NLIM
      DO I = 1, t__67, 1
        call hmx(JW,FLG)
        H1 = PTPEN(JW,3)
        DT = (DT * (HS - H1)) / (H1 - H2)
        oad_ctmp2 = ABS(DT)
        oad_ctmp3 = (DTB ** (1E00 / (FLOAT(I))))
        call oad_s_MIN_r(oad_ctmp2,oad_ctmp3,oad_ctmp1)
        DT = SIGN(oad_ctmp1,DT)
        IF(KFLAG .LE. 1) GO TO 6
        WRITE(JOUT, '(5H ITER,I4,3X,4HH = ,F12.3,6H, T = ,F12.3,     '
     >  // '            12H, NEXT DT = ,F12.5 )') I, H1, PTPEN(JW, 2),
     >  DT
6       CONTINUE
        H2 = H1
        PTPEN(JW,2) = (PTPEN(JW,2) + DT)
        IF(DTOL .GE. ABS(DT)) GO TO 12
9       CONTINUE
      END DO
      WRITE(JOUT, '(  6H0AFTER, I8,25H ITERATIONS, TEMP DIFF IS,  G' //
     >  '11.5,        9H DEG FAHR )') NLIM, DT
12    CONTINUE
      PTPEN(JW,3) = HS
      GO TO 13
1     CONTINUE
      PTPEN(JW,3) = HS
      PTPEN(JW,4) = X
13    CONTINUE
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE vmgas(FV, T, P, GASRES)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /qp/
      COMMON /qp/ CSXX, ANAMC
      REAL(w2f__4) CSXX(1 : 12, 1 : 28)
      REAL(w2f__4) ANAMC(1 : 12, 1 : 6)
      SAVE /unpt/
      COMMON /unpt/ JOUT, KNTRL, KFLAG, NCP, NPTP, NCST, NREC, NEN,
     >  WTEN, WEN, PTPEN, COST, EN, KTLN
      INTEGER(w2f__i4) JOUT
      INTEGER(w2f__i4) KNTRL
      INTEGER(w2f__i4) KFLAG
      INTEGER(w2f__i4) NCP
      INTEGER(w2f__i4) NPTP
      INTEGER(w2f__i4) NCST
      INTEGER(w2f__i4) NREC
      INTEGER(w2f__i4) NEN
      REAL(w2f__4) WTEN(1 : 5)
      REAL(w2f__4) WEN(1 : 5, 1 : 12)
      REAL(w2f__4) PTPEN(1 : 5, 1 : 6)
      REAL(w2f__4) COST(1 : 5)
      REAL(w2f__4) EN(1 : 100)
      INTEGER(w2f__i4) KTLN(1 : 15)
C
C     **** Parameters and Result ****
C
      REAL(w2f__4) FV(1 : 12)
      REAL(w2f__4) T
      REAL(w2f__4) P
      REAL(w2f__4) GASRES
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) BZ
      INTEGER(w2f__i4) I
      REAL(w2f__4) SUM
      REAL(w2f__4) TA
      REAL(w2f__4) TC
      REAL(w2f__4) VC
      REAL(w2f__4) ZA
      REAL(w2f__4) ZB
      REAL(w2f__4) ZC
      INTEGER(w2f__i4) t__68
      real(w2f__4) oad_ctmp0
C
C     **** Statements ****
C
      SUM = 0.0
      TC = 0.0
      VC = 0.0
      ZC = 0.0
      TA = T + 4.5967001343E+02
      t__68 = NCP
      DO I = 1, t__68, 1
        SUM = FV(I) + SUM
        TC = TC + FV(I) * CSXX(I,1)
        VC = VC + ((FV(I) * CSXX(I,22)) / 6.2430000305E+01)
        ZC = ZC + FV(I) * CSXX(I,21)
1       CONTINUE
      END DO
      TC = TC / SUM
      VC = VC / SUM
      ZC = ZC / SUM
      VC = (ZC * TC * 1.0733499527E+01) / VC
      TC = TA / TC
      SUM = P / VC
      ZA = 1.0 - EXP(TC * (-4.0)) * SUM * 1.9379999161E+01
      BZ = -4.0927999653E-03
      ZB = BZ * (SUM ** 2) + SUM * 1.5401999652E-01 + (BZ / 1.0E+01)
      call oad_s_MAX_r(ZA,ZB,oad_ctmp0)
      ZC = oad_ctmp0
      GASRES = (ZC * TA * 1.0733499527E+01) / P
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE vmliq(FL, T, LIQRES)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /qp/
      COMMON /qp/ CSXX, ANAMC
      REAL(w2f__4) CSXX(1 : 12, 1 : 28)
      REAL(w2f__4) ANAMC(1 : 12, 1 : 6)
      SAVE /unpt/
      COMMON /unpt/ JOUT, KNTRL, KFLAG, NCP, NPTP, NCST, NREC, NEN,
     >  WTEN, WEN, PTPEN, COST, EN, KTLN
      INTEGER(w2f__i4) JOUT
      INTEGER(w2f__i4) KNTRL
      INTEGER(w2f__i4) KFLAG
      INTEGER(w2f__i4) NCP
      INTEGER(w2f__i4) NPTP
      INTEGER(w2f__i4) NCST
      INTEGER(w2f__i4) NREC
      INTEGER(w2f__i4) NEN
      REAL(w2f__4) WTEN(1 : 5)
      REAL(w2f__4) WEN(1 : 5, 1 : 12)
      REAL(w2f__4) PTPEN(1 : 5, 1 : 6)
      REAL(w2f__4) COST(1 : 5)
      REAL(w2f__4) EN(1 : 100)
      INTEGER(w2f__i4) KTLN(1 : 15)
C
C     **** Parameters and Result ****
C
      REAL(w2f__4) FL(1 : 12)
      REAL(w2f__4) T
      REAL(w2f__4) LIQRES
C
C     **** Local Variables and Functions ****
C
      REAL(w2f__4) DLI
      INTEGER(w2f__i4) I
      REAL(w2f__4) SUM
      REAL(w2f__4) TS
      INTEGER(w2f__i4) t__69
      real(w2f__4) oad_ctmp0
      real(w2f__4) oad_ctmp1
C
C     **** Statements ****
C
      TS = T / 1.0E+03
      SUM = 0.0
      LIQRES = 0.0
      t__69 = NCP
      DO I = 1, t__69, 1
        oad_ctmp1 = (CSXX(I,16) + TS * (CSXX(I,17) + CSXX(I,18) * TS))
        call oad_s_MAX_r(oad_ctmp1,1.0,oad_ctmp0)
        DLI = oad_ctmp0
        LIQRES = LIQRES + ((FL(I) * CSXX(I,6)) / DLI)
1       CONTINUE
        SUM = FL(I) + SUM
      END DO
      LIQRES = LIQRES / SUM
      RETURN
      RETURN
      END SUBROUTINE

      SUBROUTINE wtmol(JW)
      use w2f__types
      IMPLICIT NONE
C
C     **** Global Variables & Derived Type Definitions ****
C
      SAVE /qp/
      COMMON /qp/ CSXX, ANAMC
      REAL(w2f__4) CSXX(1 : 12, 1 : 28)
      REAL(w2f__4) ANAMC(1 : 12, 1 : 6)
      SAVE /unpt/
      COMMON /unpt/ JOUT, KNTRL, KFLAG, NCPS, NPTP, NCST, NREC, NEN,
     >  WTEN, WEN, PTPEN, COST, EN, KTLN
      INTEGER(w2f__i4) JOUT
      INTEGER(w2f__i4) KNTRL
      INTEGER(w2f__i4) KFLAG
      INTEGER(w2f__i4) NCPS
      INTEGER(w2f__i4) NPTP
      INTEGER(w2f__i4) NCST
      INTEGER(w2f__i4) NREC
      INTEGER(w2f__i4) NEN
      REAL(w2f__4) WTEN(1 : 5)
      REAL(w2f__4) WEN(1 : 5, 1 : 12)
      REAL(w2f__4) PTPEN(1 : 5, 1 : 6)
      REAL(w2f__4) COST(1 : 5)
      REAL(w2f__4) EN(1 : 100)
      INTEGER(w2f__i4) KTLN(1 : 15)
C
C     **** Parameters and Result ****
C
      INTEGER(w2f__i4) JW
C
C     **** Local Variables and Functions ****
C
      INTEGER(w2f__i4) I
      LOGICAL(w2f__i4) MDO
      REAL(w2f__4) WM
      INTEGER(w2f__i4) t__70
C
C     **** Statements ****
C
      IF(WTEN(JW) .LE. 1.0000000134E-10) GO TO 1
      MDO = .false.
      GO TO 2
1     CONTINUE
      MDO = .true.
      WTEN(JW) = 0.0
2     CONTINUE
      WM = 0.0
      t__70 = NCPS
      DO I = 1, t__70, 1
        if ( MDO ) then
          WTEN(JW) = (WTEN(JW) + WEN(JW,I))
        ENDIF
        WM = WM + CSXX(I,6) * WEN(JW,I)
3       CONTINUE
      END DO
      IF(WTEN(JW) .LE. 0.0) GO TO 6
      PTPEN(JW,6) = (WM / WTEN(JW))
5     CONTINUE
      RETURN
6     CONTINUE
      PTPEN(JW,6) = 0.0
      GO TO 5
      RETURN
      END SUBROUTINE
