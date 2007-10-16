C ***********************************************************
C Fortran file translated from WHIRL Wed Aug 23 01:41:43 2006
C ***********************************************************
C ***********************************************************

      PROGRAM flash
      use w2f__types
      use active_module
      IMPLICIT NONE
C
C     **** Local Variables and Functions ****
C
      EXTERNAL aifl
      REAL(w2f__4) LIQUID(1 : 6)
      INTEGER(w2f__i4) OAD_CTMP0
      REAL(w2f__4) PIPE(1 : 6)
      REAL(w2f__4) PRESSURE(1 : 5)
      REAL(w2f__4) VAPOR(1 : 6)
C
C     **** Statements ****
C
1     CONTINUE
      GO TO 2
2     CONTINUE
      OAD_CTMP0 = 1
      CALL aifl(OAD_CTMP0, PIPE, PRESSURE, VAPOR, LIQUID)
      
      GO TO 3
3     CONTINUE
      END PROGRAM

      SUBROUTINE aifl(KF, PIPE, PRESSURE, VAPOR, LIQUID)
      use w2f__types
      use active_module
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
      
      INTEGER(w2f__i4) NT(1 : 2, 1 : 2)
      INTEGER(w2f__i1) TMP0(1 : 16)
      INTEGER(w2f__i4) T__8(1 : 4)
      EQUIVALENCE(tmp0(0), NT)
      EQUIVALENCE(tmp0(0), TMP0)
      EQUIVALENCE(tmp0(0), T__8)
C      INTEGER(w2f__i4) NT(1 : 2, 1 : 2)
      SAVE NT
      INTEGER(w2f__i4) NUM
      INTEGER(w2f__i4) OAD_CTMP0
      REAL(w2f__4) OAD_CTMP1
      REAL(w2f__4) OAD_CTMP10
      REAL(w2f__4) OAD_CTMP11
      INTEGER(w2f__i4) OAD_CTMP12
      INTEGER(w2f__i4) OAD_CTMP13
      INTEGER(w2f__i4) OAD_CTMP2
      REAL(w2f__4) OAD_CTMP3
      INTEGER(w2f__i4) OAD_CTMP4
      INTEGER(w2f__i4) OAD_CTMP5
      REAL(w2f__4) OAD_CTMP6
      REAL(w2f__4) OAD_CTMP7
      REAL(w2f__4) OAD_CTMP8
      INTEGER(w2f__i4) OAD_CTMP9
      EXTERNAL oad_s_ifix
      EXTERNAL oad_s_max_r
      EXTERNAL oad_s_pipe
      EXTERNAL oad_s_pressure
C      INTEGER(w2f__i1) TMP0(1 : 16)
      SAVE TMP0
      EXTERNAL tset
      INTEGER(w2f__i4) T__10
      INTEGER(w2f__i4) T__11
      INTEGER(w2f__i4) T__12
      INTEGER(w2f__i4) T__13
      INTEGER(w2f__i4) T__14
      INTEGER(w2f__i4) T__15
      INTEGER(w2f__i4) T__16
      INTEGER(w2f__i4) T__17
      INTEGER(w2f__i4) T__18
      INTEGER(w2f__i4) T__19
      INTEGER(w2f__i4) T__20
      INTEGER(w2f__i4) T__21
      INTEGER(w2f__i4) T__22
      INTEGER(w2f__i4) T__23
C      INTEGER(w2f__i4) T__8(1 : 4)
      SAVE T__8
      INTEGER(w2f__i4) T__9
      EXTERNAL wtmol
      REAL(w2f__4) WTOL
      SAVE WTOL
      INTEGER(w2f__i4) t__2
      INTEGER(w2f__i4) t__3
      INTEGER(w2f__i4) t__4
      INTEGER(w2f__i4) t__5
      INTEGER(w2f__i4) t__6
      INTEGER(w2f__i4) t__7
      INTEGER(w2f__i4) t__24
      INTEGER(w2f__i4) t__25
      INTEGER(w2f__i4) t__26
      INTEGER(w2f__i4) t__27
      INTEGER(w2f__i4) t__28
      INTEGER(w2f__i4) t__29
      INTEGER(w2f__i4) t__30
      INTEGER(w2f__i4) t__31
C
C     **** Temporary Variables ****
C
C      INTEGER(w2f__i1) tmp0(1 : 16)
      INTEGER(w2f__i8) tmp1
C
C     **** Initializers ****
C
      DATA HTOL / 1.0000000475E-03 /
      DATA(T__8(tmp1), tmp1 = 1, 4, 1) / 541672513, 539902529,
     >  542069577, 539904084 /
      DATA WTOL / 9.9999997474E-06 /
C
C     **** Top Level Pragmas ****
C
C$OPENAD INDEPENDENT(PIPE)
C$OPENAD DEPENDENT(VAPOR)
C$OPENAD DEPENDENT(LIQUID)
C
C     **** Statements ****
C
4     CONTINUE
      GO TO 5
5     CONTINUE
      CALL oad_s_ifix(EN(1), OAD_CTMP0)
      NIN = OAD_CTMP0
      NOUT = 2
      NUM = NIN + NOUT
      T__9 = NOUT1
      T__10 = NUM
      t__2 = T__9
      t__3 = T__10
      GO TO 6
6     CONTINUE
      J = t__2
      GO TO 110
7     CONTINUE
      J = J + 1
110   CONTINUE
      IF(J .LE. t__3) THEN
        GO TO 104
      ELSE
        GO TO 8
      ENDIF
8     CONTINUE
      CALL oad_s_ifix(EN(1), OAD_CTMP2)
      NIN = OAD_CTMP2
      NOUT = 2
      NUM = NIN + NOUT
      WTEN(1) = 0.0D00
      EN(19) = 0.0D00
      NOUT1 = NOUT + 1
      T__12 = NUM
      t__5 = T__12
      GO TO 9
9     CONTINUE
      II = 1
      GO TO 111
10    CONTINUE
      II = II + 1
111   CONTINUE
      IF(II .LE. t__5) THEN
        GO TO 102
      ELSE
        GO TO 11
      ENDIF
11    CONTINUE
      T__13 = NOUT1
      T__14 = NUM
      t__6 = T__13
      t__7 = T__14
      GO TO 12
12    CONTINUE
      J = t__6
      GO TO 112
13    CONTINUE
      J = J + 1
112   CONTINUE
      IF(J .LE. t__7) THEN
        GO TO 84
      ELSE
        GO TO 14
      ENDIF
14    CONTINUE
      WRITE(JOUT, '(2x,"aifl:",F10.4)')(PTPEN(J, 3), J = NOUT1, NUM, 1)
      PTPEN(INT(NOUT1), 1) = EN(2)
      PTPEN(INT(NOUT1), 2) = EN(3)
      GO TO 15
15    CONTINUE
      IF(DBLE(WTEN(1)) .LE. DBLE(WTOL)) THEN
        GO TO 63
      ELSE
        GO TO 16
      ENDIF
16    CONTINUE
      GO TO 17
17    CONTINUE
      IF(NOUT1 .GE. NUM) THEN
        GO TO 28
      ELSE
        GO TO 18
      ENDIF
18    CONTINUE
      GO TO 19
19    CONTINUE
      T__15 = NOUT1 + 1
      T__16 = NUM
      t__24 = T__15
      t__25 = T__16
      GO TO 20
20    CONTINUE
      J = t__24
      GO TO 113
21    CONTINUE
      J = J + 1
113   CONTINUE
      IF(J .LE. t__25) THEN
        GO TO 22
      ELSE
        GO TO 28
      ENDIF
22    CONTINUE
      T__17 = NCPS
      t__26 = T__17
      GO TO 23
23    CONTINUE
      I = 1
      GO TO 114
24    CONTINUE
      I = I + 1
114   CONTINUE
      IF(I .LE. t__26) THEN
        GO TO 26
      ELSE
        GO TO 25
      ENDIF
25    CONTINUE
      GO TO 21
26    CONTINUE
      WEN(INT(NOUT1), INT(I)) = (DBLE(WEN(J, I)) + DBLE(WEN(NOUT1, I)))
      GO TO 27
27    CONTINUE
      GO TO 24
28    CONTINUE
      WTEN(INT(NOUT1)) = 0.0D00
      CALL wtmol(NOUT1)
      GO TO 29
29    CONTINUE
      IF(DBLE(WTOL) .LT. ABS(DBLE(WTEN(NOUT1)) - DBLE(WTEN(1)))) THEN
        GO TO 31
      ELSE
        GO TO 30
      ENDIF
30    CONTINUE
      GO TO 32
31    CONTINUE
      WRITE(JOUT, '(54H0**** COMPONENT FLOW TOTAL DIFFERS FROM TOTA' //
     >  'L FLOW ***)')
      GO TO 32
32    CONTINUE
      GO TO 33
33    CONTINUE
      PTPEN(INT(NOUT1), 3) = (DBLE(EN(19)) / DBLE(WTEN(NOUT1)))
      GO TO 34
34    CONTINUE
      IF(KFLAG .LE. INT(1_w2f__i8)) THEN
        GO TO 42
      ELSE
        GO TO 35
      ENDIF
35    CONTINUE
      GO TO 36
36    CONTINUE
      WRITE(JOUT, '(1X,2A3,10HFLASH, AT ,F10.4,10H PSIA, AND ,F10.4' //
     >  ',3H DF )')(NT(K, KF), K = 1, 2, 1), (PTPEN(NOUT1, J), J = 1, 2
     > , 1)
      GO TO 37
37    CONTINUE
      IF(KF .LE. INT(1_w2f__i8)) THEN
        GO TO 39
      ELSE
        GO TO 38
      ENDIF
38    CONTINUE
      GO TO 40
39    CONTINUE
      WRITE(JOUT, '(1H+,50X,16H(STARTING TRIAL) )')
      GO TO 40
40    CONTINUE
      GO TO 41
41    CONTINUE
      WRITE(JOUT, '(2X,17HCOMBINED FEEDS -- /5X,F12.3,12H BTU/LB MO' //
     >  'L ,5X,  F12.4,11H LB MOLS/HR / 5X,17HCOMPONENT F' //
     >  'LOWS - / (5X,4F12.4))') PTPEN(NOUT1, 3), WTEN(NOUT1), (WEN(
     > NOUT1, I), I = 1, NCPS, 1)
      GO TO 42
42    CONTINUE
      OAD_CTMP7 = (-1.0D00)
      CALL tset(NOUT1, OAD_CTMP7)
      T__18 = KF
      GO TO 43
43    CONTINUE
      IF(T__18 .eq. INT(1_w2f__i8)) THEN
        GO TO 48
      ELSE
        GO TO 44
      ENDIF
44    CONTINUE
      GO TO 45
45    CONTINUE
      IF(T__18 .eq. INT(2_w2f__i8)) THEN
        GO TO 47
      ELSE
        GO TO 46
      ENDIF
46    CONTINUE
      GO TO 47
47    CONTINUE
      EN(20) = PTPEN(NOUT1, 2)
      PTPEN(INT(NOUT1), 2) = EN(3)
      OAD_CTMP8 = (-1.0D00)
      CALL hmx(NOUT1, OAD_CTMP8)
      GO TO 48
48    CONTINUE
      EN(18) = PTPEN(NOUT1, 4)
      T__19 = NCPS
      t__27 = T__19
      GO TO 49
49    CONTINUE
      I = 1
      GO TO 115
50    CONTINUE
      I = I + 1
115   CONTINUE
      IF(I .LE. t__27) THEN
        GO TO 57
      ELSE
        GO TO 51
      ENDIF
51    CONTINUE
      T__20 = NOUT
      t__28 = T__20
      GO TO 52
52    CONTINUE
      J = 1
      GO TO 116
53    CONTINUE
      J = J + 1
116   CONTINUE
      IF(J .LE. t__28) THEN
        GO TO 55
      ELSE
        GO TO 54
      ENDIF
54    CONTINUE
      GO TO 77
55    CONTINUE
      PTPEN(INT(J), 1) = PTPEN(NOUT1, 1)
      PTPEN(INT(J), 2) = PTPEN(NOUT1, 2)
      WTEN(INT(J)) = 0.0D00
      OAD_CTMP9 = J
      CALL wtmol(OAD_CTMP9)
      OAD_CTMP11 = FLOAT(INT(2_w2f__i8) - J)
      CALL oad_s_max_r(OAD_CTMP11, 0.0D00, OAD_CTMP10)
      FLAG = OAD_CTMP10
      OAD_CTMP12 = J
      CALL hmx(OAD_CTMP12, FLAG)
      OAD_CTMP13 = J
      CALL dnsty(OAD_CTMP13, FLAG)
      GO TO 56
56    CONTINUE
      GO TO 53
57    CONTINUE
      WEN(1, INT(I)) = FV(I)
      WEN(2, INT(I)) = FL(I)
      GO TO 58
58    CONTINUE
      IF(NOUT .GE. INT(3_w2f__i8)) THEN
        GO TO 60
      ELSE
        GO TO 59
      ENDIF
59    CONTINUE
      GO TO 61
60    CONTINUE
      WEN(3, INT(I)) = FW(I)
      GO TO 61
61    CONTINUE
      GO TO 62
62    CONTINUE
      GO TO 50
63    CONTINUE
      WRITE(JOUT, '(27H0**** COMBINED FEED FLOW LT , E12.3,5H **** ' //
     >  ')') WTOL
      T__21 = NOUT
      t__29 = T__21
      GO TO 64
64    CONTINUE
      J = 1
      GO TO 117
65    CONTINUE
      J = J + 1
117   CONTINUE
      IF(J .LE. t__29) THEN
        GO TO 66
      ELSE
        GO TO 77
      ENDIF
66    CONTINUE
      PTPEN(INT(J), 1) = PTPEN(NOUT1, 1)
      PTPEN(INT(J), 2) = PTPEN(NOUT1, 2)
      WTEN(INT(J)) = 0.0D00
      T__22 = NCPS
      t__30 = T__22
      GO TO 67
67    CONTINUE
      I = 1
      GO TO 118
68    CONTINUE
      I = I + 1
118   CONTINUE
      IF(I .LE. t__30) THEN
        GO TO 75
      ELSE
        GO TO 69
      ENDIF
69    CONTINUE
      GO TO 70
70    CONTINUE
      I = 3
      GO TO 119
71    CONTINUE
      I = I + 1
119   CONTINUE
      IF(I .LE. 6) THEN
        GO TO 73
      ELSE
        GO TO 72
      ENDIF
72    CONTINUE
      GO TO 65
73    CONTINUE
      PTPEN(INT(J), INT(I)) = 0.0D00
      GO TO 74
74    CONTINUE
      GO TO 71
75    CONTINUE
      WEN(INT(J), INT(I)) = 0.0D00
      GO TO 76
76    CONTINUE
      GO TO 68
77    CONTINUE
      T__23 = NCPS
      t__31 = T__23
      GO TO 78
78    CONTINUE
      II = 1
      GO TO 120
79    CONTINUE
      II = II + 1
120   CONTINUE
      IF(II .LE. t__31) THEN
        GO TO 82
      ELSE
        GO TO 80
      ENDIF
80    CONTINUE
      RETURN
      GO TO 81
81    CONTINUE
82    CONTINUE
      VAPOR(INT(II)) = WEN(1, II)
      LIQUID(INT(II)) = WEN(2, II)
      GO TO 83
83    CONTINUE
      GO TO 79
84    CONTINUE
      IF(DBLE(EN(2)) .GT. DBLE(PTPEN(J, 1))) THEN
        GO TO 86
      ELSE
        GO TO 85
      ENDIF
85    CONTINUE
      GO TO 87
86    CONTINUE
      WRITE(JOUT, 8110) J
      GO TO 87
87    CONTINUE
      GO TO 88
88    CONTINUE
      IF(DBLE(WTEN(J)) .LE. DBLE(WTOL)) THEN
        GO TO 90
      ELSE
        GO TO 89
      ENDIF
89    CONTINUE
      GO TO 91
90    CONTINUE
      OAD_CTMP4 = J
      CALL wtmol(OAD_CTMP4)
      GO TO 91
91    CONTINUE
      GO TO 92
92    CONTINUE
      IF(KPROPS .LE. INT(0_w2f__i8)) THEN
        GO TO 95
      ELSE
        GO TO 93
      ENDIF
93    CONTINUE
      GO TO 94
94    CONTINUE
      GO TO 100
95    CONTINUE
      GO TO 96
96    CONTINUE
      IF(DBLE(PTPEN(J, 3)) .LE. DBLE(HTOL)) THEN
        GO TO 98
      ELSE
        GO TO 97
      ENDIF
97    CONTINUE
      GO TO 99
98    CONTINUE
      OAD_CTMP5 = J
      OAD_CTMP6 = (-1.0D00)
      CALL hmx(OAD_CTMP5, OAD_CTMP6)
      GO TO 99
99    CONTINUE
      GO TO 100
100   CONTINUE
      WTEN(1) = (DBLE(WTEN(J)) + DBLE(WTEN(1)))
      EN(19) = (DBLE(EN(19)) + DBLE(WTEN(J)) * DBLE(PTPEN(J, 3)))
      GO TO 101
101   CONTINUE
      GO TO 13
102   CONTINUE
      CALL oad_s_pressure(II, OAD_CTMP3)
      PTPEN(INT(II), 1) = OAD_CTMP3
      GO TO 103
103   CONTINUE
      GO TO 10
104   CONTINUE
      T__11 = NCPS
      t__4 = T__11
      GO TO 105
105   CONTINUE
      II = 1
      GO TO 121
106   CONTINUE
      II = II + 1
121   CONTINUE
      IF(II .LE. t__4) THEN
        GO TO 108
      ELSE
        GO TO 107
      ENDIF
107   CONTINUE
      GO TO 7
108   CONTINUE
      CALL oad_s_pipe(II, OAD_CTMP1)
      WEN(INT(J), INT(II)) = OAD_CTMP1
      WTEN(INT(J)) = (DBLE(WTEN(J)) + DBLE(WEN(J, II)))
      GO TO 109
109   CONTINUE
      GO TO 106
 8110 FORMAT (23H0**** PRESSURE AT INLET, I3,30H LESS THAN FLASH PRESSUR
     1E **** )
 8130 FORMAT (54H0**** COMPONENT FLOW TOTAL DIFFERS FROM TOTAL FLOW ***)
 8180 FORMAT (27H0**** COMBINED FEED FLOW LT , E12.3,5H **** )
 9130 FORMAT (1X,2A3,10HFLASH, AT ,F10.4,10H PSIA, AND ,F10.4,3H DF )
 9131 FORMAT (1H+,50X,16H(STARTING TRIAL) )
 9132 FORMAT (2X,17HCOMBINED FEEDS -- /5X,F12.3,12H BTU/LB MOL ,5X,
     1  F12.4,11H LB MOLS/HR / 5X,17HCOMPONENT FLOWS - / (5X,4F12.4))
      END SUBROUTINE

      SUBROUTINE dnsty(JW, FLAG)
      use w2f__types
      use active_module
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
      INTEGER(w2f__i4) OAD_CTMP0
      REAL(w2f__4) OAD_CTMP1
      INTEGER(w2f__i4) OAD_CTMP2
      INTEGER(w2f__i4) OAD_CTMP3
      EXTERNAL oad_s_ifix
      EXTERNAL oad_s_max_i
      EXTERNAL oad_s_min_i
      INTEGER(w2f__i4) T__24
      INTEGER(w2f__i4) T__25
      INTEGER(w2f__i4) T__26
      REAL(w2f__4) VM
      EXTERNAL vmgas
      EXTERNAL vmliq
      REAL(w2f__4) VTOL
      SAVE VTOL
      EXTERNAL wtmol
      REAL(w2f__4) WTOL
      SAVE WTOL
      INTEGER(w2f__i4) t__32
      INTEGER(w2f__i4) t__33
C
C     **** Initializers ****
C
      DATA VTOL / 9.9999999748E-07 /
      DATA WTOL / 1.0000000134E-10 /
C
C     **** Statements ****
C
258   CONTINUE
      GO TO 259
259   CONTINUE
      GO TO 260
260   CONTINUE
      IF((DBLE(WTEN(JW)) .GT. DBLE(WTOL)) .AND.(DBLE(PTPEN(JW, 6)) .GT.
     >  DBLE(WTOL))) THEN
        GO TO 266
      ELSE
        GO TO 261
      ENDIF
261   CONTINUE
      GO TO 262
262   CONTINUE
      CALL wtmol(JW)
      GO TO 263
263   CONTINUE
      IF(DBLE(WTEN(JW)) .GT. DBLE(WTOL)) THEN
        GO TO 266
      ELSE
        GO TO 264
      ENDIF
264   CONTINUE
      GO TO 265
265   CONTINUE
      PTPEN(INT(JW), 5) = 0.0D00
      GO TO 289
266   CONTINUE
      OAD_CTMP1 = (DBLE(FLAG) + 2.09999990460000018899D00)
      CALL oad_s_ifix(OAD_CTMP1, OAD_CTMP0)
      KLAG = OAD_CTMP0
      CALL oad_s_max_i(KLAG, 1_w2f__i8, OAD_CTMP3)
      CALL oad_s_min_i(OAD_CTMP3, 3_w2f__i8, OAD_CTMP2)
      KLAG = OAD_CTMP2
      T__24 = KLAG
      GO TO 267
267   CONTINUE
      IF(T__24 .eq. INT(1_w2f__i8)) THEN
        GO TO 273
      ELSE
        GO TO 268
      ENDIF
268   CONTINUE
      GO TO 269
269   CONTINUE
      IF(T__24 .eq. INT(2_w2f__i8)) THEN
        GO TO 285
      ELSE
        GO TO 270
      ENDIF
270   CONTINUE
      GO TO 271
271   CONTINUE
      IF(T__24 .eq. INT(3_w2f__i8)) THEN
        GO TO 279
      ELSE
        GO TO 272
      ENDIF
272   CONTINUE
      GO TO 273
273   CONTINUE
      CALL iflash(JW)
      GO TO 274
274   CONTINUE
      IF(DBLE(PTPEN(JW, 4)) .LT. DBLE(VTOL)) THEN
        GO TO 285
      ELSE
        GO TO 275
      ENDIF
275   CONTINUE
      GO TO 276
276   CONTINUE
      IF(DBLE(PTPEN(JW, 4)) .GT.(1.0D00 - DBLE(VTOL))) THEN
        GO TO 279
      ELSE
        GO TO 277
      ENDIF
277   CONTINUE
      GO TO 278
278   CONTINUE
      CALL vmgas(FV, PTPEN(JW, 2), PTPEN(JW, 1), GASRES)
      CALL vmliq(FL, PTPEN(JW, 2), LIQRES)
      VM = (DBLE(PTPEN(JW, 4)) * DBLE(GASRES) + DBLE(LIQRES) *(1.0D00 -
     >  DBLE(PTPEN(JW, 4))))
      PTPEN(INT(JW), 5) = (DBLE(PTPEN(JW, 6)) / DBLE(VM))
      GO TO 289
279   CONTINUE
      T__26 = NCPS
      t__33 = T__26
      GO TO 280
280   CONTINUE
      I = 1
      GO TO 293
281   CONTINUE
      I = I + 1
293   CONTINUE
      IF(I .LE. t__33) THEN
        GO TO 283
      ELSE
        GO TO 282
      ENDIF
282   CONTINUE
      CALL vmgas(FV, PTPEN(JW, 2), PTPEN(JW, 1), GASRES)
      PTPEN(INT(JW), 5) = (DBLE(PTPEN(JW, 6)) / DBLE(GASRES))
      GO TO 289
283   CONTINUE
      FV(INT(I)) = WEN(JW, I)
      GO TO 284
284   CONTINUE
      GO TO 281
285   CONTINUE
      T__25 = NCPS
      t__32 = T__25
      GO TO 286
286   CONTINUE
      I = 1
      GO TO 294
287   CONTINUE
      I = I + 1
294   CONTINUE
      IF(I .LE. t__32) THEN
        GO TO 291
      ELSE
        GO TO 288
      ENDIF
288   CONTINUE
      CALL vmliq(FL, PTPEN(JW, 2), LIQRES)
      PTPEN(INT(JW), 5) = (DBLE(PTPEN(JW, 6)) / DBLE(LIQRES))
      GO TO 289
289   CONTINUE
      RETURN
      GO TO 290
290   CONTINUE
291   CONTINUE
      FL(INT(I)) = WEN(JW, I)
      GO TO 292
292   CONTINUE
      GO TO 287
      END SUBROUTINE

      SUBROUTINE hdel(I, T, HDRES)
      use w2f__types
      use active_module
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
408   CONTINUE
      GO TO 409
409   CONTINUE
      TBOILI = (DBLE(CSXX(I, 8)) + 4.59670013429999983146D+02)
      TABS = (DBLE(T) + 4.59670013429999983146D+02)
      A = ((DBLE(CSXX(I, 1)) - DBLE(TABS)) /(DBLE(CSXX(I, 1)) - DBLE(
     > TBOILI)))
      GO TO 410
410   CONTINUE
      IF(DBLE(A) .LE. 0.0D00) THEN
        GO TO 412
      ELSE
        GO TO 411
      ENDIF
411   CONTINUE
      GO TO 413
412   CONTINUE
      A = 9.99999968270000025091D-21
      GO TO 413
413   CONTINUE
      GO TO 414
414   CONTINUE
      HDRES = (DBLE(CSXX(I, 19)) *(DBLE(A) ** DBLE(CSXX(I, 20))))
      RETURN
      GO TO 415
415   CONTINUE
      END SUBROUTINE

      SUBROUTINE hmx(JW, FLAG)
      use w2f__types
      use active_module
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
      INTEGER(w2f__i4) OAD_CTMP0
      REAL(w2f__4) OAD_CTMP1
      INTEGER(w2f__i4) OAD_CTMP2
      INTEGER(w2f__i4) OAD_CTMP3
      INTEGER(w2f__i4) OAD_CTMP4
      INTEGER(w2f__i4) OAD_CTMP5
      EXTERNAL oad_s_ifix
      EXTERNAL oad_s_max_i
      EXTERNAL oad_s_min_i
      REAL(w2f__4) T
      INTEGER(w2f__i4) T__27
      INTEGER(w2f__i4) T__28
      INTEGER(w2f__i4) T__29
      INTEGER(w2f__i4) T__30
      REAL(w2f__4) VZHI
      SAVE VZHI
      REAL(w2f__4) VZLO
      SAVE VZLO
      EXTERNAL wtmol
      REAL(w2f__4) WTOL
      SAVE WTOL
      INTEGER(w2f__i4) t__34
      INTEGER(w2f__i4) t__35
      INTEGER(w2f__i4) t__36
C
C     **** Initializers ****
C
      DATA VZHI / 9.9999898672E-01 /
      DATA VZLO / 9.9999999748E-07 /
      DATA WTOL / 1.0000000134E-10 /
C
C     **** Statements ****
C
144   CONTINUE
      GO TO 145
145   CONTINUE
      GO TO 146
146   CONTINUE
      IF(DBLE(WTEN(JW)) .GT. DBLE(WTOL)) THEN
        GO TO 152
      ELSE
        GO TO 147
      ENDIF
147   CONTINUE
      GO TO 148
148   CONTINUE
      CALL wtmol(JW)
      GO TO 149
149   CONTINUE
      IF(DBLE(WTEN(JW)) .GT. DBLE(WTOL)) THEN
        GO TO 152
      ELSE
        GO TO 150
      ENDIF
150   CONTINUE
      GO TO 151
151   CONTINUE
      PTPEN(INT(JW), 3) = 0.0D00
      PTPEN(INT(JW), 4) = 0.0D00
      GO TO 184
152   CONTINUE
      OAD_CTMP1 = (DBLE(FLAG) + 2.09999990460000018899D00)
      CALL oad_s_ifix(OAD_CTMP1, OAD_CTMP0)
      KLAG = OAD_CTMP0
      CALL oad_s_max_i(KLAG, 1_w2f__i8, OAD_CTMP3)
      CALL oad_s_min_i(OAD_CTMP3, 3_w2f__i8, OAD_CTMP2)
      KLAG = OAD_CTMP2
      T = PTPEN(JW, 2)
      T__27 = KLAG
      GO TO 153
153   CONTINUE
      IF(T__27 .eq. INT(1_w2f__i8)) THEN
        GO TO 170
      ELSE
        GO TO 154
      ENDIF
154   CONTINUE
      GO TO 155
155   CONTINUE
      IF(T__27 .eq. INT(2_w2f__i8)) THEN
        GO TO 164
      ELSE
        GO TO 156
      ENDIF
156   CONTINUE
      GO TO 157
157   CONTINUE
      IF(T__27 .eq. INT(3_w2f__i8)) THEN
        GO TO 159
      ELSE
        GO TO 158
      ENDIF
158   CONTINUE
      GO TO 170
159   CONTINUE
      PTPEN(INT(JW), 4) = 1.0D00
      T__29 = NCPS
      t__35 = T__29
      GO TO 160
160   CONTINUE
      I = 1
      GO TO 198
161   CONTINUE
      I = I + 1
198   CONTINUE
      IF(I .LE. t__35) THEN
        GO TO 162
      ELSE
        GO TO 180
      ENDIF
162   CONTINUE
      FV(INT(I)) = WEN(JW, I)
      GO TO 163
163   CONTINUE
      GO TO 161
164   CONTINUE
      PTPEN(INT(JW), 4) = 0.0D00
      T__28 = NCPS
      t__34 = T__28
      GO TO 165
165   CONTINUE
      I = 1
      GO TO 199
166   CONTINUE
      I = I + 1
199   CONTINUE
      IF(I .LE. t__34) THEN
        GO TO 168
      ELSE
        GO TO 167
      ENDIF
167   CONTINUE
      GO TO 180
168   CONTINUE
      FL(INT(I)) = WEN(JW, I)
      GO TO 169
169   CONTINUE
      GO TO 166
170   CONTINUE
      CALL iflash(JW)
      GO TO 171
171   CONTINUE
      IF(DBLE(PTPEN(JW, 4)) .LT. DBLE(VZLO)) THEN
        GO TO 173
      ELSE
        GO TO 172
      ENDIF
172   CONTINUE
      GO TO 174
173   CONTINUE
      KLAG = 2
      GO TO 174
174   CONTINUE
      GO TO 175
175   CONTINUE
      IF(DBLE(PTPEN(JW, 4)) .GT. DBLE(VZHI)) THEN
        GO TO 177
      ELSE
        GO TO 176
      ENDIF
176   CONTINUE
      GO TO 178
177   CONTINUE
      KLAG = 3
      GO TO 178
178   CONTINUE
      GO TO 179
179   CONTINUE
      GO TO 180
180   CONTINUE
      H = 0.0D00
      T__30 = NCPS
      t__36 = T__30
      GO TO 181
181   CONTINUE
      I = 1
      GO TO 200
182   CONTINUE
      I = I + 1
200   CONTINUE
      IF(I .LE. t__36) THEN
        GO TO 186
      ELSE
        GO TO 183
      ENDIF
183   CONTINUE
      PTPEN(INT(JW), 3) = (DBLE(H) / DBLE(WTEN(JW)))
      GO TO 184
184   CONTINUE
      RETURN
      GO TO 185
185   CONTINUE
186   CONTINUE
      OAD_CTMP4 = I
      CALL hvap(OAD_CTMP4, T, HVRES)
      HV = HVRES
      OAD_CTMP5 = I
      CALL hdel(OAD_CTMP5, T, HDRES)
      HL = (DBLE(HV) - DBLE(HDRES))
      T__27 = KLAG
      GO TO 187
187   CONTINUE
      IF(T__27 .eq. INT(1_w2f__i8)) THEN
        GO TO 195
      ELSE
        GO TO 188
      ENDIF
188   CONTINUE
      GO TO 189
189   CONTINUE
      IF(T__27 .eq. INT(2_w2f__i8)) THEN
        GO TO 194
      ELSE
        GO TO 190
      ENDIF
190   CONTINUE
      GO TO 191
191   CONTINUE
      IF(T__27 .eq. INT(3_w2f__i8)) THEN
        GO TO 193
      ELSE
        GO TO 192
      ENDIF
192   CONTINUE
      GO TO 195
193   CONTINUE
      H = (DBLE(H) + DBLE(WEN(JW, I)) * DBLE(HV))
      GO TO 196
194   CONTINUE
      H = (DBLE(H) + DBLE(WEN(JW, I)) * DBLE(HL))
      GO TO 196
195   CONTINUE
      H = (DBLE(H) + DBLE(FL(I)) * DBLE(HL) + DBLE(FV(I)) * DBLE(HV))
      GO TO 196
196   CONTINUE
      GO TO 197
197   CONTINUE
      GO TO 182
      END SUBROUTINE

      SUBROUTINE hvap(I, T, HVRES)
      use w2f__types
      use active_module
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
416   CONTINUE
      GO TO 417
417   CONTINUE
      XXXX = 0.0D00
      TA = TZ
      GO TO 418
418   CONTINUE
      J = 1
      GO TO 424
419   CONTINUE
      J = J + 1
424   CONTINUE
      IF(J .LE. 2) THEN
        GO TO 422
      ELSE
        GO TO 420
      ENDIF
420   CONTINUE
      CALL hdel(I, XXXX, HDRES)
      HVRES = (DBLE(H(2)) + DBLE(HDRES) - DBLE(H(1)))
      RETURN
      GO TO 421
421   CONTINUE
422   CONTINUE
      H(INT(J)) = (DBLE(CSXX(I, 9)) * DBLE(TA) + DBLE(CSXX(I, 10)) *(
     > DBLE(TA) ** INT(2_w2f__i8)) + DBLE(CSXX(I, 11)) *(DBLE(TA) **
     >  INT(3_w2f__i8)) + DBLE(CSXX(I, 12)) *(DBLE(TA) ** INT(4_w2f__i8
     > )) + DBLE(CSXX(I, 13)) *(DBLE(TA) ** INT(5_w2f__i8)) + DBLE(CSXX
     > (I, 14)) *(DBLE(TA) ** INT(6_w2f__i8)) + DBLE(CSXX(I, 15)) *(
     > DBLE(TA) ** INT(7_w2f__i8)))
      TA = (DBLE(T) + DBLE(TZ))
      GO TO 423
423   CONTINUE
      GO TO 419
      END SUBROUTINE

      SUBROUTINE iflash(JW)
      use w2f__types
      use active_module
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
      INTEGER(w2f__i4) OAD_CTMP0
      REAL(w2f__4) PSI
      EXTERNAL pvap
      REAL(w2f__4) PVRES
      REAL(w2f__4) R
      REAL(w2f__4) RK(1 : 12)
      REAL(w2f__4) RKMIN
      SAVE RKMIN
      REAL(w2f__4) TEST
      INTEGER(w2f__i4) T__34
      INTEGER(w2f__i4) T__35
      INTEGER(w2f__i4) T__36
      INTEGER(w2f__i4) T__37
      INTEGER(w2f__i4) T__38
      INTEGER(w2f__i4) T__39
      INTEGER(w2f__i4) T__40
      INTEGER(w2f__i4) t__41
      INTEGER(w2f__i4) t__42
      INTEGER(w2f__i4) t__43
      INTEGER(w2f__i4) t__44
      INTEGER(w2f__i4) t__45
      INTEGER(w2f__i4) t__46
C
C     **** Initializers ****
C
      DATA RKMIN / 1.0000000036E-15 /
C
C     **** Statements ****
C
295   CONTINUE
      GO TO 296
296   CONTINUE
      T__34 = NCPS
      t__41 = T__34
      GO TO 297
297   CONTINUE
      I = 1
      GO TO 383
298   CONTINUE
      I = I + 1
383   CONTINUE
      IF(I .LE. t__41) THEN
        GO TO 377
      ELSE
        GO TO 299
      ENDIF
299   CONTINUE
      MCAL = 1
      PSI = 1.0D00
      GO TO 300
300   CONTINUE
      MDO = 0
      GO TO 301
301   CONTINUE
      K = 1
      GO TO 384
302   CONTINUE
      K = K + 1
384   CONTINUE
      IF(K .LE. 20) THEN
        GO TO 304
      ELSE
        GO TO 303
      ENDIF
303   CONTINUE
      WRITE(JOUT, '(22H0*** FLASH CALCN, TYPE, I3,31H, FAILED TO CN' //
     >  'VRGE IN 20                                      ' //
     >  '                       ITER -/ 10X,5HDEL =, G13.' //
     >  '3,11H, FOR PSI =,G13.6 )') MCAL, DEL, PSI
      GO TO 328
304   CONTINUE
      TEST = (- DBLE(WTEN(JW)))
      DERIV = 0.0D00
      T__35 = NCPS
      t__42 = T__35
      GO TO 305
305   CONTINUE
      I = 1
      GO TO 385
306   CONTINUE
      I = I + 1
385   CONTINUE
      IF(I .LE. t__42) THEN
        GO TO 368
      ELSE
        GO TO 307
      ENDIF
307   CONTINUE
      IF(MDO .eq. INT(0_w2f__i8)) THEN
        GO TO 311
      ELSE
        GO TO 308
      ENDIF
308   CONTINUE
      GO TO 309
309   CONTINUE
      IF(MDO .eq. INT(1_w2f__i8)) THEN
        GO TO 321
      ELSE
        GO TO 310
      ENDIF
310   CONTINUE
      GO TO 311
311   CONTINUE
      GO TO 312
312   CONTINUE
      IF(DBLE(TEST) .LE. 0.0D00) THEN
        GO TO 314
      ELSE
        GO TO 313
      ENDIF
313   CONTINUE
      GO TO 319
314   CONTINUE
      T__36 = MCAL
      GO TO 315
315   CONTINUE
      IF(T__36 .eq. INT(1_w2f__i8)) THEN
        GO TO 356
      ELSE
        GO TO 316
      ENDIF
316   CONTINUE
      GO TO 317
317   CONTINUE
      IF(T__36 .eq. INT(2_w2f__i8)) THEN
        GO TO 351
      ELSE
        GO TO 318
      ENDIF
318   CONTINUE
      GO TO 319
319   CONTINUE
      GO TO 320
320   CONTINUE
      MDO = 1
      GO TO 321
321   CONTINUE
      DEL = (DBLE(TEST) / DBLE(DERIV))
      PSI = (DBLE(PSI) - DBLE(DEL))
      GO TO 322
322   CONTINUE
      IF(ABS(DBLE(DEL)) .LE. 9.99999997480000068427D-07) THEN
        GO TO 328
      ELSE
        GO TO 323
      ENDIF
323   CONTINUE
      GO TO 324
324   CONTINUE
      IF((MCAL .eq. INT(1_w2f__i8)) .AND.(DBLE(PSI) .LT.
     >  4.00000005960000015737D-01)) THEN
        GO TO 327
      ELSE
        GO TO 325
      ENDIF
325   CONTINUE
      GO TO 326
326   CONTINUE
      GO TO 302
327   CONTINUE
      MCAL = 2
      PSI = 0.0D00
      GO TO 300
328   CONTINUE
      T__36 = MCAL
      GO TO 329
329   CONTINUE
      IF(T__36 .eq. INT(1_w2f__i8)) THEN
        GO TO 342
      ELSE
        GO TO 330
      ENDIF
330   CONTINUE
      GO TO 331
331   CONTINUE
      IF(T__36 .eq. INT(2_w2f__i8)) THEN
        GO TO 333
      ELSE
        GO TO 332
      ENDIF
332   CONTINUE
      GO TO 342
333   CONTINUE
      GO TO 334
334   CONTINUE
      IF(DBLE(PSI) .LT. 9.99999997480000068427D-07) THEN
        GO TO 351
      ELSE
        GO TO 335
      ENDIF
335   CONTINUE
      GO TO 336
336   CONTINUE
      R = ((1.0D00 - DBLE(PSI)) / DBLE(PSI))
      T__38 = NCPS
      t__44 = T__38
      GO TO 337
337   CONTINUE
      I = 1
      GO TO 386
338   CONTINUE
      I = I + 1
386   CONTINUE
      IF(I .LE. t__44) THEN
        GO TO 340
      ELSE
        GO TO 339
      ENDIF
339   CONTINUE
      GO TO 360
340   CONTINUE
      FL(INT(I)) = (DBLE(WEN(JW, I)) /(DBLE(RK(I)) * DBLE(R) + 1.0D00))
      FV(INT(I)) = (DBLE(WEN(JW, I)) - DBLE(FL(I)))
      GO TO 341
341   CONTINUE
      GO TO 338
342   CONTINUE
      GO TO 343
343   CONTINUE
      IF(DBLE(PSI) .GT. 9.99998986719999982675D-01) THEN
        GO TO 356
      ELSE
        GO TO 344
      ENDIF
344   CONTINUE
      GO TO 345
345   CONTINUE
      R = (DBLE(PSI) /(1.0D00 - DBLE(PSI)))
      T__37 = NCPS
      t__43 = T__37
      GO TO 346
346   CONTINUE
      I = 1
      GO TO 387
347   CONTINUE
      I = I + 1
387   CONTINUE
      IF(I .LE. t__43) THEN
        GO TO 349
      ELSE
        GO TO 348
      ENDIF
348   CONTINUE
      GO TO 360
349   CONTINUE
      FV(INT(I)) = (DBLE(WEN(JW, I)) /((DBLE(R) / DBLE(RK(I))) + 1.0D00
     > ))
      FL(INT(I)) = (DBLE(WEN(JW, I)) - DBLE(FV(I)))
      GO TO 350
350   CONTINUE
      GO TO 347
351   CONTINUE
      T__40 = NCPS
      t__46 = T__40
      GO TO 352
352   CONTINUE
      I = 1
      GO TO 388
353   CONTINUE
      I = I + 1
388   CONTINUE
      IF(I .LE. t__46) THEN
        GO TO 354
      ELSE
        GO TO 360
      ENDIF
354   CONTINUE
      FV(INT(I)) = WEN(JW, I)
      FL(INT(I)) = 0.0D00
      GO TO 355
355   CONTINUE
      GO TO 353
356   CONTINUE
      T__39 = NCPS
      t__45 = T__39
      GO TO 357
357   CONTINUE
      I = 1
      GO TO 389
358   CONTINUE
      I = I + 1
389   CONTINUE
      IF(I .LE. t__45) THEN
        GO TO 366
      ELSE
        GO TO 359
      ENDIF
359   CONTINUE
      GO TO 360
360   CONTINUE
      PTPEN(INT(JW), 4) = (1.0D00 - DBLE(PSI))
      GO TO 361
361   CONTINUE
      IF(KFLAG .LE. INT(1_w2f__i8)) THEN
        GO TO 364
      ELSE
        GO TO 362
      ENDIF
362   CONTINUE
      GO TO 363
363   CONTINUE
      WRITE(JOUT, '(10X,4HK(I),8X,11HVAPOR FLOWS,5X,9HLIQ FLOWS, 3X' //
     >  ',"COMPONENT",/,(5X,3G15.5,3X,6A3 ))')(RK(I), FV(I), FL(I), (
     > ANAMC(I, JJ), JJ = 1, 6, 1), I = 1, NCPS, 1)
      GO TO 364
364   CONTINUE
      RETURN
      GO TO 365
365   CONTINUE
366   CONTINUE
      FL(INT(I)) = WEN(JW, I)
      FV(INT(I)) = 0.0D00
      GO TO 367
367   CONTINUE
      GO TO 358
368   CONTINUE
      T__36 = MCAL
      GO TO 369
369   CONTINUE
      IF(T__36 .eq. INT(1_w2f__i8)) THEN
        GO TO 374
      ELSE
        GO TO 370
      ENDIF
370   CONTINUE
      GO TO 371
371   CONTINUE
      IF(T__36 .eq. INT(2_w2f__i8)) THEN
        GO TO 373
      ELSE
        GO TO 372
      ENDIF
372   CONTINUE
      GO TO 374
373   CONTINUE
      NUMER = (DBLE(RK(I)) +(-1.0D00))
      DENOM = (DBLE(RK(I)) - DBLE(NUMER) * DBLE(PSI))
      GO TO 375
374   CONTINUE
      NUMER = (1.0D00 -(1.0D00 / DBLE(RK(I))))
      DENOM = (1.0D00 - DBLE(NUMER) * DBLE(PSI))
      GO TO 375
375   CONTINUE
      TEST = (DBLE(TEST) +(DBLE(WEN(JW, I)) / DBLE(DENOM)))
      DERIV = (DBLE(DERIV) +((DBLE(WEN(JW, I)) * DBLE(NUMER)) /(DBLE(
     > DENOM) ** INT(2_w2f__i8))))
      GO TO 376
376   CONTINUE
      GO TO 306
377   CONTINUE
      OAD_CTMP0 = I
      CALL pvap(OAD_CTMP0, PTPEN(JW, 2), PVRES)
      RK(INT(I)) = (DBLE(PVRES) / DBLE(PTPEN(JW, 1)))
      GO TO 378
378   CONTINUE
      IF(DBLE(RK(I)) .GE. DBLE(RKMIN)) THEN
        GO TO 381
      ELSE
        GO TO 379
      ENDIF
379   CONTINUE
      GO TO 380
380   CONTINUE
      WRITE(JOUT, '(13H0** COMPONENT ,I4,23H SHOWS NEGL. VOLATILITY' //
     >  ' )') I
      RK(INT(I)) = RKMIN
      GO TO 381
381   CONTINUE
      GO TO 382
382   CONTINUE
      GO TO 298
      END SUBROUTINE

      SUBROUTINE pvap(I, T, PVRES)
      use w2f__types
      use active_module
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
425   CONTINUE
      GO TO 426
426   CONTINUE
      TC = ((DBLE(T) +(-3.2D+01)) / 1.7999999523000000945D00)
      PLIQ = (DBLE(CSXX(I, 24)) -(DBLE(CSXX(I, 25)) /(DBLE(CSXX(I, 26))
     >  + DBLE(TC))))
      GO TO 427
427   CONTINUE
      IF(DBLE(PLIQ) .LT. DBLE(PLMAX)) THEN
        GO TO 430
      ELSE
        GO TO 428
      ENDIF
428   CONTINUE
      GO TO 429
429   CONTINUE
      PVRES = 1.0D+10
      GO TO 435
430   CONTINUE
      GO TO 431
431   CONTINUE
      IF(DBLE(PLIQ) .GT. DBLE(PLMIN)) THEN
        GO TO 434
      ELSE
        GO TO 432
      ENDIF
432   CONTINUE
      GO TO 433
433   CONTINUE
      PVRES = 0.0D00
      GO TO 435
434   CONTINUE
      PVRES = ((1.0D+01 ** DBLE(PLIQ)) * 1.93368419999999999193D-02)
      GO TO 435
435   CONTINUE
      RETURN
      GO TO 436
436   CONTINUE
      END SUBROUTINE

      SUBROUTINE tbtd(KS, JW, P, TB, TD, HB, HD)
      use w2f__types
      use active_module
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
      INTEGER(w2f__i4) OAD_CTMP0
      INTEGER(w2f__i4) OAD_CTMP1
      REAL(w2f__4) OAD_CTMP10
      REAL(w2f__4) OAD_CTMP11
      REAL(w2f__4) OAD_CTMP12
      REAL(w2f__4) OAD_CTMP13
      INTEGER(w2f__i4) OAD_CTMP14
      REAL(w2f__4) OAD_CTMP15
      REAL(w2f__4) OAD_CTMP16
      REAL(w2f__4) OAD_CTMP17
      REAL(w2f__4) OAD_CTMP18
      INTEGER(w2f__i4) OAD_CTMP2
      REAL(w2f__4) OAD_CTMP3
      INTEGER(w2f__i4) OAD_CTMP4
      REAL(w2f__4) OAD_CTMP5
      REAL(w2f__4) OAD_CTMP6
      REAL(w2f__4) OAD_CTMP7
      INTEGER(w2f__i4) OAD_CTMP8
      REAL(w2f__4) OAD_CTMP9
      EXTERNAL oad_s_log
      EXTERNAL oad_s_max_r
      EXTERNAL oad_s_min_r
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
      INTEGER(w2f__i4) T__49
      INTEGER(w2f__i4) T__50
      INTEGER(w2f__i4) T__51
      INTEGER(w2f__i4) T__52
      INTEGER(w2f__i4) T__53
      INTEGER(w2f__i4) T__54
      INTEGER(w2f__i4) T__55
      INTEGER(w2f__i4) T__56
      INTEGER(w2f__i4) T__57
      INTEGER(w2f__i4) T__58
      INTEGER(w2f__i4) T__59
      INTEGER(w2f__i4) T__60
      INTEGER(w2f__i4) T__61
      INTEGER(w2f__i4) T__62
      REAL(w2f__4) VF
      REAL(w2f__4) VFAC
      SAVE VFAC
      REAL(w2f__4) W
      REAL(w2f__4) WTOL
      SAVE WTOL
      REAL(w2f__4) ZF(1 : 12)
      INTEGER(w2f__i4) t__48
      INTEGER(w2f__i4) t__63
      INTEGER(w2f__i4) t__64
      INTEGER(w2f__i4) t__65
      INTEGER(w2f__i4) t__66
      INTEGER(w2f__i4) t__67
      INTEGER(w2f__i4) t__68
      INTEGER(w2f__i4) t__69
      INTEGER(w2f__i4) t__70
      INTEGER(w2f__i4) t__71
      INTEGER(w2f__i4) t__72
      INTEGER(w2f__i4) t__73
      INTEGER(w2f__i4) t__74
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
C
C     **** Statements ****
C
437   CONTINUE
      GO TO 438
438   CONTINUE
      GO TO 439
439   CONTINUE
      IF((DBLE(WTEN(JW)) .LE. DBLE(WTOL)) .OR.(DBLE(P) .LE. DBLE(PTOL))
     > ) THEN
        GO TO 638
      ELSE
        GO TO 440
      ENDIF
440   CONTINUE
      GO TO 441
441   CONTINUE
      IF((DBLE(TB) .GT. DBLE(TLIM)) .OR.(DBLE(TB) .LT. DBLE(TZ))) THEN
        GO TO 443
      ELSE
        GO TO 442
      ENDIF
442   CONTINUE
      GO TO 444
443   CONTINUE
      TB = 0.0D00
      GO TO 444
444   CONTINUE
      GO TO 445
445   CONTINUE
      IF((DBLE(TD) .GT. DBLE(TLIM)) .OR.(DBLE(TD) .LT. DBLE(TZ))) THEN
        GO TO 447
      ELSE
        GO TO 446
      ENDIF
446   CONTINUE
      GO TO 448
447   CONTINUE
      TD = 0.0D00
      GO TO 448
448   CONTINUE
      GO TO 449
449   CONTINUE
      AKBARL = 0.0D00
      T__49 = NCPS
      t__48 = T__49
      GO TO 450
450   CONTINUE
      I = 1
      GO TO 640
451   CONTINUE
      I = I + 1
640   CONTINUE
      IF(I .LE. t__48) THEN
        GO TO 636
      ELSE
        GO TO 452
      ENDIF
452   CONTINUE
      AKBAR = (1.0D+01 ** DBLE(AKBARL))
      GO TO 453
453   CONTINUE
      IF(KS .eq. INT(2_w2f__i8)) THEN
        GO TO 504
      ELSE
        GO TO 454
      ENDIF
454   CONTINUE
      GO TO 455
455   CONTINUE
      T__50 = NCPS
      t__63 = T__50
      GO TO 456
456   CONTINUE
      I = 1
      GO TO 641
457   CONTINUE
      I = I + 1
641   CONTINUE
      IF(I .LE. t__63) THEN
        GO TO 458
      ELSE
        GO TO 557
      ENDIF
458   CONTINUE
      RATIOK = (DBLE(AKE(I)) / DBLE(AKBAR))
      GO TO 459
459   CONTINUE
      IF((DBLE(RATIOK) .GE. 1.0D+02) .AND.(DBLE(WEN(JW, I)) .GT. 0.0D00
     > )) THEN
        GO TO 462
      ELSE
        GO TO 460
      ENDIF
460   CONTINUE
      GO TO 461
461   CONTINUE
      GO TO 457
462   CONTINUE
      J = 0
      SUMZ = 0.0D00
      T__55 = NCPS
      t__67 = T__55
      GO TO 463
463   CONTINUE
      I = 1
      GO TO 642
464   CONTINUE
      I = I + 1
642   CONTINUE
      IF(I .LE. t__67) THEN
        GO TO 498
      ELSE
        GO TO 465
      ENDIF
465   CONTINUE
      VF = (DBLE(SUMZ) *(DBLE(VFAC) + 1.0D00))
      CALL oad_s_max_r(VF, 1.00000004750000004786D-03, OAD_CTMP7)
      VF = OAD_CTMP7
      WRITE(JOUT, '(57H0****NON-CONDENSABLES PRESENT,BUBBLE POINT C' //
     >  'AL. WITH VF                                     ' //
     >  '                       = ,F6.3)') VF
      T = TB
      I1 = 1
      T__56 = LIM2
      t__68 = T__56
      GO TO 466
466   CONTINUE
      I = 1
      GO TO 643
467   CONTINUE
      I = I + 1
643   CONTINUE
      IF(I .LE. t__68) THEN
        GO TO 469
      ELSE
        GO TO 468
      ENDIF
468   CONTINUE
      MS = 1
      GO TO 566
469   CONTINUE
      SUMD = 0.0D00
      T__57 = NCPS
      t__69 = T__57
      GO TO 470
470   CONTINUE
      K = 1
      GO TO 644
471   CONTINUE
      K = K + 1
644   CONTINUE
      IF(K .LE. t__69) THEN
        GO TO 489
      ELSE
        GO TO 472
      ENDIF
472   CONTINUE
      FUNXON = (DBLE(SUMD) +(1.0D00 /(DBLE(VFAC) + 1.0D00)))
      GO TO 473
473   CONTINUE
      IF(DBLE(EPS) .GT. ABS(DBLE(FUNXON))) THEN
        GO TO 595
      ELSE
        GO TO 474
      ENDIF
474   CONTINUE
      GO TO 475
475   CONTINUE
      IF(I .GT. INT(1_w2f__i8)) THEN
        GO TO 478
      ELSE
        GO TO 476
      ENDIF
476   CONTINUE
      GO TO 477
477   CONTINUE
      DFUNC = 0.0D00
      GO TO 479
478   CONTINUE
      DFUNC = (DBLE(DT) /(DBLE(FUNXON) - DBLE(FUNCS)))
      GO TO 479
479   CONTINUE
      OAD_CTMP10 = ABS(DBLE(DFUNC))
      CALL oad_s_max_r(OAD_CTMP10, 9.99999977650000045071D-03,
     >  OAD_CTMP9)
      DT = (-(DBLE(FUNXON) * SIGN(DBLE(OAD_CTMP9), DBLE(DFUNC))))
      CALL oad_s_min_r(DH, DT, OAD_CTMP12)
      CALL oad_s_max_r(DL, OAD_CTMP12, OAD_CTMP11)
      DT = OAD_CTMP11
      GO TO 480
480   CONTINUE
      IF(DBLE(EPS) .GE. ABS(DBLE(DT))) THEN
        GO TO 595
      ELSE
        GO TO 481
      ENDIF
481   CONTINUE
      GO TO 482
482   CONTINUE
      FUNCS = FUNXON
      GO TO 483
483   CONTINUE
      GO TO 484
484   CONTINUE
      IF((DBLE(TLIM) .GT.(DBLE(DT) + DBLE(T))) .AND.(DBLE(TZ) .LT.(DBLE
     > (DT) + DBLE(T)))) THEN
        GO TO 487
      ELSE
        GO TO 485
      ENDIF
485   CONTINUE
      GO TO 486
486   CONTINUE
      DT = (DBLE(DT) * 5.0D-01)
      GO TO 483
487   CONTINUE
      T = (DBLE(DT) + DBLE(T))
      GO TO 488
488   CONTINUE
      GO TO 467
489   CONTINUE
      T__58 = J
      t__70 = T__58
      GO TO 490
490   CONTINUE
      KK = 1
      GO TO 645
491   CONTINUE
      KK = KK + 1
645   CONTINUE
      IF(KK .LE. t__70) THEN
        GO TO 493
      ELSE
        GO TO 492
      ENDIF
492   CONTINUE
      OAD_CTMP8 = K
      CALL pvap(OAD_CTMP8, T, PVRES)
      PVK = (DBLE(PVRES) / DBLE(P))
      SUMD = (DBLE(SUMD) +(((DBLE(PVK) +(-1.0D00)) *(DBLE(WEN(JW, K)) /
     >  DBLE(WTEN(JW)))) /(DBLE(PVK) * DBLE(VF) + 1.0D00 - DBLE(VF))))
      GO TO 496
493   CONTINUE
      IF(KJ(KK) .eq. K) THEN
        GO TO 496
      ELSE
        GO TO 494
      ENDIF
494   CONTINUE
      GO TO 495
495   CONTINUE
      GO TO 491
496   CONTINUE
      GO TO 497
497   CONTINUE
      GO TO 471
498   CONTINUE
      RATIOK = (DBLE(AKE(I)) / DBLE(AKBAR))
      GO TO 499
499   CONTINUE
      IF(DBLE(RATIOK) .LT. 1.0D+02) THEN
        GO TO 502
      ELSE
        GO TO 500
      ENDIF
500   CONTINUE
      GO TO 501
501   CONTINUE
      J = J + 1
      ZF(INT(J)) = (DBLE(WEN(JW, I)) / DBLE(WTEN(JW)))
      KJ(J) = I
      SUMZ = (DBLE(ZF(J)) + DBLE(SUMZ))
      GO TO 502
502   CONTINUE
      GO TO 503
503   CONTINUE
      GO TO 464
504   CONTINUE
      T__51 = NCPS
      t__64 = T__51
      GO TO 505
505   CONTINUE
      I = 1
      GO TO 646
506   CONTINUE
      I = I + 1
646   CONTINUE
      IF(I .LE. t__64) THEN
        GO TO 508
      ELSE
        GO TO 507
      ENDIF
507   CONTINUE
      T = TD
      D = (DBLE(WTEN(JW)) / DBLE(P))
      MS = 2
      GO TO 558
508   CONTINUE
      RATIOK = (DBLE(AKE(I)) / DBLE(AKBAR))
      GO TO 509
509   CONTINUE
      IF((DBLE(WEN(JW, I)) .GT. 0.0D00) .AND.(DBLE(RATIOK) .LE.
     >  9.99999977650000045071D-03)) THEN
        GO TO 512
      ELSE
        GO TO 510
      ENDIF
510   CONTINUE
      GO TO 511
511   CONTINUE
      GO TO 506
512   CONTINUE
      GO TO 513
513   CONTINUE
      IF(KS .eq. INT(1_w2f__i8)) THEN
        GO TO 557
      ELSE
        GO TO 514
      ENDIF
514   CONTINUE
      GO TO 515
515   CONTINUE
      J = 0
      SUMZ = 0.0D00
      T__59 = NCPS
      t__71 = T__59
      GO TO 516
516   CONTINUE
      I = 1
      GO TO 647
517   CONTINUE
      I = I + 1
647   CONTINUE
      IF(I .LE. t__71) THEN
        GO TO 551
      ELSE
        GO TO 518
      ENDIF
518   CONTINUE
      ALF = (DBLE(SUMZ) *(DBLE(VFAC) + 1.0D00))
      CALL oad_s_max_r(ALF, 1.00000004750000004786D-03, OAD_CTMP13)
      ALF = OAD_CTMP13
      VF = (1.0D00 - DBLE(ALF))
      WRITE(JOUT, '(51H0****NON-VOLATILES PRESENT,DEW POINT CAL. WI' //
     >  'TH LF= ,F6.3)') ALF
      T = TD
      I1 = 1
      T__60 = LIM2
      t__72 = T__60
      GO TO 519
519   CONTINUE
      I = 1
      GO TO 648
520   CONTINUE
      I = I + 1
648   CONTINUE
      IF(I .LE. t__72) THEN
        GO TO 522
      ELSE
        GO TO 521
      ENDIF
521   CONTINUE
      MS = 2
      GO TO 566
522   CONTINUE
      SUMD = 0.0D00
      T__61 = NCPS
      t__73 = T__61
      GO TO 523
523   CONTINUE
      K = 1
      GO TO 649
524   CONTINUE
      K = K + 1
649   CONTINUE
      IF(K .LE. t__73) THEN
        GO TO 542
      ELSE
        GO TO 525
      ENDIF
525   CONTINUE
      FUNXON = (DBLE(SUMD) +(1.0D00 /(DBLE(VFAC) + 1.0D00)))
      GO TO 526
526   CONTINUE
      IF(DBLE(EPS) .GT. ABS(DBLE(FUNXON))) THEN
        GO TO 594
      ELSE
        GO TO 527
      ENDIF
527   CONTINUE
      GO TO 528
528   CONTINUE
      IF(I .GT. INT(1_w2f__i8)) THEN
        GO TO 531
      ELSE
        GO TO 529
      ENDIF
529   CONTINUE
      GO TO 530
530   CONTINUE
      DFUNC = 0.0D00
      GO TO 532
531   CONTINUE
      DFUNC = (DBLE(DT) /(DBLE(FUNXON) - DBLE(FUNCS)))
      GO TO 532
532   CONTINUE
      OAD_CTMP16 = ABS(DBLE(DFUNC))
      CALL oad_s_max_r(OAD_CTMP16, 9.99999977650000045071D-03,
     >  OAD_CTMP15)
      DT = (-(DBLE(FUNXON) * SIGN(DBLE(OAD_CTMP15), DBLE(DFUNC))))
      CALL oad_s_min_r(DH, DT, OAD_CTMP18)
      CALL oad_s_max_r(DL, OAD_CTMP18, OAD_CTMP17)
      DT = OAD_CTMP17
      GO TO 533
533   CONTINUE
      IF(DBLE(EPS) .GT. ABS(DBLE(DT))) THEN
        GO TO 594
      ELSE
        GO TO 534
      ENDIF
534   CONTINUE
      GO TO 535
535   CONTINUE
      FUNCS = FUNXON
      GO TO 536
536   CONTINUE
      GO TO 537
537   CONTINUE
      IF((DBLE(TLIM) .GT.(DBLE(DT) + DBLE(T))) .AND.(DBLE(TZ) .LT.(DBLE
     > (DT) + DBLE(T)))) THEN
        GO TO 540
      ELSE
        GO TO 538
      ENDIF
538   CONTINUE
      GO TO 539
539   CONTINUE
      DT = (DBLE(DT) * 5.0D-01)
      GO TO 536
540   CONTINUE
      T = (DBLE(DT) + DBLE(T))
      GO TO 541
541   CONTINUE
      GO TO 520
542   CONTINUE
      T__62 = J
      t__74 = T__62
      GO TO 543
543   CONTINUE
      KK = 1
      GO TO 650
544   CONTINUE
      KK = KK + 1
650   CONTINUE
      IF(KK .LE. t__74) THEN
        GO TO 546
      ELSE
        GO TO 545
      ENDIF
545   CONTINUE
      OAD_CTMP14 = K
      CALL pvap(OAD_CTMP14, T, PVRES)
      PVK = (DBLE(PVRES) / DBLE(P))
      SUMD = (DBLE(SUMD) +(((DBLE(PVK) +(-1.0D00)) *(DBLE(WEN(JW, K)) /
     >  DBLE(WTEN(JW)))) /(DBLE(PVK) * DBLE(VF) + 1.0D00 - DBLE(VF))))
      GO TO 549
546   CONTINUE
      IF(KJ(KK) .eq. K) THEN
        GO TO 549
      ELSE
        GO TO 547
      ENDIF
547   CONTINUE
      GO TO 548
548   CONTINUE
      GO TO 544
549   CONTINUE
      GO TO 550
550   CONTINUE
      GO TO 524
551   CONTINUE
      RATIOK = (DBLE(AKE(I)) / DBLE(AKBAR))
      GO TO 552
552   CONTINUE
      IF(DBLE(RATIOK) .GT. 9.99999977650000045071D-03) THEN
        GO TO 555
      ELSE
        GO TO 553
      ENDIF
553   CONTINUE
      GO TO 554
554   CONTINUE
      J = J + 1
      ZF(INT(J)) = (DBLE(WEN(JW, I)) / DBLE(WTEN(JW)))
      KJ(J) = I
      SUMZ = (DBLE(ZF(J)) + DBLE(SUMZ))
      GO TO 555
555   CONTINUE
      GO TO 556
556   CONTINUE
      GO TO 517
557   CONTINUE
      T = TB
      D = (DBLE(WTEN(JW)) * DBLE(P))
      MS = 1
      GO TO 558
558   CONTINUE
      GO TO 559
559   CONTINUE
      IF((DBLE(T) .GT. DBLE(TLIM)) .OR.(DBLE(T) .LT. DBLE(TZ))) THEN
        GO TO 561
      ELSE
        GO TO 560
      ENDIF
560   CONTINUE
      GO TO 562
561   CONTINUE
      T = 0.0D00
      GO TO 562
562   CONTINUE
      GO TO 563
563   CONTINUE
      MB = 0
      T__52 = LIM
      t__65 = T__52
      GO TO 564
564   CONTINUE
      J = 1
      GO TO 651
565   CONTINUE
      J = J + 1
651   CONTINUE
      IF(J .LE. t__65) THEN
        GO TO 567
      ELSE
        GO TO 566
      ENDIF
566   CONTINUE
      NMP = NMOPT(MS)
      WRITE(JOUT, '( 6H0**** ,A3,34H-POINT CALCN HAS NOT CONVERGED ' //
     >  'IN ,I3, 13H ITERATIONS -/6X,22HDATA AND RESULTS ' //
     >  'ARE - / 6X,  3HP = , F10.4, 6H, TB =, F10.4, 6H,' //
     >  ' TD =, F10.4, 6H, DT =,F10.4/  6X,5HDEL =,F12.8,' //
     >  '14H, DEL(PREV.) =,F12.8, 7H, EPS =, F12.8 /  6X,' //
     >  '29HFLOWS (LB MOLS/HR) - TOTAL = ,F12.3/ (3X,6F11' // '.3) )')
     >  NMP, LIM, P, TB, TD, DT, DEL, DELP, EPS, WTEN(JW), (WEN(JW, K),
     >  K = 1, NCPS, 1)
      GO TO 589
567   CONTINUE
      DEL = D
      T__53 = NCPS
      t__66 = T__53
      GO TO 568
568   CONTINUE
      I = 1
      GO TO 652
569   CONTINUE
      I = I + 1
652   CONTINUE
      IF(I .LE. t__66) THEN
        GO TO 618
      ELSE
        GO TO 570
      ENDIF
570   CONTINUE
      DEL = (DBLE(DEL) / DBLE(D))
      GO TO 571
571   CONTINUE
      IF(DBLE(EPS) .GE. ABS(DBLE(DEL))) THEN
        GO TO 589
      ELSE
        GO TO 572
      ENDIF
572   CONTINUE
      GO TO 573
573   CONTINUE
      IF(MB .eq. INT(0_w2f__i8)) THEN
        GO TO 582
      ELSE
        GO TO 574
      ENDIF
574   CONTINUE
      GO TO 575
575   CONTINUE
      IF(MB .eq. INT(1_w2f__i8)) THEN
        GO TO 577
      ELSE
        GO TO 576
      ENDIF
576   CONTINUE
      GO TO 582
577   CONTINUE
      DELP = ((DBLE(DELP) - DBLE(DEL)) / DBLE(DEL))
      GO TO 578
578   CONTINUE
      IF(DBLE(EPS) .LT. ABS(DBLE(DELP))) THEN
        GO TO 581
      ELSE
        GO TO 579
      ENDIF
579   CONTINUE
      GO TO 580
580   CONTINUE
      GO TO 582
581   CONTINUE
      DT = (DBLE(DT) / DBLE(DELP))
      GO TO 588
582   CONTINUE
      MB = 1
      DT = SIGN(DBLE(FR) *(DBLE(T) - DBLE(TZ)), DBLE(DEL))
      GO TO 583
583   CONTINUE
      IF(MS .eq. INT(2_w2f__i8)) THEN
        GO TO 585
      ELSE
        GO TO 584
      ENDIF
584   CONTINUE
      GO TO 586
585   CONTINUE
      DT = (- DBLE(DT))
      GO TO 586
586   CONTINUE
      GO TO 587
587   CONTINUE
      GO TO 588
588   CONTINUE
      CALL oad_s_min_r(DH, DT, OAD_CTMP6)
      CALL oad_s_max_r(DL, OAD_CTMP6, OAD_CTMP5)
      DT = OAD_CTMP5
      DELP = DEL
      GO TO 631
589   CONTINUE
      T__54 = MS
      GO TO 590
590   CONTINUE
      IF(T__54 .eq. INT(1_w2f__i8)) THEN
        GO TO 595
      ELSE
        GO TO 591
      ENDIF
591   CONTINUE
      GO TO 592
592   CONTINUE
      IF(T__54 .eq. INT(2_w2f__i8)) THEN
        GO TO 594
      ELSE
        GO TO 593
      ENDIF
593   CONTINUE
      GO TO 595
594   CONTINUE
      TD = T
      GO TO 599
595   CONTINUE
      TB = T
      GO TO 596
596   CONTINUE
      IF(KS .eq. INT(3_w2f__i8)) THEN
        GO TO 504
      ELSE
        GO TO 597
      ENDIF
597   CONTINUE
      GO TO 598
598   CONTINUE
      GO TO 599
599   CONTINUE
      GO TO 600
600   CONTINUE
      IF((DBLE(TB) .GT. DBLE(TLIM)) .OR.(DBLE(TB) .LT. DBLE(TZ))) THEN
        GO TO 617
      ELSE
        GO TO 601
      ENDIF
601   CONTINUE
      GO TO 602
602   CONTINUE
      IF((DBLE(TD) .GT. DBLE(TZ)) .AND.(DBLE(TD) .LT. DBLE(TLIM))) THEN
        GO TO 604
      ELSE
        GO TO 603
      ENDIF
603   CONTINUE
      GO TO 617
604   CONTINUE
      GO TO 605
605   CONTINUE
      IF(KFLAG .LE. INT(1_w2f__i8)) THEN
        GO TO 616
      ELSE
        GO TO 606
      ENDIF
606   CONTINUE
      GO TO 607
607   CONTINUE
      WRITE(JOUT, '(3H AT, F10.5,6H PSIA, )') P
      GO TO 608
608   CONTINUE
      IF(KS .ne. INT(2_w2f__i8)) THEN
        GO TO 610
      ELSE
        GO TO 609
      ENDIF
609   CONTINUE
      GO TO 611
610   CONTINUE
      WRITE(JOUT, '(1H+,20X,9HBBLE PT = ,F10.5,4H DF, )') TB
      GO TO 611
611   CONTINUE
      GO TO 612
612   CONTINUE
      IF(KS .GT. INT(1_w2f__i8)) THEN
        GO TO 614
      ELSE
        GO TO 613
      ENDIF
613   CONTINUE
      GO TO 615
614   CONTINUE
      WRITE(JOUT, '(1H+,44X,8HDEW PT = ,F10.5)') TD
      GO TO 615
615   CONTINUE
      GO TO 616
616   CONTINUE
      RETURN
      GO TO 639
617   CONTINUE
      WRITE(JOUT, '(/,10X,"BUBBLE AND/OR DEW POINT TEMPS OUT OF BOU' //
     >  'NDS",2E20.4)') TB, TD
      STOP
      RETURN
      GO TO 639
618   CONTINUE
      W = WEN(JW, I)
      GO TO 619
619   CONTINUE
      IF(DBLE(W) .LE. DBLE(WTOL)) THEN
        GO TO 634
      ELSE
        GO TO 620
      ENDIF
620   CONTINUE
      GO TO 621
621   CONTINUE
      OAD_CTMP4 = I
      CALL pvap(OAD_CTMP4, T, PVRES)
      PV = PVRES
      T__54 = MS
      GO TO 622
622   CONTINUE
      IF(T__54 .eq. INT(1_w2f__i8)) THEN
        GO TO 633
      ELSE
        GO TO 623
      ENDIF
623   CONTINUE
      GO TO 624
624   CONTINUE
      IF(T__54 .eq. INT(2_w2f__i8)) THEN
        GO TO 626
      ELSE
        GO TO 625
      ENDIF
625   CONTINUE
      GO TO 633
626   CONTINUE
      GO TO 627
627   CONTINUE
      IF(DBLE(PTOL) .GE. DBLE(PV)) THEN
        GO TO 630
      ELSE
        GO TO 628
      ENDIF
628   CONTINUE
      GO TO 629
629   CONTINUE
      DEL = (DBLE(DEL) -(DBLE(W) / DBLE(PV)))
      GO TO 634
630   CONTINUE
      DT = DH
      MB = 0
      GO TO 631
631   CONTINUE
      T = (DBLE(DT) + DBLE(T))
      GO TO 632
632   CONTINUE
      GO TO 565
633   CONTINUE
      DEL = (DBLE(DEL) - DBLE(PV) * DBLE(W))
      GO TO 634
634   CONTINUE
      GO TO 635
635   CONTINUE
      GO TO 569
636   CONTINUE
      OAD_CTMP0 = I
      CALL pvap(OAD_CTMP0, TB, PVRES)
      AKE(INT(I)) = (DBLE(PVRES) / DBLE(P))
      OAD_CTMP3 = (DBLE(AKE(I)) + 1.00000001339999999893D-10)
      CALL oad_s_log(OAD_CTMP3, OAD_CTMP1)
      CALL oad_s_log(1.0D+01, OAD_CTMP2)
      AKBARL = (DBLE(AKBARL) +((OAD_CTMP1 *(DBLE(WEN(JW, I)) / DBLE(
     > WTEN(JW)))) / OAD_CTMP2))
      GO TO 637
637   CONTINUE
      GO TO 451
638   CONTINUE
      WRITE(JOUT, '(32H0**** TBTD HAS BAD DATA --- KS =,I3, 6H, JW ' //
     >  '=, I3 / 6X,    3HP =,F10.4, 6H, TB =,F10.4,6H, T' //
     >  'D =, F10.4,6H, WT =, F12.3 )') KS, JW, P, TB, TD, WTEN(JW)
      RETURN
      GO TO 639
639   CONTINUE
      END SUBROUTINE

      SUBROUTINE tset(JW, FLAG)
      use w2f__types
      use active_module
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
      INTEGER(w2f__i4) OAD_CTMP0
      REAL(w2f__4) OAD_CTMP1
      REAL(w2f__4) OAD_CTMP2
      REAL(w2f__4) OAD_CTMP3
      EXTERNAL oad_s_min_r
      REAL(w2f__4) P
      REAL(w2f__4) T(1 : 2)
      EXTERNAL tbtd
      LOGICAL(w2f__i4) T__66
      INTEGER(w2f__i4) T__67
      REAL(w2f__4) X
      INTEGER(w2f__i4) t__75
C
C     **** Initializers ****
C
      DATA DTB / 1.0E+08 /
      DATA DTOL / 9.9999997474E-06 /
      DATA DTS / 1.0E+01 /
      DATA NLIM / 20 /
C
C     **** Statements ****
C
201   CONTINUE
      GO TO 202
202   CONTINUE
      HS = PTPEN(JW, 3)
      GO TO 203
203   CONTINUE
      IF(KFLAG .GT. INT(1_w2f__i8)) THEN
        GO TO 205
      ELSE
        GO TO 204
      ENDIF
204   CONTINUE
      GO TO 206
205   CONTINUE
      WRITE(JOUT, '(30H FIND TEMP. TO MATCH ENTHY. OF ,F12.3)') HS
      GO TO 206
206   CONTINUE
      GO TO 207
207   CONTINUE
      IF(DBLE(FLAG) .GE. 0.0D00) THEN
        GO TO 234
      ELSE
        GO TO 208
      ENDIF
208   CONTINUE
      GO TO 209
209   CONTINUE
      P = PTPEN(JW, 1)
      OAD_CTMP0 = 3
      CALL tbtd(OAD_CTMP0, JW, P, T(1), T(2), H(1), H(2))
      GO TO 210
210   CONTINUE
      J = 1
      GO TO 256
211   CONTINUE
      J = J + 1
256   CONTINUE
      IF(J .LE. 2) THEN
        GO TO 217
      ELSE
        GO TO 212
      ENDIF
212   CONTINUE
      X = ((DBLE(HS) - DBLE(H(1))) /(DBLE(H(2)) - DBLE(H(1))))
      GO TO 213
213   CONTINUE
      IF(DBLE(DTOL) .GT.(DBLE(T(2)) - DBLE(T(1)))) THEN
        GO TO 216
      ELSE
        GO TO 214
      ENDIF
214   CONTINUE
      GO TO 215
215   CONTINUE
      DT = (DBLE(X) *(DBLE(T(2)) - DBLE(T(1))))
      H2 = H(1)
      PTPEN(INT(JW), 2) = (DBLE(T(1)) + DBLE(DT))
      GO TO 240
216   CONTINUE
      PTPEN(INT(JW), 3) = HS
      PTPEN(INT(JW), 4) = X
      GO TO 254
217   CONTINUE
      IF(KPROPS .GT. INT(0_w2f__i8)) THEN
        GO TO 220
      ELSE
        GO TO 218
      ENDIF
218   CONTINUE
      GO TO 219
219   CONTINUE
      PTPEN(INT(JW), 2) = T(J)
      FLG = FLOAT(J + INT((-1_w2f__i8)))
      CALL hmx(JW, FLG)
      H(INT(J)) = PTPEN(JW, 3)
      GO TO 220
220   CONTINUE
      T__66 = ((((-1.0) ** J)) *(H(J) - HS)) .LE. 0.0
      GO TO 221
221   CONTINUE
      IF(T__66) THEN
        GO TO 224
      ELSE
        GO TO 222
      ENDIF
222   CONTINUE
      GO TO 223
223   CONTINUE
      GO TO 211
224   CONTINUE
      H2 = H(J)
      DT = (DBLE(DTS) *((-1.0D00) ** J))
      PTPEN(INT(JW), 2) = (DBLE(T(J)) + DBLE(DT))
      GO TO 225
225   CONTINUE
      IF(J .eq. INT(1_w2f__i8)) THEN
        GO TO 227
      ELSE
        GO TO 226
      ENDIF
226   CONTINUE
      GO TO 228
227   CONTINUE
      FLG = 0.0D00
      GO TO 228
228   CONTINUE
      GO TO 229
229   CONTINUE
      IF(J .eq. INT(2_w2f__i8)) THEN
        GO TO 231
      ELSE
        GO TO 230
      ENDIF
230   CONTINUE
      GO TO 232
231   CONTINUE
      FLG = 1.0D00
      GO TO 232
232   CONTINUE
      GO TO 233
233   CONTINUE
      GO TO 241
234   CONTINUE
      GO TO 235
235   CONTINUE
      IF(DBLE(DTOL) .GT. ABS(DBLE(PTPEN(JW, 2)))) THEN
        GO TO 237
      ELSE
        GO TO 236
      ENDIF
236   CONTINUE
      GO TO 238
237   CONTINUE
      PTPEN(INT(JW), 2) = SIGN(DBLE(DTS), DBLE(HS))
      GO TO 238
238   CONTINUE
      GO TO 239
239   CONTINUE
      DT = PTPEN(JW, 2)
      H2 = 0.0D00
      GO TO 240
240   CONTINUE
      FLG = FLAG
      GO TO 241
241   CONTINUE
      T__67 = NLIM
      t__75 = T__67
      GO TO 242
242   CONTINUE
      I = 1
      GO TO 257
243   CONTINUE
      I = I + 1
257   CONTINUE
      IF(I .LE. t__75) THEN
        GO TO 245
      ELSE
        GO TO 244
      ENDIF
244   CONTINUE
      WRITE(JOUT, '(  6H0AFTER, I8,25H ITERATIONS, TEMP DIFF IS,  G' //
     >  '11.5,        9H DEG FAHR )') NLIM, DT
      GO TO 253
245   CONTINUE
      CALL hmx(JW, FLG)
      H1 = PTPEN(JW, 3)
      DT = ((DBLE(DT) *(DBLE(HS) - DBLE(H1))) /(DBLE(H1) - DBLE(H2)))
      OAD_CTMP2 = ABS(DBLE(DT))
      OAD_CTMP3 = (DBLE(DTB) **(1.0D00 / FLOAT(I)))
      CALL oad_s_min_r(OAD_CTMP2, OAD_CTMP3, OAD_CTMP1)
      DT = SIGN(DBLE(OAD_CTMP1), DBLE(DT))
      GO TO 246
246   CONTINUE
      IF(KFLAG .LE. INT(1_w2f__i8)) THEN
        GO TO 249
      ELSE
        GO TO 247
      ENDIF
247   CONTINUE
      GO TO 248
248   CONTINUE
      WRITE(JOUT, '(5H ITER,I4,3X,4HH = ,F12.3,6H, T = ,F12.3,     ' //
     >  '            12H, NEXT DT = ,F12.5 )') I, H1, PTPEN(JW, 2), DT
      GO TO 249
249   CONTINUE
      H2 = H1
      PTPEN(INT(JW), 2) = (DBLE(PTPEN(JW, 2)) + DBLE(DT))
      GO TO 250
250   CONTINUE
      IF(DBLE(DTOL) .GE. ABS(DBLE(DT))) THEN
        GO TO 253
      ELSE
        GO TO 251
      ENDIF
251   CONTINUE
      GO TO 252
252   CONTINUE
      GO TO 243
253   CONTINUE
      PTPEN(INT(JW), 3) = HS
      GO TO 254
254   CONTINUE
      RETURN
      GO TO 255
255   CONTINUE
      END SUBROUTINE

      SUBROUTINE vmgas(FV, T, P, GASRES)
      use w2f__types
      use active_module
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
      REAL(w2f__4) OAD_CTMP0
      EXTERNAL oad_s_max_r
      REAL(w2f__4) SUM
      REAL(w2f__4) TA
      REAL(w2f__4) TC
      INTEGER(w2f__i4) T__68
      REAL(w2f__4) VC
      REAL(w2f__4) ZA
      REAL(w2f__4) ZB
      REAL(w2f__4) ZC
      INTEGER(w2f__i4) t__76
C
C     **** Statements ****
C
390   CONTINUE
      GO TO 391
391   CONTINUE
      SUM = 0.0D00
      TC = 0.0D00
      VC = 0.0D00
      ZC = 0.0D00
      TA = (DBLE(T) + 4.59670013429999983146D+02)
      T__68 = NCP
      t__76 = T__68
      GO TO 392
392   CONTINUE
      I = 1
      GO TO 398
393   CONTINUE
      I = I + 1
398   CONTINUE
      IF(I .LE. t__76) THEN
        GO TO 396
      ELSE
        GO TO 394
      ENDIF
394   CONTINUE
      TC = (DBLE(TC) / DBLE(SUM))
      VC = (DBLE(VC) / DBLE(SUM))
      ZC = (DBLE(ZC) / DBLE(SUM))
      VC = ((DBLE(TC) * DBLE(ZC) * 1.07334995269999993184D+01) / DBLE(
     > VC))
      TC = (DBLE(TA) / DBLE(TC))
      SUM = (DBLE(P) / DBLE(VC))
      ZA = (1.0D00 - DBLE(SUM) * EXP(DBLE(TC) *(-4.0D00)) *
     >  1.93799991610000006403D+01)
      BZ = (-4.09279996529999991889D-03)
      ZB = (DBLE(BZ) *(DBLE(SUM) ** INT(2_w2f__i8)) + DBLE(SUM) *
     >  1.54019996520000007623D-01 +(DBLE(BZ) / 1.0D+01))
      CALL oad_s_max_r(ZA, ZB, OAD_CTMP0)
      ZC = OAD_CTMP0
      GASRES = ((DBLE(TA) * DBLE(ZC) * 1.07334995269999993184D+01) /
     >  DBLE(P))
      RETURN
      GO TO 395
395   CONTINUE
396   CONTINUE
      SUM = (DBLE(FV(I)) + DBLE(SUM))
      TC = (DBLE(TC) + DBLE(FV(I)) * DBLE(CSXX(I, 1)))
      VC = (DBLE(VC) +((DBLE(FV(I)) * DBLE(CSXX(I, 22))) /
     >  6.24300003050000000826D+01))
      ZC = (DBLE(ZC) + DBLE(FV(I)) * DBLE(CSXX(I, 21)))
      GO TO 397
397   CONTINUE
      GO TO 393
      END SUBROUTINE

      SUBROUTINE vmliq(FL, T, LIQRES)
      use w2f__types
      use active_module
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
      REAL(w2f__4) OAD_CTMP0
      REAL(w2f__4) OAD_CTMP1
      EXTERNAL oad_s_max_r
      REAL(w2f__4) SUM
      REAL(w2f__4) TS
      INTEGER(w2f__i4) T__69
      INTEGER(w2f__i4) t__77
C
C     **** Statements ****
C
399   CONTINUE
      GO TO 400
400   CONTINUE
      TS = (DBLE(T) / 1.0D+03)
      SUM = 0.0D00
      LIQRES = 0.0D00
      T__69 = NCP
      t__77 = T__69
      GO TO 401
401   CONTINUE
      I = 1
      GO TO 407
402   CONTINUE
      I = I + 1
407   CONTINUE
      IF(I .LE. t__77) THEN
        GO TO 405
      ELSE
        GO TO 403
      ENDIF
403   CONTINUE
      LIQRES = (DBLE(LIQRES) / DBLE(SUM))
      RETURN
      GO TO 404
404   CONTINUE
405   CONTINUE
      OAD_CTMP1 = (DBLE(CSXX(I, 16)) + DBLE(TS) *(DBLE(CSXX(I, 17)) +
     >  DBLE(CSXX(I, 18)) * DBLE(TS)))
      CALL oad_s_max_r(OAD_CTMP1, 1.0D00, OAD_CTMP0)
      DLI = OAD_CTMP0
      LIQRES = (DBLE(LIQRES) +((DBLE(FL(I)) * DBLE(CSXX(I, 6))) / DBLE(
     > DLI)))
      SUM = (DBLE(FL(I)) + DBLE(SUM))
      GO TO 406
406   CONTINUE
      GO TO 402
      END SUBROUTINE

      SUBROUTINE wtmol(JW)
      use w2f__types
      use active_module
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
      INTEGER(w2f__i4) T__70
      REAL(w2f__4) WM
      INTEGER(w2f__i4) t__78
C
C     **** Statements ****
C
122   CONTINUE
      GO TO 123
123   CONTINUE
      GO TO 124
124   CONTINUE
      IF(DBLE(WTEN(JW)) .LE. 1.00000001339999999893D-10) THEN
        GO TO 127
      ELSE
        GO TO 125
      ENDIF
125   CONTINUE
      GO TO 126
126   CONTINUE
      MDO = .FALSE.
      GO TO 128
127   CONTINUE
      MDO = .TRUE.
      WTEN(INT(JW)) = 0.0D00
      GO TO 128
128   CONTINUE
      WM = 0.0D00
      T__70 = NCPS
      t__78 = T__70
      GO TO 129
129   CONTINUE
      I = 1
      GO TO 143
130   CONTINUE
      I = I + 1
143   CONTINUE
      IF(I .LE. t__78) THEN
        GO TO 137
      ELSE
        GO TO 131
      ENDIF
131   CONTINUE
      IF(DBLE(WTEN(JW)) .LE. 0.0D00) THEN
        GO TO 134
      ELSE
        GO TO 132
      ENDIF
132   CONTINUE
      GO TO 133
133   CONTINUE
      PTPEN(INT(JW), 6) = (DBLE(WM) / DBLE(WTEN(JW)))
      GO TO 135
134   CONTINUE
      PTPEN(INT(JW), 6) = 0.0D00
      GO TO 135
135   CONTINUE
      RETURN
      GO TO 136
136   CONTINUE
137   CONTINUE
      IF(MDO) THEN
        GO TO 139
      ELSE
        GO TO 138
      ENDIF
138   CONTINUE
      GO TO 140
139   CONTINUE
      WTEN(INT(JW)) = (DBLE(WTEN(JW)) + DBLE(WEN(JW, I)))
      GO TO 140
140   CONTINUE
      GO TO 141
141   CONTINUE
      WM = (DBLE(WM) + DBLE(CSXX(I, 6)) * DBLE(WEN(JW, I)))
      GO TO 142
142   CONTINUE
      GO TO 130
      END SUBROUTINE
