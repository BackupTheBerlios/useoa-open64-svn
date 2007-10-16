C                           DISCLAIMER
C
C   This file was generated on 10/26/98 by the version of
C   ADIFOR compiled on June, 1998.
C
C   ADIFOR was prepared as an account of work sponsored by an
C   agency of the United States Government, Rice University, and
C   the University of Chicago.  NEITHER THE AUTHOR(S), THE UNITED
C   STATES GOVERNMENT NOR ANY AGENCY THEREOF, NOR RICE UNIVERSITY,
C   NOR THE UNIVERSITY OF CHICAGO, INCLUDING ANY OF THEIR EMPLOYEES
C   OR OFFICERS, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES
C   ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETE-
C   NESS, OR USEFULNESS OF ANY INFORMATION OR PROCESS DISCLOSED, OR
C   REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
C
C
      subroutine g_tset(g_p_, jw, flag)
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
        common /unpt/ jout, kntrl, kflag, ncps, nptp, ncst, nrec, nen, w
     *ten(5), wen(5, 12), ptpen(5, 6), cost(5), en(100), ktln(15)
        common /flos/ fv(12), fl(12), fw(12)
C     COMMON /ACT/ WTEN(5),WEN(5,12),PTPEN(5,6),COST(5),EN(100)
C     COMMON /UNACT/ JOUT,KNTRL,KFLAG,NPTP,NCST,NREC,NEN,NCPS,KTLN(15)
        dimension t(2), h(2)
        integer g_pmax_
        parameter (g_pmax_ = 8)
        integer g_i_, g_p_, g_jout(g_pmax_), g_kntrl(g_pmax_), g_kflag(g
     *_pmax_), g_ncps(g_pmax_), g_nptp(g_pmax_), g_ncst(g_pmax_), g_nrec
     *(g_pmax_), g_nen(g_pmax_)
        integer g_ktln(g_pmax_, 15)
        real r3_w, r2_w, r1_w, r7_v, r7_b, r2_v, r3_v, r4_v, r5_v, r6_v
        real r8_v, r2_b, r3_b, r4_b, r5_b, r6_b, r1_p, r2_p, g_hs(g_pmax
     *_), g_ptpen(g_pmax_, 5, 6)
        real g_p(g_pmax_), g_t(g_pmax_, 2), g_h(g_pmax_, 2), g_x(g_pmax_
     *), g_dt(g_pmax_), g_h2(g_pmax_), g_h1(g_pmax_), g_r2_w(g_pmax_), g
     *_r3_w(g_pmax_), g_r1_w(g_pmax_)
        real g_wten(g_pmax_, 5), g_wen(g_pmax_, 5, 12), g_cost(g_pmax_, 
     *5), g_en(g_pmax_, 100)
        common /g_unpt/ g_jout, g_kntrl, g_kflag, g_ncps, g_nptp, g_ncst
     *, g_nrec, g_nen, g_wten, g_wen, g_ptpen, g_cost, g_en, g_ktln
        integer g_ehfid
        save g_r1_w, /g_unpt/
        save g_hs, g_p, g_t, g_h, g_x, g_dt, g_h2, g_h1, g_r2_w, g_r3_w
        external g_hmx
        external g_tbtd
        data dtol /1.e-5/, nlim /20/, dts /10./, dtb /1.e8/
C
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'tset','g_tset.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        do g_i_ = 1, g_p_
          g_hs(g_i_) = g_ptpen(g_i_, jw, 3)
        enddo
        hs = ptpen(jw, 3)
C--------
        if (kflag .gt. 1) then
C*ADIFOR* I/O statement contains active variables
          write (jout, 9000) hs
        endif
C                                           
C         CHOOSE OPTION, AND IDENTIFY PHASES IF NECESSARY 
        if (flag .ge. 0.) then
          goto 16
        endif
        do g_i_ = 1, g_p_
          g_p(g_i_) = g_ptpen(g_i_, jw, 1)
        enddo
        p = ptpen(jw, 1)
C--------
        call g_tbtd(g_p_, 3, jw, p, g_p, g_pmax_, t(1), g_t(1, 1), g_pma
     *x_, t(2), g_t(1, 2), g_pmax_, h(1), g_h(1, 1), g_pmax_, h(2), g_h(
     *1, 2), g_pmax_)
C      write(jout,1200)H(1),H(2)
C1200  format(2x,'hbub= ',F10.4,'hdew= ',F10.4)
        do 99999 j = 1, 2
          if (kprops .gt. 0) then
            goto 3
          endif
          do g_i_ = 1, g_p_
            g_ptpen(g_i_, jw, 2) = g_t(g_i_, j)
          enddo
          ptpen(jw, 2) = t(j)
C--------
          flg = float(j - 1)
          call g_hmx(g_p_, jw, flg)
          do g_i_ = 1, g_p_
            g_h(g_i_, j) = g_ptpen(g_i_, jw, 3)
          enddo
          h(j) = ptpen(jw, 3)
C--------
3         if (((h(j) - hs) * (-1.) ** j) .le. 0.) then
            goto 14
          endif
5         continue
99999   continue
C                                                         
C         TWO PHASES PRESENT                              
        r5_v = h(2) - h(1)
        r6_v = (hs - h(1)) / r5_v
        r2_b = 1.0 / r5_v
        r3_b = (-r6_v) / r5_v
        r5_b = -r3_b + (-r2_b)
        do g_i_ = 1, g_p_
          g_x(g_i_) = r2_b * g_hs(g_i_) + r5_b * g_h(g_i_, 1) + r3_b * g
     *_h(g_i_, 2)
        enddo
        x = r6_v
C--------
        if ((t(2) - t(1)) .lt. dtol) then
          goto 100
        endif
        r4_v = t(2) - t(1)
        do g_i_ = 1, g_p_
          g_dt(g_i_) = r4_v * g_x(g_i_) + x * g_t(g_i_, 2) + (-x) * g_t(
     *g_i_, 1)
        enddo
        dt = x * r4_v
C--------
        do g_i_ = 1, g_p_
          g_h2(g_i_) = g_h(g_i_, 1)
        enddo
        h2 = h(1)
C--------
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 2) = g_t(g_i_, 1) + g_dt(g_i_)
        enddo
        ptpen(jw, 2) = t(1) + dt
C--------
        goto 18
C                                                         
C         ONLY ONE PHASE PRESENT,- J=1 FOR LIQUID, J=2 FOR VAPOR  
14      do g_i_ = 1, g_p_
          g_h2(g_i_) = g_h(g_i_, j)
        enddo
        h2 = h(j)
C--------
        do g_i_ = 1, g_p_
          g_dt(g_i_) = 0.0
        enddo
        dt = dts * (-1.) ** j
C--------
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 2) = g_t(g_i_, j) + g_dt(g_i_)
        enddo
        ptpen(jw, 2) = t(j) + dt
C--------
        if (j .eq. 1) then
          flg = 0.
        endif
        if (j .eq. 2) then
          flg = 1.
        endif
        goto 20
C                             
C         SINGLE PHASE SPECIFIED BY FLAG      
16      if (abs(ptpen(jw, 2)) .lt. dtol) then
          r2_v = sign (dts, hs)

          if ((dts .eq. 0.0e0) .or. ( hs .eq. 0.0e0)) then
             call ehbfSO (5,dts, hs, r2_v, r1_p, r2_p,
     +g_ehfid,
     +179)
          else if (dts .gt. 0.0e0) then
             if ( hs .gt. 0.0e0) then
                r1_p = 1.0e0
             else
                r1_p = -1.0e0
             endif
          else
             if ( hs .gt. 0.0e0) then
                r1_p = -1.0e0
             else
                r1_p =  1.0e0
             endif
          endif

          r2_p = 0.0e0
          do g_i_ = 1, g_p_
            g_ptpen(g_i_, jw, 2) = r2_p * g_hs(g_i_)
          enddo
          ptpen(jw, 2) = r2_v
C--------
        endif
        do g_i_ = 1, g_p_
          g_dt(g_i_) = g_ptpen(g_i_, jw, 2)
        enddo
        dt = ptpen(jw, 2)
C--------
        do g_i_ = 1, g_p_
          g_h2(g_i_) = 0.0
        enddo
        h2 = 0.
C--------
18      flg = flag
C                                                               
C         ADJUST TEMPERATURE TO MATCH ENTHALPY SPEC, USING NEWTON'S METH
20      do 99998 i = 1, nlim
C      write(jout,1234)flg
C1234  format(2x,'tset: flg=',F5.2)
          call g_hmx(g_p_, jw, flg)
          do g_i_ = 1, g_p_
            g_h1(g_i_) = g_ptpen(g_i_, jw, 3)
          enddo
          h1 = ptpen(jw, 3)
C--------
          r3_v = hs - h1
          r7_v = h1 - h2
          r8_v = r3_v * dt / r7_v
          r2_b = 1.0 / r7_v
          r3_b = (-r8_v) / r7_v
          r6_b = r2_b * dt
          r7_b = r2_b * r3_v
          r4_b = r3_b + (-r6_b)
          do g_i_ = 1, g_p_
            g_dt(g_i_) = r6_b * g_hs(g_i_) + r4_b * g_h1(g_i_) + r7_b * 
     *g_dt(g_i_) + (-r3_b) * g_h2(g_i_)
          enddo
          dt = r8_v
C--------
          r2_v = abs(dt)

          if (dt .gt. 0.0e0) then
             r1_p =  1.0e0
          else if (dt .lt. 0.0e0) then
             r1_p = -1.0e0
          else
             call ehufSO (3,dt, r2_v, r1_p,
     +g_ehfid,
     +246)
          endif
          do g_i_ = 1, g_p_
            g_r2_w(g_i_) = r1_p * g_dt(g_i_)
          enddo
          r2_w = r2_v
          do g_i_ = 1, g_p_
            g_r3_w(g_i_) = 0.0
          enddo
          r3_w = (dtb ** (1. / float(i)))
          r3_v = min (r2_w, r3_w)

          if (r2_w .lt.  r3_w) then
             r1_p = 1.0e0
             r2_p = 0.0e0
          else if (r2_w .gt.  r3_w) then
             r1_p = 0.0e0
             r2_p = 1.0e0
          else
             call ehbfSO (8,r2_w, r3_w, r3_v, r1_p, r2_p,
     +g_ehfid,
     +267)
             r2_p = 1.0e0 -  r1_p
          endif
          do g_i_ = 1, g_p_
            g_r1_w(g_i_) = r1_p * g_r2_w(g_i_) + r2_p * g_r3_w(g_i_)
          enddo
          r1_w = r3_v
          r3_v = sign (r1_w, dt)

          if ((r1_w .eq. 0.0e0) .or. ( dt .eq. 0.0e0)) then
             call ehbfSO (5,r1_w, dt, r3_v, r1_p, r2_p,
     +g_ehfid,
     +279)
          else if (r1_w .gt. 0.0e0) then
             if ( dt .gt. 0.0e0) then
                r1_p = 1.0e0
             else
                r1_p = -1.0e0
             endif
          else
             if ( dt .gt. 0.0e0) then
                r1_p = -1.0e0
             else
                r1_p =  1.0e0
             endif
          endif

          r2_p = 0.0e0
          do g_i_ = 1, g_p_
            g_dt(g_i_) = r1_p * g_r1_w(g_i_) + r2_p * g_dt(g_i_)
          enddo
          dt = r3_v
C--------
          if (kflag .le. 1) then
            goto 25
          endif
C*ADIFOR* I/O statement contains active variables
          write (jout, 9025) i, h1, ptpen(jw, 2), dt
25        do g_i_ = 1, g_p_
            g_h2(g_i_) = g_h1(g_i_)
          enddo
          h2 = h1
C--------
          do g_i_ = 1, g_p_
            g_ptpen(g_i_, jw, 2) = g_ptpen(g_i_, jw, 2) + g_dt(g_i_)
          enddo
          ptpen(jw, 2) = ptpen(jw, 2) + dt
C--------
          if (abs(dt) .le. dtol) then
            goto 80
          endif
50        continue
99998   continue
C                                                                       
C*ADIFOR* I/O statement contains active variables
        write (jout, 9050) nlim, dt
80      do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 3) = g_hs(g_i_)
        enddo
        ptpen(jw, 3) = hs
C--------
        goto 900
C                                                                      
C         SINGLE COMPONENT, TWO PHASES                                 
100     do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 3) = g_hs(g_i_)
        enddo
        ptpen(jw, 3) = hs
C--------
        do g_i_ = 1, g_p_
          g_ptpen(g_i_, jw, 4) = g_x(g_i_)
        enddo
        ptpen(jw, 4) = x
C--------
900     return
C                                                                      
9050    format ('0AFTER',i8,' ITERATIONS, TEMP DIFF IS',g11.5,' DEG FAHR
     *')
9000    format (' FIND TEMP. TO MATCH ENTHY. OF',f12.3)
9025    format (' ITER',i4,3x,'H = ',f12.3,', T = ',f12.3,', NEXT DT = '
     *,f12.5)
C                                                                      
      end
