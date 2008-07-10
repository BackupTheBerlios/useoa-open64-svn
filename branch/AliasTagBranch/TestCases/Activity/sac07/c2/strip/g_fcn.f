C                           DISCLAIMER
C
C   This file was generated on 06/04/96 by the version of
C   ADIFOR compiled on May 24 1996.
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
C     File:  FCN.f
C
      subroutine g_fcn(g_p_, x, y, g_y, ldg_y, yp, g_yp, ldg_yp)
C
C     ROUTINE TO EVALUATE THE DERIVATIVE F(X,Y) CORRESPONDING
C     TO THE DIFFERENTIAL EQUATION:
C                    DY/DX = F(X,Y) .
C     THE ROUTINE STORES THE VECTOR OF DERIVATIVES IN YP(*). 
C
        double precision x, y(20), yp(20)
        integer id, iwt, n
        double precision w(20)
        common /stcom5/ w, iwt, n, id
        double precision sum, cparm(4), ytemp(20)
        integer i, iid
        integer g_pmax_
        parameter (g_pmax_ = 5)
        integer g_i_, g_p_, ldg_y, ldg_yp
        double precision d1_p, d1_w, d5_b, d4_b, d2_v, d2_b, g_ytemp(g_p
     *max_, 20), g_y(ldg_y, 20), g_yp(ldg_yp, 20), g_sum(g_pmax_)
        double precision g_d1_w(g_pmax_)
        save g_ytemp, g_sum, g_d1_w
        intrinsic dble
        data cparm /1.d-1, 1.d0, 1.d1, 2.d1/
C
        integer g_ehfid
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'fcn','g_fcn.f')
C
        if (g_p_ .gt. g_pmax_) then
          print *, 'Parameter g_p_ is greater than g_pmax_'
          stop
        endif
        if (iwt .lt. 0) then
          goto 40
        endif
        do 99999 i = 1, n
          do g_i_ = 1, g_p_
            g_ytemp(g_i_, i) = g_y(g_i_, i)
          enddo
          ytemp(i) = y(i)
C--------
          do g_i_ = 1, g_p_
            g_y(g_i_, i) = w(i) * g_y(g_i_, i)
          enddo
          y(i) = y(i) * w(i)
C--------
20        continue
99999   continue
40      iid = mod(id, 10)
C
C     ADAPTED FROM PROBLEM C2
        do g_i_ = 1, g_p_
          g_yp(g_i_, 1) = -g_y(g_i_, 1)
        enddo
        yp(1) = -y(1) + 2.d0
C--------
        d2_b = y(1) + y(1)
        do g_i_ = 1, g_p_
          g_sum(g_i_) = d2_b * g_y(g_i_, 1)
        enddo
        sum = y(1) * y(1)
C--------
        do 99998 i = 2, n
          d4_b = cparm(iid - 1) * dble(2 ** i)
          d5_b = (-10.0d0) * dble(i)
          do g_i_ = 1, g_p_
            g_yp(g_i_, i) = d4_b * g_sum(g_i_) + d5_b * g_y(g_i_, i)
          enddo
          yp(i) = (-10.0d0) * dble(i) * y(i) + cparm(iid - 1) * dble(2 *
     ** i) * sum
C--------
          d2_v = sum * sum
          d1_p = 2.0d0 * sum
          d4_b = y(i) + y(i)
          do g_i_ = 1, g_p_
            g_d1_w(g_i_) = d4_b * g_y(g_i_, i) + d1_p * g_sum(g_i_)
          enddo
          d1_w = d2_v + y(i) * y(i)
          d2_v = sqrt(d1_w)

          if ( d1_w .gt. 0.0d0 ) then
             d1_p = 1.0d0 / (2.0d0 *  d2_v)
          else
             call ehufDO (9,d1_w, d2_v, d1_p,
     +g_ehfid,
     +103)
          endif
          do g_i_ = 1, g_p_
            g_sum(g_i_) = d1_p * g_d1_w(g_i_)
          enddo
          sum = d2_v
C--------
50        continue
99998   continue
C
        if (iwt .lt. 0) then
          goto 680
        endif
        do 99997 i = 1, n
          d2_b = 1.0d0 / w(i)
          do g_i_ = 1, g_p_
            g_yp(g_i_, i) = d2_b * g_yp(g_i_, i)
          enddo
          yp(i) = yp(i) / w(i)
C--------
          do g_i_ = 1, g_p_
            g_y(g_i_, i) = g_ytemp(g_i_, i)
          enddo
          y(i) = ytemp(i)
C--------
660       continue
99997   continue
680     continue
        return
      end
