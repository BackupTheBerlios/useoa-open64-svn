C                           DISCLAIMER
C
C   This file was generated on 01/06/99 by the version of
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
      subroutine s_dmsasep(nx, ny, x, s_x, fvec, s_fvec, bottom, top, le
     *ft, right, elem)
        integer nx, ny, elem
        double precision fvec(*)
        double precision x(nx * ny), bottom(nx + 2), top(nx + 2), left(n
     *y + 2), right(ny + 2)
C     **********
C
C     Subroutine dmsafg
C
C     This subroutine computes the function and gradient of the
C     minimal surface area problem.
C
C     The subroutine statement is
C
C       subroutine dmsafg(nx,ny,x,f,fgrad,task,bottom,top,left,right)
C
C     where
C
C       nx is an integer variable.
C         On entry nx is the number of grid points in the first
C            coordinate direction.
C         On exit nx is unchanged.
C
C       ny is an integer variable.
C         On entry ny is the number of grid points in the second
C            coordinate direction.
C         On exit ny is unchanged.
C
C       x is a double precision array of dimension nx*ny.
C         On entry x specifies the vector x if task = 'F', 'G', or 'FG'.
C            Otherwise x need not be specified.
C         On exit x is unchanged if task = 'F', 'G', or 'FG'. Otherwise
C            x is set according to task.
C
C       f is a double precision variable.
C         On entry f need not be specified.
C         On exit f is set to the function evaluated at x if task = 'F'
C            or 'FG'.
C
C       fgrad is a double precision array of dimension nx*ny.
C         On entry fgrad need not be specified.
C         On exit fgrad contains the gradient evaluated at x if
C            task = 'G' or 'FG'.
C
C       task is a character variable.
C         On entry task specifies the action of the subroutine:
C
C            task               action
C            ----               ------
C             'F'     Evaluate the function at x.
C             'G'     Evaluate the gradient at x.
C             'FG'    Evaluate the function and the gradient at x.
C             'XS'    Set x to the standard starting point xs.
C
C         On exit task is unchanged.
C
C       bottom is a double precision array of dimension nx + 2.
C         On entry bottom must contain boundary data beginning
C            with the lower left corner of the domain.
C         On exit bottom is unchanged.
C
C       top is a double precision array of dimension nx + 2.
C         On entry top must contain boundary data beginning with
C            the upper left corner of the domain.
C         On exit top is unchanged.
C
C       left is a double precision array of dimension ny + 2.
C         On entry left must contain boundary data beginning with
C            the lower left corner of the domain.
C         On exit left is unchanged.
C
C       right is a double precision array of dimension ny + 2.
C         On entry right must contain boundary data beginning with
C            the lower right corner of the domain.
C         On exit right is unchanged.
C
C     MINPACK-2 Project. November 1995.
C     Argonne National Laboratory.
C     Ali Bouaricha
C
C     **********
        double precision one, p5, two, zero
        parameter (zero = 0.0d0, p5 = 0.5d0, one = 1.0d0, two = 2.0d0)
C
        integer i, j, k
        double precision area, dvdx, dvdy, hx, hy, v, vb, vl, vr, vt
C
C     Initialize.
C
        integer s_v, s_x(nx * ny), s_vr, s_vt, s_dvdx, s_dvdy, s_d1_w, s
     *_fvec(*), s_vb, s_vl
        double precision d2_p, d2_v, d1_p, d1_w, d5_v, d2_b
        integer g_ehfid
        save s_v, s_vr, s_vt, s_dvdx, s_dvdy, s_d1_w, s_vb, s_vl
        data g_ehfid /0/
C
        call ehsfid(g_ehfid, 'dmsasep','s_dmsasep.f')
C
        hx = one / dble(nx + 1)
        hy = one / dble(ny + 1)
        area = p5 * hx * hy
C
C     Computation of the function and gradient over the lower
C     triangular elements.
C
        elem = 0
        do 99998 j = 0, ny
          do 99999 i = 0, nx
            k = nx * (j - 1) + i
            if (i .ge. 1 .and. j .ge. 1) then
              call dspcpq(s_v, s_x(k))                         
              v = x(k)
C--------
            else
              if (j .eq. 0) then
                call dspzro(s_v)                              
                v = bottom(i + 1)
C--------
              endif
              if (i .eq. 0) then
                call dspzro(s_v)                              
                v = left(j + 1)
C--------
              endif
            endif
            if (i .lt. nx .and. j .gt. 0) then
              call dspcpq(s_vr, s_x(k + 1))                    
              vr = x(k + 1)
C--------
            else
              if (i .eq. nx) then
                call dspzro(s_vr)                             
                vr = right(j + 1)
C--------
              endif
              if (j .eq. 0) then
                call dspzro(s_vr)                             
                vr = bottom(i + 2)
C--------
              endif
            endif
            if (i .gt. 0 .and. j .lt. ny) then
              call dspcpq(s_vt, s_x(k + nx))                   
              vt = x(k + nx)
C--------
            else
              if (i .eq. 0) then
                call dspzro(s_vt)                             
                vt = left(j + 2)
C--------
              endif
              if (j .eq. ny) then
                call dspzro(s_vt)                             
                vt = top(i + 1)
C--------
              endif
            endif
            d2_b = 1.0d0 / hx
            call dspg2q(s_dvdx, -d2_b, s_v, d2_b, s_vr)        
            dvdx = (vr - v) / hx
C--------
            d2_b = 1.0d0 / hy
            call dspg2q(s_dvdy, -d2_b, s_v, d2_b, s_vt)        
            dvdy = (vt - v) / hy
C--------
            elem = elem + 1
            d2_v = dvdx * dvdx
            d2_p = 2.0d0 * dvdx
            d5_v = dvdy * dvdy
            d1_p = 2.0d0 * dvdy
            call dspg2q(s_d1_w, d1_p, s_dvdy, d2_p, s_dvdx)    
            d1_w = one + d2_v + d5_v
            d2_v = sqrt(d1_w)

            if ( d1_w .gt. 0.0d0 ) then
               d1_p = 1.0d0 / (2.0d0 *  d2_v)
            else
               call ehufDO (9,d1_w, d2_v, d1_p,
     +g_ehfid,
     +196)
            endif
            call dspg1q(s_fvec(elem), area * d1_p, s_d1_w)     
            fvec(elem) = area * d2_v
C--------
40          continue
99999     continue
50        continue
99998   continue
C
C     Computation of the function and the gradient over the upper
C     triangular elements.
C
        do 99996 j = 1, ny + 1
          do 99997 i = 1, nx + 1
            k = nx * (j - 1) + i
            if (i .le. nx .and. j .gt. 1) then
              call dspcpq(s_vb, s_x(k - nx))                   
              vb = x(k - nx)
C--------
            else
              if (j .eq. 1) then
                call dspzro(s_vb)                             
                vb = bottom(i + 1)
C--------
              endif
              if (i .eq. nx + 1) then
                call dspzro(s_vb)                             
                vb = right(j)
C--------
              endif
            endif
            if (i .gt. 1 .and. j .le. ny) then
              call dspcpq(s_vl, s_x(k - 1))                    
              vl = x(k - 1)
C--------
            else
              if (j .eq. ny + 1) then
                call dspzro(s_vl)                             
                vl = top(i)
C--------
              endif
              if (i .eq. 1) then
                call dspzro(s_vl)                             
                vl = left(j + 1)
C--------
              endif
            endif
            if (i .le. nx .and. j .le. ny) then
              call dspcpq(s_v, s_x(k))                         
              v = x(k)
C--------
            else
              if (i .eq. nx + 1) then
                call dspzro(s_v)                              
                v = right(j + 1)
C--------
              endif
              if (j .eq. ny + 1) then
                call dspzro(s_v)                              
                v = top(i + 1)
C--------
              endif
            endif
            d2_b = 1.0d0 / hx
            call dspg2q(s_dvdx, -d2_b, s_vl, d2_b, s_v)        
            dvdx = (v - vl) / hx
C--------
            d2_b = 1.0d0 / hy
            call dspg2q(s_dvdy, -d2_b, s_vb, d2_b, s_v)        
            dvdy = (v - vb) / hy
C--------
            elem = elem + 1
            d2_v = dvdx * dvdx
            d2_p = 2.0d0 * dvdx
            d5_v = dvdy * dvdy
            d1_p = 2.0d0 * dvdy
            call dspg2q(s_d1_w, d1_p, s_dvdy, d2_p, s_dvdx)    
            d1_w = one + d2_v + d5_v
            d2_v = sqrt(d1_w)

            if ( d1_w .gt. 0.0d0 ) then
               d1_p = 1.0d0 / (2.0d0 *  d2_v)
            else
               call ehufDO (9,d1_w, d2_v, d1_p,
     +g_ehfid,
     +282)
            endif
            call dspg1q(s_fvec(elem), area * d1_p, s_d1_w)     
            fvec(elem) = area * d2_v
C--------
60          continue
99997     continue
70        continue
99996   continue
C
C     Scale the function and the gradient.
C
      end
