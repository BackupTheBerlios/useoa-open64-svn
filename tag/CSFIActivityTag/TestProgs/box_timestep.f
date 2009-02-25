CUN ***************************************************
CUN 
CUN Only the globals module and box_timestep routine
CUN 
CUN ***************************************************


      module all_globals_mod
        integer ndim
        parameter ( ndim = 3 )

        integer n_max

        double precision blength(ndim)
        double precision bheight(ndim)
        double precision bwidth
        double precision area(ndim)
        double precision vol(ndim)

        double precision y(2*ndim) 
        double precision r(2*ndim) 
        double precision r1(2*ndim) 
        double precision r_t(2*ndim) 
        double precision r_s(2*ndim) 
        double precision proj_t(2*ndim) 
        double precision proj_s(2*ndim) 
        double precision x(2*ndim,2*ndim)

        double precision alpha
        double precision beta

        double precision u0
        double precision delta

        double precision robert_filter_coeff

        double precision delta_t

        double precision hundred
        double precision thousand
        double precision day
        double precision year
        double precision Sv
        double precision days_per_50m_mixed_layer
        double precision gamma_T
        double precision gamma_S
        double precision epsilon_ic
        double precision noise_correlation_time
        double precision integration_time
        double precision epsilon_regularize
        double precision fdeps

        logical verbmode

        double precision thc_tot, thc_t, thc_s

        double precision 
     &                 told(ndim)
     &               , tnow(ndim)
     &               , tnew(ndim)
     &               , sold(ndim)
     &               , snow(ndim)
     &               , snew(ndim)

        double precision uvel

        double precision rho(ndim)

        double precision nullForce(ndim-1)
        double precision fw(ndim-1)
        double precision tStar(ndim-1)
        double precision sStar(ndim-1)

        double precision ubar, t(ndim), s(ndim)

c-- dependent and independent variables

        double precision metric1, metric2

        double precision metric

        double precision xx(2*ndim)

        double precision tsvec(2*ndim)

      end module


c-----------------------------------------------------------------------
      subroutine box_timestep ( 
     &     gammaLoc, fldStar, extForLoc, 
     &     uVelLoc, fldNow, fldOld, fldNew )
c-----------------------------------------------------------------------
      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      double precision uVelLoc
      double precision gammaLoc
      double precision fldStar(ndim-1)
      double precision extForLoc(ndim-1)
      double precision fldNow(ndim)
      double precision fldNew(ndim)
      double precision fldOld(ndim)
      character        ytstype*1
      
c-- local variables:
      double precision dFldDt(ndim)
cph      double precision velsign

c-- routine body

cph the following block simulates the bug in the matlab code
cph      if ( ytstype .EQ. 'T' ) then
cph         velsign = 1.D0
cph      else
cph         velsign = -1.D0
cph      endif

      if ( uVelLoc .GE. 0. ) then
         dFldDt(1) = 
     &        ( extForLoc(1)
     &        + gammaLoc*( fldStar(1) - fldNow(1) )*vol(1)
     &        + uVelLoc* ( fldNow(3)  - fldNow(1) ) ) / vol(1)
         dFldDt(2) = 
     &        ( extForLoc(2)
     &        + gammaLoc*( fldStar(2) - fldNow(2) )*vol(2)
     &        + uVelLoc* ( fldNow(1)  - fldNow(2) ) ) / vol(2)
         dFldDt(3) = 
     &        uVelLoc*( fldNow(2) - fldNow(3) ) / vol(3)
      else
         dFldDt(1) = 
     &        ( extForLoc(1)
     &        + gammaLoc*( fldStar(1) - fldNow(1) )*vol(1)
     &        - uVelLoc* ( fldNow(2) -  fldNow(1) ) ) / vol(1)
         dFldDt(2) = 
     &        ( extForLoc(2)
     &        + gammaLoc*( fldStar(2) - fldNow(2) )*vol(2)
     &        - uVelLoc* ( fldNow(3) -  fldNow(2) ) ) / vol(2)
         dFldDt(3) = 
     &        -uVelLoc*( fldNow(1) - fldNow(3) ) / vol(3)
      end if

      call box_update ( fldNew, fldOld, dFldDt )

      end

c-----------------------------------------------------------------------
      subroutine box_update ( fldNew, fldOld, dFldDt )
c-----------------------------------------------------------------------
      use all_globals_mod


      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      
      double precision fldNew(ndim)
      double precision fldOld(ndim)
      double precision dFldDt(ndim)

c-- local variables:

c-- routine body

      do l = 1, ndim
         fldNew(l) = fldOld(l) + 2.D0*delta_t*dFldDt(l)
      enddo

      end


