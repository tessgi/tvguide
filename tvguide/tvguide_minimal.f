!*==TVGUIDE_MINIMAL
      subroutine TVGUIDEF(in_ra, in_dec, dstart, intvls)

      IMPLICIT NONE
Cf2py intent(in) :: alpha, delta 
Cf2py intent(out) :: intvls

!      A provisional subroutine that provides
!      information about TESS visibility of potential targets
!      
!      Originally written by Koji Mukai for the TESS Science Support
!      Center 2017.
!      
!      Tom Barclay - I modfied for use in in a python module using f2py

*-TVGUIDEF
    
      DOUBLE PRECISION DEG2RAD , RAD2DEG, VEC_ANGLE
      PARAMETER (DEG2RAD=0.01745329252D+00)
      PARAMETER (RAD2DEG=57.29577951D+00)

      DOUBLE PRECISION lambda1 , step, dstart
      DOUBLE PRECISION edge , corner , gapx , gapy
      DOUBLE PRECISION in_ra , in_dec , alpha , delta , jd
      DOUBLE PRECISION lambda , beta , lambda_ , beta0 , lambda0
      DOUBLE PRECISION betas(4)
      DOUBLE PRECISION c1limit , c1gap , c2limit
      DOUBLE PRECISION cc(3) , targt(3) , locx(3) , locy(3)
      DOUBLE PRECISION tgt_cc(3) , offset , offsetx , offsety , cosoff
      DOUBLE PRECISION locxang , locyang, x

      INTEGER JULIAN
      INTEGER intvls( 13 )
      INTEGER i, j, k, totint

      INCLUDE 'tvguide.inc'

      jd = DBLE(JULIAN(2000,1,1)) - 0.5D+00
      locx(1) = 0.0D+00
      locx(2) = 1.0D+00
      locx(3) = 0.0D+00
      locy(2) = 0.0D+00

      alpha = in_ra * 0.06666666666
      delta = in_dec

      totint = 0
      DO j = 1 , 13
         intvls(j) = 0
      ENDDO

      CALL TO_ECLIP(alpha,delta,jd,lambda,beta)

      lambda1 = lambda1 + dstart

      IF ( beta.LE.c1limit ) THEN
         DO j = 1 , 13
            lambda0 = lambda1 + (j-1)*step
            lambda_ = lambda - lambda0
            CALL ECL_CARTS(lambda_,beta,targt)
            DO k = 1 , 4
               CALL ECL_CARTS(0.0D+00,betas(k),cc)
               offset = VEC_ANGLE(targt,cc)
               cosoff = COS(offset*DEG2RAD)
               IF ( offset.LE.corner ) THEN
                  DO i = 1 , 3
                     tgt_cc(i) = targt(i)/cosoff - cc(i)
                  ENDDO
                  CALL VEC_RENRM(tgt_cc)
                  beta0 = betas(k)*DEG2RAD
                  locy(1) = -SIN(beta0)
                  locy(3) = COS(beta0)
                  locxang = VEC_ANGLE(tgt_cc,locx)*DEG2RAD
                  locyang = VEC_ANGLE(tgt_cc,locy)
                  offsetx = offset*COS(locxang)
                  IF ( locyang.LE.90.0 ) THEN
                     offsety = offset*SIN(locxang)
                  ELSE
                     offsety = -offset*SIN(locxang)
                  ENDIF
                  IF ( ABS(offsetx) .LE. edge
     &                .AND. ABS(offsety) .LE. edge ) THEN
                     x = x +1
                     IF ( ABS(offsetx).LE.gapx
     &                   .OR. ABS(offsety) .LE. gapy ) THEN
                        x = x+1
                     ELSE
                        intvls(j) = k
                        totint = totint + 1
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDIF

      return
      END
             




