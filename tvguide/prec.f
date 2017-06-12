*+ARK_PREC
        subroutine ARK_PREC
     &                   ( alpha, delta, equinox, jd, p_alpha, p_delta )

        implicit none

        double precision pi, pi_180, radian, fifteen
        parameter( pi = 3.14159265358979323846264338328d+00 )
        parameter( pi_180 = 0.0174532925177210736d+00 )
        parameter( radian = 57.29577951d+00 )
        parameter( fifteen = 1.5d+01 )

        double precision alpha, delta
        double precision equinox, jd
        double precision p_alpha, p_delta

C
C	Precession programme using the "rigorous formula" in the
C	Astronomical Almanac (86).  alpha in decimal hours, delta
C	in decimal degrees, equinox is in years
C
        double precision r_alpha, r_delta
c						! in radian
        double precision zeta, zed, theta, T
        double precision cos_theta, sin_theta
        double precision cos_delta, sin_delta
        double precision X, Y, Z
        double precision work

*       Convert alpha and delta to radians
        r_alpha = alpha * fifteen * pi_180
        r_delta = delta * pi_180

*       Precess to J2000.0
        t = ( equinox - 2.0D+03 ) * 1.0D-02
        call PREC_ANGLES( t, zeta, zed, theta )
        r_alpha = r_alpha - zed
        cos_theta = cos( theta )
        sin_theta = sin( theta )
        cos_delta = cos( r_delta )
        sin_delta = sin( r_delta )
        work = cos( r_alpha )
        x = sin( r_alpha ) * cos_delta
        y = sin_theta * sin_delta + work * cos_theta * cos_delta
        z = cos_theta * sin_delta - work * sin_theta * cos_delta
        call THREED_ANGLES( x, y, z, r_alpha, r_delta )
        r_alpha = r_alpha - zeta

*       Next, change to the chosen equinox
        t = ( jd - 2.4515445D+06 ) / 3.6525D+04
        call PREC_ANGLES( t, zeta, zed, theta )
        r_alpha = r_alpha + zeta
        cos_theta = cos( theta )
        sin_theta = sin( theta )
        cos_delta = cos( r_delta )
        sin_delta = sin( r_delta )
        work = cos( r_alpha )
        x = sin( r_alpha ) * cos_delta
        y = work * cos_theta * cos_delta - sin_theta * sin_delta
        z = work * sin_theta * cos_delta + cos_theta * sin_delta
        call THREED_ANGLES( x, y, z, r_alpha, r_delta )
        r_alpha = r_alpha + zed + pi * 2.0
        r_alpha = mod( r_alpha, pi * 2.0D+00 )
C		change into decimal hours/degrees
        p_alpha = r_alpha * radian / fifteen
        p_delta = r_delta * radian
C
        end

*+PREC_ANGLES
        subroutine PREC_ANGLES( t, zeta, zed, theta )

        implicit none

        double precision t
        double precision zeta, zed, theta
c				! Time since J2000.0
C

        zeta = ( ( 8.72664626D-08 * t + 1.464331242D-06 ) * t
     1				+ 1.118086019D-02 ) * t
        zed = ( ( 8.901179185D-08 * t + 5.307546255D-06 ) * t
     1				+ 1.118086019D-02 ) * t
        theta = ( ( -2.024581932D-07 * t - 2.068215164D-06 ) * t
     1				+ 9.71717297D-03 ) * t
C
        end

*+THREED_ANGLES
        subroutine THREED_ANGLES( x, y, z, r_alpha, r_delta )

        implicit none

        double precision pi
        Parameter( pi = 3.1415926536 )

        double precision x, y, z
        double precision r_alpha, r_delta
c						! Orthogonal Vector
c						! Radians
C
        double precision cos_delta, temp

        if( z .gt. 1.0D+00 ) then
          z = 1.0D+00
        else if( z .lt. -1.0D+00 ) then
          z = -1.0D+00
        end if
        r_delta = asin( z )
        cos_delta = sqrt( 1.0 - z * z )
        if( cos_delta .gt. 0.0D+00 ) then
          temp = y / cos_delta
          if( abs( temp ) .gt. 1.0D+00 ) then
            r_alpha = 0.0D+00
          else
            if( x .ge. 0.0D+00 ) then
              r_alpha = acos( temp )
            else
              r_alpha = pi * 2.0D+00 - acos( temp )
            end if
          end if
        else
          r_alpha = 0.0D+00
        end if

        end
