*+TO_ECLIP
        subroutine TO_ECLIP( ra, dec, jd, lambda, beta )

        implicit none

        double precision fox, fifteen
        parameter( fox = 2.415020d+06 )
        parameter( fifteen = 1.5d+01 )

        double precision ra, dec, jd, lambda, beta

*       Description:
*         Converts celestial coordinates (ra, dec), ra in decimal hours
*         and dec in decimal degrees, to ecliptic coordinates (lambda, beta)
*         for time = jd (unmodified Julian date)
*
*       Parameters/Arguments:
*         fox          (p) : JD at the beginning of the 20th century
*         fifteen      (p) : 15.0, in double precision
*         ra, dec      (i) : Celestial coordinates
*         jd           (i) : Time in unmodified Julian date
*         lambda, beta (o) : Ecliptic coordinates
*
*       Dependencies:
*         Duffet-Smith routine EQECL_DS below.
*
*       Origin:
*         KM's wrapper routine for DS library routine
*
*       Author:
*         Koji Mukai, 1993 Feb 08, First official release
*-TO_ECLIP

        double precision rad, djd

        djd = jd - fox
        rad = ra * fifteen
        call EQECL_DS( djd, rad, dec, +1, lambda, beta )

        end


*+FM_ECLIP
        subroutine FM_ECLIP( lambda, beta, jd, ra, dec )

        implicit none

        double precision fox, fifteen
        parameter( fox = 2.415020d+06 )
        parameter( fifteen = 1.5d+01 )

        double precision lambda, beta, jd, ra, dec

*       Description:
*         Reverse of TO_ECLIP above
*
*       Parameters/Arguments:
*         fox          (p) : JD at the beginning of the 20th century
*         fifteen      (p) : 15.0, in double precision
*         lambda, beta (i) : Ecliptic coordinates
*         jd           (i) : Time in unmodified Julian date
*         ra, dec      (o) : Celestial coordinates
*
*       Dependencies:
*         Duffet-Smith routine EQECL_DS below.
*
*       Origin:
*         KM's wrapper routine for DS library routine
*
*       Author:
*         Koji Mukai, 1993 Feb 08, First official release
*         Koji Mukai, 1993 Mar 16, ra and rad mix-up fixed.
*-TO_ECLIP

        double precision rad, djd

        djd = jd - fox
        call EQECL_DS( djd, lambda, beta, -1, rad, dec )
        ra = rad / fifteen

        end

*+EQECL_DS
        subroutine EQECL_DS( djd, x, y, switch, p, q )

        implicit none

        double precision deg2rad
        parameter( deg2rad = 1.74532925199433d-02 )
*                          = pi / 180, converts from degrees to radians

        double precision d_psi, d_eps, rcp, rsp, height
        common / astro1 / d_psi, d_eps, rcp, rsp, height

        double precision djd, x, y, p, q
        integer switch

*       Description:
*         Bottom level routine for Celestial/Ecliptic conversion
*
*       Common Block Variables/Arguments:
*         d_psi, d_eps    (c) : nutation parameters
*         rcp, rsp        (c) : Something to do with horizontal parallax
*         height          (c) : Obervatory altitude
*         djd             (i) : JD since beginning of 20th century
*         x, y            (i) : Input coordinates
*         switch          (i) : +1 for celestial to ecliptic, -1 for reverse
*         p, q            (o) : Output coorddinates
*
*       Dependencies:
*         SET_NUTATE, another D-S routine
*
*       Origin:
*         Duffet-Smith p.59
*
*       Author
*         Koji Mukai, 1993 Feb 08, first official release
*-EQECL_DS

        double precision t, c, eps, eps1, s_eps, c_eps
        double precision sy, cy, sx, cx, sq, a, ty

        call SET_NUTATE( djd )

*       from subroutine OBLIQ, p.51
        t = djd / 3.6525d+04
        c = ( ( ( -1.81d-03 * t ) + 5.9d-03 ) * t + 4.6845d+01 ) * t
	eps = 2.345229444d+01 - ( c / 3.6d+03 )
*       end of OBLIQ

        eps1 = eps + d_eps
        s_eps = sin( eps1 * deg2rad )
        c_eps = cos( eps1 * deg2rad )

        cy = cos( y * deg2rad )
        sy = sin( y * deg2rad )

        if( abs( cy ) .lt. 1.0d-20 ) cy = 1.0d-20
        ty = sy / cy
        cx = cos( x * deg2rad )
        sx = sin( x * deg2rad )
        sq = ( sy * c_eps ) - ( cy * s_eps * sx * switch )
        q = atan( sq / ( sqrt( 1.0d+00 - sq * sq ) + 1.0d-20 ) )
     &                                                         / deg2rad
        a = ( sx * c_eps ) + ( ty * s_eps * switch )
        p = atan( a / cx ) / deg2rad
        if( cx .lt. 0.0d+00 ) p = p + 1.8d+02
	if( p .gt. 3.6d+02 ) p = P - 3.6d+02
	if( p .lt. 0.0d+00 ) p = p + 3.6d+02

        end
