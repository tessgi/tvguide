	Subroutine SET_NUTATE( DJD )
C
C	Calculate the amount of nutation in ecliptic longitude (d_Psi)
C	and in the obliquity of the ecliptic (d_Eps) in degrees
C	for a given DJD
C			Based on a BASIC routine in "Astronomy with your
C			Personal Computer" (Duffett-Smith), CUP
C					FORTRAN version by KM, 23 Oct 1988
C
	Implicit none
C
        double precision deg2rad
	Double Precision whole
        parameter( deg2rad = 1.74532925199433d-02 )
*                          = pi / 180, converts from degrees to radians
	Parameter( whole = 3.60D+02 )
C
	Double Precision DJD
C
	Double Precision T, T2, A, B, DIFF
	Double Precision LS, LS2, LD, LD2, MS, MD, NM, NM2
C
	Double Precision did_DJD
	Save did_DJD
C
	Double Precision d_Psi, d_Eps, RCP, RSP, Height
	Common / astro1 / d_Psi, d_Eps, RCP, RSP, Height
C
	DIFF = DJD - did_DJD
	If( Abs( DIFF ) .lt. 0.01D+00 ) Return
	T = DJD / 3.6525D+04
	T2 = T * T
	A = 1.000021358D+02 * T
	B = whole * ( A - Int( A ) )
	LS = 2.796967D+02 + 3.03D-04 * T2 + B
	LS = LS * deg2rad
	A = 1.336855231D+03 * T
	B = whole * ( A - Int( A ) )
	LD = 2.704342D+02 - 1.133D-03 * T2 + B
	LD = LD * deg2rad
	A = 9.999736056D+01 * T
	B = whole * ( A - Int( A ) )
	MS = 3.584758D+02 - 1.5D-04 * T2 + B
	MS = MS * deg2rad
	A = 1.325552359D+07 * T
	B = whole * ( A - Int( A ) )
	MD = 2.961046D+02 + 9.192D-03 * T2 * B
	MD = MD * deg2rad
	A = 5.372616667D+00 * T
	B = whole * ( A - Int( A ) )
	NM = 2.591833D+02 + 2.078D-03 * T2 - B
	NM = NM * deg2rad
	LS2 = LS * 2.0D+00
	LD2 = LD * 2.0D+00
	NM2 = NM * 2.0D+00
C
	d_Psi = ( -1.72327D+01 - 1.737D-02 * T ) * Sin( NM )
     &	+ ( -1.2729D+00 - 1.3D-04 * T ) * Sin( LS2 )
     &	+ 2.088D-01 * Sin( NM2 ) - 2.037D-01 * Sin( LD2 )
     &	+ ( 1.261D-01 - 3.1D-04 * T ) * Sin( MS )
     &	+ 6.75D-02 * Sin( MD )
     &	- ( 4.97D-02 - 1.2D-04 * T ) * Sin( LS2 + MS )
     &	- 3.42D-02 * Sin( LD2 - NM )
     &  - 2.61D-02 * Sin( LD2 + MD )
     &	+ 2.14D-02 * Sin( LS2 - MS )
     &	- 1.49D-02 * Sin( LS2 - LD2 + MD )
     &	+ 1.24D-02 * Sin( LS2 - NM )
     &  + 1.14D-02 * Sin( LD2 - MD )
	d_Psi = d_Psi / 3.6D+03
	d_Eps = ( 9.21D+00 + 9.1D-04 * T ) * Cos( NM )
     &	+ ( 5.522D-01 - 2.9D-04 * T ) * Cos( LS2 )
     &	- 9.04D-02 * Cos( NM2 ) + 8.84D-02 * Cos( LD2 )
     &	+ 2.16D-02 * Cos( LS2 + MS )
     &  + 1.83D-02 * Cos( LD2 - NM )
     &	+ 1.13D-02 * Cos( LD2 + MD )
     &  - 9.3D-03 * Cos( LS2 - MS ) 
     & 	- 6.6D-03 * Cos( LS2 - NM )
	d_Eps = d_Eps / 3.6D+03
C
	did_DJD = DJD
C
	End
