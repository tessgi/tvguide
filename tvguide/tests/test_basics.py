
"""Basic sanity checks to verify that tvguide works.
To run, simply type "py.test".
"""


def test_import():
    """Can we import tvguide successfully?"""
    import tvguide
    from tvguide import tvguide
    from tvguide import tvguide_csv
    from tvguide import TessPointing
    from tvguide import check_many
    from tvguide import check_observable
    

def test_observable():
    """
    let's check targets that we expect to be observable
    """
    from tvguide import TessPointing

    ra = 219.864863
    dec = -60.832280
    alphacen = TessPointing(ra, dec)
    assert alphacen.is_observable() == 2

    ra = 90
    dec = -66.5607083333
    southeclipticpole = TessPointing(ra, dec)
    assert southeclipticpole.is_observable() == 2

    ra = 0.
    dec = 0.
    ecliptic = TessPointing(ra, dec)
    assert ecliptic.is_observable() == 0

    ra = 270
    dec = 66.5607083333
    northeclipticpole = TessPointing(ra, dec)
    assert northeclipticpole.is_observable() == 1

    # ra = 90
    # dec = -66.56071
    # southeclipticpole = TessPointing(ra, dec)
    # assert southeclipticpole.get_camera(fallback=True) == 4    

    ra = 264.35182
    dec = -78.40052
    targetinchipgap = TessPointing(ra, dec)
    assert targetinchipgap.get_camera() == 0
    assert targetinchipgap.get_camera(fallback=True) == 3

    
