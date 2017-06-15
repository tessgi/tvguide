from __future__ import absolute_import

import numpy as np
from ._tvguide import tvguidef
import argparse
from astropy.coordinates import SkyCoord
from astropy import units as u
from . import Highlight


class TessPointing(object):

    def __init__(self, ra_deg, dec_deg, dstart=0):
        self.ra_deg = ra_deg
        self.dec_deg = dec_deg
        self.dstart = dstart

    def is_observable(self):
        """
        is the target observable in Cycle 1?
        returns
        0, not observable, in ecliptic,
        1, not observable, in Cycle 2
        2, observable
        """
        # convert to ecliptic
        gc = SkyCoord(ra=self.ra_deg * u.degree, dec=self.dec_deg * u.degree,
                      frame='icrs')
        lat = gc.barycentrictrueecliptic.lat.value
        if (lat > -6) & (lat < 6):
            return 0
        elif (lat >= 6):
            return 1
        else:
            return 2

    def get_13cameras(self):
        """
        returns an array of thirteen integers
            the value of each integer ranges from 0-4
            with 0 meaning not observable, and the integer
            refering to the camera if the target is observale
            13 valies, one for each sector in Cycle 1
        """
        return tvguidef(self.ra_deg, self.dec_deg, self.dstart)

    def get_numcampaigns(self):
        """
        returns an integer of how many sectors a target is observable
        """
        return np.nonzero(self.get_13cameras())[0].shape[0]

    def get_maxminmedave(self):
        """
        get the max, min and average number of campaigns that a target
            is observable by TESS in Cycle 1
        """
        step_arr = np.arange(0, 360, 0.5)
        outarr = np.zeros_like(step_arr)
        dstart_orig = np.copy(self.dstart)
        for i, dstart in enumerate(np.arange(0, 360, 0.5)):
            self.dstart = dstart
            outarr[i] = self.get_numcampaigns()

        self.dstart = dstart_orig
        return (int(np.max(outarr)), int(np.min(outarr)),
                int(np.median(outarr)), np.mean(outarr))


def tvguide(args=None):
    """
    exposes tvguide to the command line
    """
    parser = argparse.ArgumentParser(
        description="Determine whether targets are observable using TESS.")
    parser.add_argument('ra', nargs=1, type=float,
                        help="Right Ascension in decimal degrees (J2000).")
    parser.add_argument('dec', nargs=1, type=float,
                        help="Declination in decimal degrees (J2000).")

    args = parser.parse_args(args)
    ra, dec = args.ra[0], args.dec[0]

    tessObj = TessPointing(ra, dec)

    if tessObj.is_observable() == 0:
        print(Highlight.RED + "Sorry, the target is not observable by TESS"
              "during Cycle 1 or 2." + Highlight.END)
    elif tessObj.is_observable() == 1:
        print(Highlight.RED + "Sorry, the target is not observable by TESS"
              " during Cycle 1.\nBut may be observable in Cycle 2" +
              Highlight.END)
    elif tessObj.is_observable() == 2:
        print(Highlight.GREEN + "Success! The target may be observable by TESS"
              " during Cycle 1." + Highlight.END)

        outlst = tessObj.get_maxminmedave()
        print(Highlight.GREEN + "We can observe this source for:" +
              Highlight.END)
        print(Highlight.GREEN + "    maximum: {0} sectors".format(
            outlst[0]) + Highlight.END)
        print(Highlight.GREEN + "    minimum: {0} sectors".format(
            outlst[1]) + Highlight.END)
        print(Highlight.GREEN + "    median:  {0} sectors".format(
            outlst[2]) + Highlight.END)
        print(Highlight.GREEN + "    average: {0:0.2f} sectors".format(
            outlst[3]) + Highlight.END)


def tvguide_fromfile(args=None):
    pass


def tvguide_fromtic(args=None):
    pass


if __name__ == '__main__':
    pass
