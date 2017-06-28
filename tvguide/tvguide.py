from __future__ import absolute_import

import numpy as np
from ._tvguide import tvguidef
import argparse
from astropy.coordinates import SkyCoord
from astropy import units as u
from . import Highlight

from . import logger


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

    def get_camera(self):
        """
        which camera is the star on?
        """
        cams = self.get_13cameras()
        cams = cams[cams > 0]
        if np.shape(cams)[0] > 0:
            return int(np.median(cams))
        else:
            return 0

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


def parse_file(infile, exit_on_error=True):
    """Parse a comma-separated file with columns "ra,dec,magnitude".
    """
    try:
        a, b = np.atleast_2d(
                            np.genfromtxt(
                                        infile,
                                        usecols=[0, 1],
                                        delimiter=','
                                        )
                    ).T
    except IOError as e:
        if exit_on_error:
            logger.error("There seems to be a problem with the input file, "
                         "the format should be: RA_degrees (J2000), Dec_degrees (J2000). "
                         "There should be no header, columns should be "
                         "separated by a comma")
            sys.exit(1)
        else:
            raise e
    return a, b

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
        print(Highlight.GREEN +
              "Success! The target may be observable by TESS during Cycle 1." +
              Highlight.END)
        print(Highlight.GREEN +
              "Looks like it may fall into Camera {}.".format(
                  tessObj.get_camera()) + Highlight.END)

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


def tvguide_csv(args=None):
    """
    exposes tvguide-csv to the command line
    """
    parser = argparse.ArgumentParser(
        description="Determine whether targets in a csv are observable using TESS.")
    parser.add_argument('input_filename', nargs=1, type=str,
                        help="Path to a comma-separated table containing "
                             "columns 'ra, dec' (decimal degrees) "
                             "or 'TIC number'.")
    args = parser.parse_args(args)
    input_fn = args.input_filename[0]
    output_fn = input_fn + '-tvguide.csv'
    # First, try assuming the file has the classic "ra, dec format
    try:
        ra, dec = parse_file(input_fn, exit_on_error=False)
        minC = np.zeros_like(ra, dtype=int)
        maxC = np.zeros_like(ra, dtype=int)
        for idx in range(len(ra)):
            tobj = TessPointing(ra[idx], dec[idx])
            minC[idx] = tobj.get_maxminmedave()[1]
            maxC[idx] = tobj.get_maxminmedave()[0]
        output = np.array([ra, dec, minC, maxC])
        print("Writing {0}".format(output_fn))
        np.savetxt(output_fn, output.T, delimiter=', ',
                   fmt=['%10.10f', '%10.10f', '%i', '%i'])
    # If this fails, assume the file has a single "name" column
    except ValueError:
        raise NotImplementedError

# def tvguide_fromtic(args=None):
#     pass


if __name__ == '__main__':
    pass
