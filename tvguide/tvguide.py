from __future__ import absolute_import

import numpy as np
from ._tvguide import tvguidef
import argparse


class TessPointing(object):

    def __init__(self, ra_deg, dec_deg, dstart=0):
        self.ra_deg = ra_deg
        self.dec_deg = dec_deg
        self.dstart = dstart

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
            int(np.median(outarr)), np.mean(outarr) )



def tvguide(args=None):
    pass


if __name__ == '__main__':
    pass
