#!/usr/bin/env python
import sys
import os
import setuptools
from numpy.distutils.core import setup, Extension

if "publish" in sys.argv[-1]:
    os.system("python setup.py sdist upload")
    sys.exit()

# Define the Fortran extension.
tvguide = Extension("tvguide._tvguide",
                    ["tvguide/tvguide_minimal.f",
                     "tvguide/sys.f",
                     "tvguide/eclip.f",
                     "tvguide/cartesian.f",
                     "tvguide/getlun.f",
                     "tvguide/julian.f",
                     "tvguide/lentrim.f",
                     "tvguide/nutate.f",
                     "tvguide/upcase.f",
                     "tvguide/tvguide.inc"])



# Load the __version__ variable without importing the package
exec(open('tvguide/version.py').read())

# Command-line tools
# we use scripts because entry_points doesn't seem to work
# with numpy.distutils
scripts = ['scripts/tvguide',
           'scripts/tvguide-csv',
           ]

setup(name='tvguide',
      version=__version__,
      description="Determine whether targets are observable TESS.",
      # long_description=long_description,
      author='Tom Barclay',
      author_email='tom@tombarclay.com',
      license='MIT',
      url='https://github.com/tessgi/tvguide',
      packages=['tvguide'],
      ext_modules=[tvguide, ],
      # data_files=[('kpub/templates', ['kpub/templates/template.md',
      #                  'kpub/templates/template-overview.md'])],
      # install_requires=["jinja2",
      #                   "six",
      #                   "astropy",
      #                   "ads"],
      scripts=scripts,
      classifiers=[
              "Development Status :: 3 - Alpha",
              "License :: OSI Approved :: MIT License",
              "Operating System :: OS Independent",
              "Programming Language :: Python",
              "Intended Audience :: Science/Research",
              "Topic :: Scientific/Engineering :: Astronomy", ],
      )
