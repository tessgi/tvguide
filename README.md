# tvguide

A tool for determining whether stars and galaxies are observable by TESS.

[![Travis status](http://img.shields.io/travis/tessgi/tvguide/master.svg)](http://travis-ci.org/tessgi/tvguide)
[![PyPI](http://img.shields.io/pypi/v/tvguide.svg)](https://pypi.python.org/pypi/tvguide/)
[![DOI](https://zenodo.org/badge/94136696.svg)](https://zenodo.org/badge/latestdoi/94136696)


## Installation
This will work in Python 2.7 and Python 3.x

You can install using pip
``` bash
$ pip install tvguide --upgrade
```

or via the github repository
``` bash
$ git clone https://github.com/tessgi/tvguide.git
$ cd tvguide
$ python setup.py install
```

## Usage
Pick your favorite star and have a whirl. I'm a big fan of Alpha Centauri
```
$ tvguide 219.9009 -60.8356

Success! The target may be observable by TESS during Cycle 1.
We can observe this source for:
    maximum: 2 sectors
    minimum: 0 sectors
    median:  1 sectors
    average: 1.16 sectors
```

You can also run on a file with targets
currently implemented is using RA and Dec.
```
$ head inputfilename.csv

150., -60.
10., -75.
51., 0.
88., +65

$ tvguide-csv inputfilename.csv

Writing example-file.csv-tvguide.csv.

$ head example-file.csv-tvguide.csv

150.0000000000, -60.0000000000, 0, 2
10.0000000000, -75.0000000000, 1, 3
51.0000000000, 0.0000000000, 0, 1
88.0000000000, 65.0000000000, 0, 0
```
This new file appends two additional columns. The number in the first column is the minimum number of sectors the target is observable for and the second is the maximum.

You can also run from within a Python script:
```python
import tvguide

tvguide.check_observable(150.00, -60.00)

tvguide.check_many(ra_array, dec_array)
```

## Citation
If you find this code useful and want to cite it in your research then we have made that possible for you
```
Mukai, K. & Barclay, T. 2017, tvguide: A tool for determining whether stars and galaxies are observable by TESS., v1.0.0, Zenodo, doi:10.5281/zenodo.823357
