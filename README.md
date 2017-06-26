# tvguide

A tool for determining whether stars and galaxies are observable by TESS.

[![Travis status](http://img.shields.io/travis/tessgi/tvguide/master.svg)](http://travis-ci.org/tessgi/tvguide)
[![PyPI](http://img.shields.io/pypi/v/tvguide.svg)](https://pypi.python.org/pypi/tvguide/)

## Installation
You can install using pip


``` bash
> pip install tvguide --upgrade
```

or via the github repository
``` bash
> git clone https://github.com/tessgi/tvguide.git
> cd tvguide
> python setup.py install
```

## Useage
Pick your favorite star and have a whirl. I'm a big fan of Alpha Centauri
``` 
> tvguide 219.9009 -60.8356

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
> head inputfilename.csv

150., -60.
10., -75.
51., 0.
88., +65

> tvguide-csv inputfilename.csv

Writing example-file.csv-tvguide.csv.

>head example-file.csv-tvguide.csv

150.0000000000, -60.0000000000, 2
10.0000000000, -75.0000000000, 2
51.0000000000, 0.0000000000, 2
88.0000000000, 65.0000000000, 1
```
This new file appends another column with values 0, 1 or 2. 
* 2 = May be observable in Cycle 1 (you should propose for these targets)
* 1 = May be observable in Cycle 2
* 0 = not observable in Cycle 1 or 2
