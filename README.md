# tvguide

A tool for determining whether stars and galaxies are observable by TESS.

We will move this to pypi soon but for now

``` bash
> git clone https://github.com/tessgi/tvguide.git
> cd tvguide
> python setup.py install
```

then pick your favorite star and have a whirl. I'm a big fan of Alpha Centauri
``` 
> tvguide 219.9009 -60.8356

Success! The target may be observable by TESS during Cycle 1.
We can observe this source for:
    maximum: 2 sectors
    minimum: 0 sectors
    median:  1 sectors
    average: 1.16 sectors
```
