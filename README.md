# gRoot

> Prototype tool for visualising and interacting with the Urban forest data

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)



### Description:


An online version of this app can be found [here](https://datasciencecampus.shinyapps.io/cardiff_app/). If you would like to install the package, which contains all the Urban Forest data for Cardiff, functions for visualising the data and the app, then please follow the instructions below. 

More details about the app be found in the vignette which is also contained within the package. 



### Installation:

```
#install.packages(devtools)
devtools::install_github("datasciencecampus/gReeneRy")
```

### Usage

Once installed, the app can be run with the command:

```
gReeneRy::launch_app
```

### Further documentation

All documentation can be found with 
```
?gReeneRy
```

A vignette giving a brief overview is also provided and can be accessed with:

```
vignette(gReeneRy)
```

### Related projects
* [trees-deckgl-visualisation](https://github.com/datasciencecampus/trees-deckgl-visualisation)
* [street-view pipeline](https://github.com/datasciencecampus/street-view-pipeline)

### Data sources 
* [Urban-forest-data](https://github.com/datasciencecampus/street-view-pipeline)
* [Urban-tree-cover-wales](http://lle.gov.wales/catalogue/item/UrbanTreeCover2013Polygons/?lang=ens)
* [Parish_geojson](http://geoportal.statistics.gov.uk/datasets/parishes-and-non-civil-parished-areas-december-2016-generalised-clipped-boundaries-in-england-and-wales)
We used generalised clipped boundaries and filtered for lad16nm = Cardiff. This
file was downloaded via the API.
