# rassta
<img align="right" width="230" height="270" src="man/figures/rassta_logo.png">

`rassta` (**r**aster-based **s**patial **st**ratification **a**lgorithms) is a collection of algorithms for the spatial stratification of
landscapes, sampling, and modeling of spatially-varying phenomena in the [R](https://www.r-project.org) environment.

`rassta` offers a simple framework for the stratification of geographic space based on raster layers representing landscape factors and/or
factor scales. The stratification process follows a hierarchical approach, which is based on first level units (i.e., classification units)
and second-level units (i.e., stratification units). Nonparametric techniques allow to measure the correspondence between the geographic
space and the landscape configuration represented by the units. These correspondence metrics are useful to define sampling schemes and to
model the spatial variability of environmental phenomena.

## Installation

`rassta` has been submitted to CRAN and a decision from the *CRAN team* is still pending. In the meantime, `rassta` can be installed as follows:

```
remotes::install_github("bafuentes/rassta")
```
