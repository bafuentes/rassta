# rassta 1.0.3

* All the functions that return SpatRaster objects now assign proper variable names 
instead of getting the names from a reference SpatRaster.

* Fixed a bug which prevented select_functions() from running properly when the 
argument var.rast represents a single-layer SpatRaster object.

* select_functions() now allows to work with SpatRaster objects representing 
classification units whose numeric IDs are not sequentially defined (e.g., 
unordered classification units).

* Fixed a bug which prevented strata() from retrieving the correct minimum and 
maximum values from SpatRaster objects with NA values present, and thus, from 
assigning the correct numeric codes for stratification units.  


# rassta 1.0.2

* Some functions have been renamed and vignettes for some functions have been created.


# rassta 1.0.1

* Fixed violation of CRAN's policy regarding package code that attempts to write
to the user library. Check logs can be accessed through:
https://cran-archive.r-project.org/web/checks/2021/2021-10-08_check_results_rassta.html


# rassta 1.0.0

* Added a `NEWS.md` file to track changes to the package.
