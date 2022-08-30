# **rassta 1.0.5**

### **Fixes**

* Fixed a bug which forced `engine()` to delete not only the tiles of a modeled
variable, but the final (merged) layer too.

* Fixed a bug which prevented the (re) classification of SpatRasters due to a 
change in `terra`'s arguments (`othersNA` to `others = NA`).


# **rassta 1.0.4**

### **New**

* Function `predict_functions()` now allows generalized additive models (GAM) as
prediction method.

### **Enhancements**

* Complete removal of package `raster` dependency

* Code optimization for several functions based on native `terra` methods such
as `add()`, `classify()`, `minmax()`, `not.na()`, and `predict()`.

* Function `som_pam()` now allows a multi-layer SpatRaster, possibly containing
layers with different structure (location and number) of NA values, as reference
SpatRaster.


# **rassta 1.0.3**

### **Enhancements**

* `select_functions()` now allows to work with SpatRaster objects representing 
classification units whose numeric IDs are not sequentially defined (e.g., 
unordered classification units).

### **Fixes**

* All the functions that return SpatRaster objects now assign proper variable
names instead of getting the names from a reference SpatRaster.

* Fixed a bug which prevented `select_functions()` from running properly when
the argument var.rast represents a single-layer SpatRaster object.

* Fixed a bug which prevented `strata()` from retrieving the correct minimum and
maximum values from SpatRaster objects with NA values present, and thus, from
assigning the correct numeric codes for stratification units.


# **rassta 1.0.2**

### **Enhancements**

* Some functions have been renamed and vignettes for some functions have been
created.


# **rassta 1.0.1**

### **Fixes**

* Fixed violation of CRAN's policy regarding package code that attempts to write
to the user library. Check logs can be accessed through:
https://cran-archive.r-project.org/web/checks/2021/2021-10-08_check_results_rassta.html


# **rassta 1.0.0**

* Initial release
