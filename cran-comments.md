## First submission - Sunday, Sep 19, ~12:00 AM CDT

### Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

### R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
- New submission

#### Results
> On windows-x86_64-devel (r-devel),
  ubuntu-gcc-release (r-release),
  fedora-clang-devel (r-devel)
  
  checking CRAN incoming feasibility ... NOTE
  
  New submission
  Maintainer: 'Bryan A. Fuentes <bryandrep@gmail.com>'

0 errors ✓ | 0 warnings ✓ | 1 note x

### Issues with first submission

* **Issue 1.** If there are references describing the methods in your package,
please add these in the description field of your DESCRIPTION file.

* **Issue 2.** You write information messages to the console that cannot be
easily suppressed.

* **Issue 3.** Please make sure that you do not change the user's options, par
or working directory.

## Second submission - Tuesday, Sep 21, ~11:40 AM CDT

### Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

### R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
- New submission

#### Results
> On windows-x86_64-devel (r-devel),
  ubuntu-gcc-release (r-release),
  fedora-clang-devel (r-devel)
  
  checking CRAN incoming feasibility ... NOTE
  
  New submission
  Maintainer: 'Bryan A. Fuentes <bryandrep@gmail.com>'
  
  0 errors ✓ | 0 warnings ✓ | 1 note x

### Proposed fixes

* **Issue 1.** The authors of the package have written a manuscript describing
the methods presented in the package. This manuscript is still in internal
review. Reference to this manuscript will be added to the package's description
file once it is available as a preprint/published journal article.

* **Issue 2.** The automatic display of information messages in the console has
been canceled. Functions that previously had this issue now include a 'verbose'
argument. This argument allows the user to decide whether or not to suppress
warning messages in the console.

* **Issue 3.** A call of on.exit() as been included in function 'figure.r' to
restore the user's graphical parameters after their modification by the
function.

**Second submission was approved by CRAN**

## Third submission - Wednesday, Oct 12, ~3:45 PM CDT 

### Issues with previous submission (second submission, accepted by CRAN)

- The package includes one example with code that attempts write to the user
library, which is in violation of CRAN's policy. Check logs regarding this issue
can be accessed through:

https://cran-archive.r-project.org/web/checks/2021/2021-10-08_check_results_rassta.html

**The package was archived because of this issue**

### Proposed fixes

- The package's example code does not attempt to write to the user library
anymore. One example uses *tempdir()* to write temporary files, which are
deleted at the end of the example (see *engine()*).

### Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

### R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:  
- New submission  
  Maintainer: ‘Bryan A. Fuentes <bryandrep@gmail.com>’  

  **Package was archived on CRAN**

#### Results
> On windows-x86_64-devel (r-devel),
  ubuntu-gcc-release (r-release),
  fedora-clang-devel (r-devel)
  
  checking CRAN incoming feasibility ... NOTE  
  
  New submission  
  Maintainer: 'Bryan A. Fuentes <bryandrep@gmail.com>'  

0 errors ✓ | 0 warnings ✓ | 1 note x

## Fourth submission - Wednesday, Dec 8, ~00:20 AM CDT

### Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

### R CMD check results
There were no ERRORs or WARNINGs. 

> On windows-x86_64-devel (r-devel),
  ubuntu-gcc-release (r-release),
  fedora-clang-devel (r-devel)
  
  checking CRAN incoming feasibility ... NOTE
  
  - Possibly misspelled words in DESCRIPTION:
     Dorantes (25:90)
     Tipton (26:9)
  
  0 errors ✓ | 0 warnings ✓ | 1 note x

