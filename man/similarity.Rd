% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/similarity.r
\name{similarity}
\alias{similarity}
\title{Calculate the Landscape Similarity to Stratification Units}
\usage{
similarity(
  su.rast,
  su.code,
  sig.rast,
  fun = mean,
  to.disk = FALSE,
  outdir = ".",
  prefix = "su_",
  extension = ".tif",
  overwrite = FALSE,
  ...
)
}
\arguments{
\item{su.rast}{SpatRaster, as in \code{\link[terra]{rast}}. Single-layer
SpatRaster representing the stratification units occurring across
geographic space. Integer values are expected as cell values (i.e., numeric
codes) of stratification units.}

\item{su.code}{List. The structure of the stratification units' numeric code.
This (nested) list should indicate the names of the landscape
factors/factor scales used to create the stratification units, and the
position (start, end) of their corresponding classification units' ID in
the numeric code. See \strong{Examples}.}

\item{sig.rast}{SpatRaster. Multi-layer SpatRaster with the spatial
signatures of all the classification units that were used to create the
stratification units. The spatial signatures should follow this name
convention: \emph{x_n}; where \emph{x} is the landscape factor/factor scale,
and \emph{n} is the numeric ID of the classification unit to which
the spatial signature belongs.}

\item{fun}{Function. The mathematical function must accept a vector of values
and return a single value (e.g., mean, max, sum, etc.). See
\code{\link[terra]{app}}. Default: mean}

\item{to.disk}{Boolean. Write the output raster layers of landscape
similarity to disk? See note about parallel processing. Default: FALSE}

\item{outdir}{Character. If \emph{to.disk = TRUE}, string specifying the path
for the output raster layers of landscape similarity. Default: "."}

\item{prefix}{Character. If \emph{to.disk = TRUE}, prefix for the file name
of the output raster layers of landscape similarity. Default: "su_"}

\item{extension}{Character. If \emph{to.disk = TRUE}, string specifying the
extension for the output raster layers of landscape signature. Default:
".tif"}

\item{overwrite}{Boolean. When \emph{to.disk = TRUE}, should raster layers in
disk and with same name as the output landscape similarities be
overwritten? Default: FALSE}

\item{...}{Additional arguments as for \code{\link[terra]{writeRaster}} (if
\emph{to.disk = TRUE}).}
}
\value{
A list with the following components:

\strong{landsim}: Multi-layer SpatRaster with the landscape similarity to
each stratification unit present in \emph{su.rast}.

\strong{codes}: A data frame with the numeric code for each stratification
unit and the corresponding classification units' numeric ID for each
landscape factor/factor scale.
}
\description{
For each stratification unit present in a single-layer SpatRaster, a raster
layer of landscape similarity is created by aggregating the stratification
unit's corresponding set of spatial signatures (see \code{\link{signature}}).
For a stratification unit \emph{x}, the corresponding set of spatial
signatures consists of one spatial signature for each of the \emph{n}
classification units that are present in the numeric code of \emph{x} (one
classification unit per landscape factor/factor scale). The aggregation
process is performed cell-wise, and by using a mathematical function which
takes multiple values but return a single value (e.g., mean, sum, min, max).
The resulting raster layer represents the correspondence between an \emph{XY}
location in geographic space and the landscape configuration represented by a
given stratification unit.
}
\details{
The landscape similarity is a landscape correspondence metric. The
aggregation of multiple spatial signatures into a single landscape similarity
layer is somewhat similar to the application of fuzzy logic and aggregation
operators in GIS-based multi-criteria decision analysis. Furthermore, the
aggregation of raster layers indicating relative optimality for
spatially-varying phenomena, like spatial signatures, can be guided by
physical/ecological principles like Sprengel-Liebig's law of the minimum. In
such case, one could select the \emph{min} function when aggregating the
spatial signatures into landscape similarities.

When writing the output raster layers of landscape similarity to disk, a
parallel backend can be registered before running this function with
\code{\link[doParallel]{registerDoParallel}} to speed-up computation.
}
\examples{
require(terra)
p <- system.file("exdat", package = "rassta")
# Single-layer SpatRaster of stratification units
fsu <- list.files(path = p, pattern = "strata2.tif", full.names = TRUE)
su <- terra::rast(fsu)
# Define the structure of the stratification units' numeric code
code <- list(geology = c(1,1), climate = c(2,2), topography = c(3,3))
# Multi-layer SpatRaster of spatial signatures of classification units
fsig <- list.files(path = p, pattern = "geology_|climate_|topography_",
                   full.names = TRUE
                  )
sig <- terra::rast(fsig)
# Calculate landscape similarity to stratification units
landsim <- similarity(su.rast = su, su.code = code, sig.rast = sig)

# Plot some landscape similarities
# if(interactive()){plot(landsim$landsim[[c(1,10,12,14)]],
#                        col = hcl.colors(100, "Oslo", rev = TRUE)
#                       )}

#-------
# A note on the numeric code of stratification units

# For a given stratification unit, the structure of its corresponding numeric
# code indicates: (1) the landscape factors and/or factor scales that were
# accounted for when creating the stratification unit, and (2) the numeric id
# of the classification unit from each landscape factor/factor scale.
# Consider the following numeric code structure:

su.code <- list(geology = c(1,1), climate = c(2,2), topography = c(3,4))

# The stratification units are composed of classification units from...
# ...three landscape factors: geology, climate, and topography
names(su.code)

# For geology, the classification units are represented by the first...
# ...digit in the numeric code
su.code$geology

# For climate, the classification units are represented by the second...
# ...digit in the numeric code
su.code$climate
# For topography, the classification units are represented by the third...
# ...and fourth digit in the numeric code
su.code$topography

# Thus, the numeric code of the stratification units 1101 and 2410 means:
su <- c(1101, 2410)
su[1]   # 'geology' = 1, 'climate' = 1, and 'topography' = 1
su[2]   # 'geology' = 2, 'climate' = 4, and 'topography' = 10

}
\seealso{
\code{\link{strata}}

Other Landscape Correspondence Metrics: 
\code{\link{predict_functions}()},
\code{\link{select_functions}()},
\code{\link{signature}}
}
\concept{Landscape Correspondence Metrics}
