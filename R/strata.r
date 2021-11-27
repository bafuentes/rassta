#' @title
#' Create Stratification Units
#'
#' @description
#' Stratification units are created from the spatial intersection of raster
#' layers representing different sets of classification units. Each set of
#' classification units is related to a particular landscape factor (e.g.,
#' topography, climate) or to a particular spatial scale for a single landscape
#' factor (e.g., micro-climate, macro-topography). Each resulting stratification
#' unit is considered to represent a distinct landscape configuration in terms
#' of multiple landscape factors/factor scales (represented by the classification
#' units). This function automatically assigns a unique numeric code to each
#' stratification unit. For \emph{x} stratification unit, the numeric code
#' represents the unique combination of classification units whose spatial
#' intersection resulted in \emph{x}. See \strong{Examples} to get a better idea
#' of the logic behind the code assignment process.
#'
#' @param cu.rast SpatRaster, as in \code{\link[terra]{rast}}. Multi-layer
#'   SpatRaster for which each layer represents a set of classification units
#'   for a particular landscape factor or factor scale. Integer cell values
#'   (i.e., numeric identifiers) are expected.
#' @param to.disk Boolean. Write the resulting raster layer of stratification
#'   units to disk? Default: FALSE
#' @param outdir Character. If \emph{to.disk = TRUE}, string specifying the path
#'   for the output raster layer of stratification units. Default: "."
#' @param su.name Character. If \emph{to.disk = TRUE}, file name (including
#'   extension) for the output raster layer of stratification units.
#' @param ... Additional arguments as for \code{\link[terra]{writeRaster}}.
#'
#' @return
#' \strong{su.rast}: Single-layer SpatRaster representing the stratification
#' units occurring across geographic space. The cell values in this raster layer
#' represents the numeric codes of stratification units.
#'
#' \strong{code.mult}: Multipliers used for the creation of the numeric codes.
#' See \strong{Details}.
#'
#' @details
#' When printing \emph{su.rast$code.mult}, the output shows the multiplier
#' used for each landscape factor/factor scale. From this output, one can manually
#' replicate the creation of stratification units through simple raster algebra.
#' To do so, a weighted sum of the SpatRasters containing the classification
#' units for each landscape factor/factor scale should be performed using the
#' multipliers as weights. Note that the weights do not imply relative
#' importance. The weights are required only to preserve a logical structure of
#' the landscape factors/factor scales in the resulting numeric code.
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Multi-layer SpatRast with classification units (Cus)
#' ## Three sets (i.e., landscape factors): geology, climate and topography
#' fcu <- list.files(path = p,
#'                   pattern = "geology.tif|climate.tif|topography.tif",
#'                   full.names = TRUE
#'                  )
#' cu <- terra::rast(fcu)
#' # Stratification units (SUs)
#' su <- strata(cu.rast = cu)
#' # Plot the stratification units
#' if(interactive()){plot(su$su.rast, type = "classes")}
#' #
#' # Note code structure from SUs and corresponding values from CUs
#' z <- c(su$su.rast, cu)[46,61]   # Example of one cell (row = 45, column = 45)
#' su$code.mult                    # Multipliers
#' z[c("SU", names(su$code.mult))] # Code structure
#'
#' # Note what happens when some landscape factors have cell values greater...
#' #... than 1 digit (i.e., more than 9 distinct classification units)
#' cu <- c(cu[[1]], cu[[2]]^4, cu[[3]]^2)
#' su <- strata(cu.rast = cu)
#' su$code.mult
#' c(su$su.rast, cu[[names(su$code.mult)]])[46,61]
#'
#' @export
#' @family
#' Functions for Landscape Stratification
#' @rdname
#' strata
#'
strata <- function(cu.rast, to.disk = FALSE, outdir = ".", su.name, ...)
{

  # Construct ordered multi-layer SpatRaster
  ## Get number of layers
  nras <- terra::nlyr(cu.rast)
  ## Get (sorted) maximum values from layers
  sortmax <- base::sort(terra::minmax(cu.rast)[2,])
  ## Function to get index of layer in SpatRaster according to...
  ## ...corresponding maximum value
  maxfun <- function(x, y) base::which(terra::minmax(y)[2,] == x)
  ## Get indexes of layers in SpatRaster, now sorted by maximum values
  stacklist <- base::sapply(sortmax, maxfun, cu.rast)
  ## Eliminate duplicated indexes (happens when two or more layers have...
  ## ...the exact same maximum value)
  stacklist <- (base::Reduce(c, base::unique(stacklist)))[1:nras]
  ## Re-order layers in original stack according to sorted indexes
  curast <- (cu.rast)[[c(stacklist)]]

  # Construct vector of powers before raster algebra
  ## Get vector of maximum values
  maxs <- terra::minmax(curast)[2,]
  ## Function to count number of digits in positive integers
  digitfun <- function(x) base::floor(base::log10(x)) + 1
  ## Count number of digits for each maximum value
  ndigits <- base::sapply(maxs, digitfun)
  ## Get reverse cumulative number of digits
  digitsum <- base::rev(base::cumsum(base::rev((ndigits))))
  ## Get cumulative number of digits before each element
  digitpow <- digitsum - ndigits
  ## Ten to the power function
  tenpow <- function(x) 10^x
  ## Get multiplier for each layer according to cumulative number of...
  ## ...(previous) digits
  mults <- base::sapply(digitpow, tenpow)

  ## Raster algebra (optional raster to disk)
  if (to.disk == TRUE) {

    rasclasses <- terra::app(x = (curast * mults),
                             fun = sum,
                             filename = base::file.path(outdir, su.name),
                             wopt = base::list(datatype = 'INT4S',
                                               names = "SU",
                                               ...
                                              )
                            )

  } else {

    rasclasses <- terra::app(x = (curast * mults), fun = sum)

  }

  # Return elements
  base::names(rasclasses) <- "SU"
  layers <- base::names(curast)
  multipliers <- c(mults)
  base::names(multipliers) <- layers
  base::list(su.rast = rasclasses, code.mult = multipliers)

}
