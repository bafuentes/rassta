#' @title
#' Calculate the Landscape Similarity to Stratification Units
#'
#' @description
#' For each stratification unit present in a single-layer SpatRaster, a raster
#' layer of landscape similarity is created by aggregating the stratification
#' unit's corresponding set of spatial signatures (see \code{\link{signature}}).
#' For a stratification unit \emph{x}, the corresponding set of spatial
#' signatures consists of one spatial signature for each of the \emph{n}
#' classification units that are present in the numeric code of \emph{x} (one
#' classification unit per landscape factor/factor scale). The aggregation
#' process is performed cell-wise, and by using a mathematical function which
#' takes multiple values but return a single value (e.g., mean, sum, min, max).
#' The resulting raster layer represents the correspondence between an \emph{XY}
#' location in geographic space and the landscape configuration represented by a
#' given stratification unit.
#'
#' @param su.rast SpatRaster, as in \code{\link[terra]{rast}}. Single-layer
#'   SpatRaster representing the stratification units occurring across
#'   geographic space. Integer values are expected as cell values (i.e., numeric
#'   codes) of stratification units.
#' @param su.code List. The structure of the stratification units' numeric code.
#'   This (nested) list should indicate the names of the landscape
#'   factors/factor scales used to create the stratification units, and the
#'   position (start, end) of their corresponding classification units' ID in
#'   the numeric code. See \strong{Examples}.
#' @param sig.rast SpatRaster. Multi-layer SpatRaster with the spatial
#'   signatures of all the classification units that were used to create the
#'   stratification units. The spatial signatures should follow this name
#'   convention: \emph{x_n}; where \emph{x} is the landscape factor/factor scale,
#'   and \emph{n} is the numeric ID of the classification unit to which
#'   the spatial signature belongs.
#' @param fun Function. The mathematical function must accept a vector of values
#'   and return a single value (e.g., mean, max, sum, etc.). See
#'   \code{\link[terra]{app}}. Default: mean
#' @param to.disk Boolean. Write the output raster layers of landscape
#'   similarity to disk? See note about parallel processing. Default: FALSE
#' @param outdir Character. If \emph{to.disk = TRUE}, string specifying the path
#'   for the output raster layers of landscape similarity. Default: "."
#' @param prefix Character. If \emph{to.disk = TRUE}, prefix for the file name
#'   of the output raster layers of landscape similarity. Default: "su_"
#' @param extension Character. If \emph{to.disk = TRUE}, string specifying the
#'   extension for the output raster layers of landscape signature. Default:
#'   ".tif"
#' @param overwrite Boolean. When \emph{to.disk = TRUE}, should raster layers in
#'   disk and with same name as the output landscape similarities be
#'   overwritten? Default: FALSE
#' @param ... Additional arguments as for \code{\link[terra]{writeRaster}} (if
#'   \emph{to.disk = TRUE}).
#'
#' @return
#' A list with the following components:
#'
#' \strong{landsim}: Multi-layer SpatRaster with the landscape similarity to
#' each stratification unit present in \emph{su.rast}.
#'
#' \strong{codes}: A data frame with the numeric code for each stratification
#' unit and the corresponding classification units' numeric ID for each
#' landscape factor/factor scale.
#'
#' @details
#' The landscape similarity is a landscape correspondence metric. The
#' aggregation of multiple spatial signatures into a single landscape similarity
#' layer is somewhat similar to the application of fuzzy logic and aggregation
#' operators in GIS-based multi-criteria decision analysis. Furthermore, the
#' aggregation of raster layers indicating relative optimality for
#' spatially-varying phenomena, like spatial signatures, can be guided by
#' physical/ecological principles like Sprengel-Liebig's law of the minimum. In
#' such case, one could select the \emph{min} function when aggregating the
#' spatial signatures into landscape similarities.
#'
#' When writing the output raster layers of landscape similarity to disk, a
#' parallel backend can be registered before running this function with
#' \code{\link[doParallel]{registerDoParallel}} to speed-up computation.
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Single-layer SpatRaster of stratification units
#' fsu <- list.files(path = p, pattern = "strata2.tif", full.names = TRUE)
#' su <- terra::rast(fsu)
#' # Define the structure of the stratification units' numeric code
#' code <- list(geology = c(1,1), climate = c(2,2), topography = c(3,3))
#' # Multi-layer SpatRaster of spatial signatures of classification units
#' fsig <- list.files(path = p, pattern = "geology_|climate_|topography_",
#'                    full.names = TRUE
#'                   )
#' sig <- terra::rast(fsig)
#' # Calculate landscape similarity to stratification units
#' landsim <- similarity(su.rast = su, su.code = code, sig.rast = sig)
#'
#' # Plot some landscape similarities
#' # if(interactive()){plot(landsim$landsim[[c(1,10,12,14)]],
#' #                        col = hcl.colors(100, "Oslo", rev = TRUE)
#' #                       )}
#'
#' #-------
#' # A note on the numeric code of stratification units
#'
#' # For a given stratification unit, the structure of its corresponding numeric
#' # code indicates: (1) the landscape factors and/or factor scales that were
#' # accounted for when creating the stratification unit, and (2) the numeric id
#' # of the classification unit from each landscape factor/factor scale.
#' # Consider the following numeric code structure:
#'
#' su.code <- list(geology = c(1,1), climate = c(2,2), topography = c(3,4))
#'
#' # The stratification units are composed of classification units from...
#' # ...three landscape factors: geology, climate, and topography
#' names(su.code)
#'
#' # For geology, the classification units are represented by the first...
#' # ...digit in the numeric code
#' su.code$geology
#'
#' # For climate, the classification units are represented by the second...
#' # ...digit in the numeric code
#' su.code$climate

#' # For topography, the classification units are represented by the third...
#' # ...and fourth digit in the numeric code
#' su.code$topography
#'
#' # Thus, the numeric code of the stratification units 1101 and 2410 means:
#' su <- c(1101, 2410)
#' su[1]   # 'geology' = 1, 'climate' = 1, and 'topography' = 1
#' su[2]   # 'geology' = 2, 'climate' = 4, and 'topography' = 10
#'
#' @export
#' @family
#' Landscape Correspondence Metrics
#' @rdname
#' similarity
#' @seealso
#' \code{\link{strata}}
#'
similarity <- function(su.rast, su.code, sig.rast, fun = mean, to.disk = FALSE,
                       outdir = ".", prefix = "su_", extension = ".tif",
                       overwrite = FALSE, ...)
{

  #-----Binding variables to prevent devtools::check() notes-----#
  i <- j <- rw <- NULL
  #--------------------------------------------------------------#

  # Variable = landscape factor or factor scale

  # Split numeric codes according to user-define code structure
  ## List of numeric codes for stratification units
  su.list <- base::unlist(base::as.list(terra::unique(su.rast)))
  ## Recursive splitting
  `%do%` <- foreach::`%do%`
  `%:%` <- foreach::`%:%`
  x <- foreach::foreach(i = su.list) %:%
    foreach::foreach(j = 1:base::length(su.code)) %do% {

      # Split digits according to beginning/end of digits for single variable
      x <- stringi::stri_sub(i, su.code[[j]][[1]], su.code[[j]][[2]])
      x <- base::as.numeric(x)
      base::list(stats::setNames(x, base::names(su.code)[j]))

    }

  # Format numeric codes into data frame
  ## flatten nested list into named vector
  x <- base::unlist(x)
  ## Stacking named vector into data frame
  x <- utils::stack(x)
  ## Renaming columns
  base::colnames(x) <- c("value", "variable")
  ## Creation of ID column for stratification units
  x$id <- base::rep(1:(base::nrow(x)/base::length(su.code)),
                    each = base::length(su.code)
                  )
  ## Reshaping data frame with variables as new columns
  x <- stats::reshape(x, idvar = "id", timevar = "variable", direction = "wide")
  ## Renaming variable columns according user-defined code structure
  base::colnames(x)[-1] <- base::names(su.code)
  ## Resetting row numbers
  base::rownames(x) = base::seq(length = base::nrow(x))
  ## Adding column of codes for stratification unit
  x$SU <- su.list
  ## Removing ID
  x <- x[,-1]

  # Define processing scheme (disk [parallel] VS memory [sequential])
  `%ps%` <- if(to.disk == TRUE) { foreach::`%dopar%` } else { foreach::`%do%` }

  # Aggregation of spatial signatures into landscape similarity
  landsim <- foreach::foreach(rw = 1:base::nrow(x)) %ps% {

    # Base raster to serve as reference for multi-layer SpatRaster
    spatsign <- su.rast

    # Sequential construction of multi-layer SpatRaster of spatial signatures
    # ... for each stratification unit
    `%do%` <- foreach::`%do%`
    foreach::foreach(c = 1:base::length(su.code)) %do% {

      # Find spatial signature for classification unit in current iteration
      ## Search pattern
      sptsgn <- base::paste("^",
                            base::colnames(x[c]),
                            "_",
                            x[rw,c],
                            "$",
                            sep = ""
                          )
      ## Find spatial signature
      sptsgn <- base::grep(pattern = sptsgn,
                           base::names(sig.rast),
                           value = TRUE
                          )
      sptsgn <- sig.rast[[sptsgn]]

      # Add spatial signature for classification unit in current iteration
      spatsign <- c(spatsign, sptsgn)

    }

    ### Discard base raster -> Only signatures now
    spatsign <- (spatsign[[-1]])

    # Name for raster layer of landscape similarity
    ## layer name (from column with numeric codes of stratification units SU)
    lname <- base::as.character(x[rw, -c(1:base::length(su.code))])
    ## File name
    rname <- base::paste(prefix, lname, extension, sep = "")

    if(to.disk == TRUE) {

      # > Disk-based writing < #

      # Aggregation of spatial signatures into landscape similarity
      landsim <- terra::app(spatsign,
                            fun = fun,
                            na.rm = TRUE,
                            filename = base::file.path(outdir, rname),
                            overwrite = overwrite,
                            wopt = base::list(names = lname, ...)
                          )
      gc()

      # Retrieve file name for raster layer of landscape similarity
      landsim <- rname

    } else {

      # > Memory-based writing < #

      # Aggregation of spatial signatures into landscape similarity
      landsim <- terra::app(spatsign, fun = fun, na.rm = TRUE)
      gc()

      # Rename and retrieve raster layer of landscape similarity
      base::names(landsim) <- lname
      terra::varnames(landsim) <- lname
      landsim <- landsim

    }

  }

  if(to.disk == TRUE) {

    # Retrieve raster layers of landscape similarity from disk
    landsim <- base::unlist(landsim)
    landsim <- base::paste("^", landsim, "$", sep = "")
    landsim <- base::lapply(landsim,
                            list.files,
                            path = outdir,
                            full.names = TRUE
                          )
    landsim <- terra::rast(base::unlist(landsim))

    # Final objects
    list(landsim = landsim, codes = x)

  } else {

    # Retrieve raster layers of landscape similarity from memory
    landsim <- terra::rast(landsim)

    # Final objects
    list(landsim = landsim, codes = x)

  }

}
