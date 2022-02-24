#' @title
#' Calculate the Spatial Signature of Classification Units
#'
#' @description
#' Using a mathematical function, a raster layer is created from the cell-wise
#' aggregation of a set of predicted distribution functions for a classification
#' unit (see \code{\link{predict_functions}}). Each element in this set can be
#' thought of as a correspondence measurement between an \emph{XY} location in
#' the geographic space and the landscape configuration represented by a given
#' classification unit in terms of a specific variable. Therefore, aggregating
#' the set of predicted distribution functions into a single layer produces an
#' overall (multivariate) measurement of correspondence. This multivariate
#' landscape correspondence metric is considered to be the \emph{spatial
#' signature} of the classification unit.
#'
#' @param pdif.rast SpatRaster, as in \code{\link[terra]{rast}}. Multi-layer
#'   SpatRaster whose raster layers represent predicted distribution functions
#'   for continuous variables and for one or more classification units. All
#'   predicted distribution functions for a particular classification unit are
#'   considered as part of the same set. There must be a matching pattern in the
#'   names of predicted distribution functions from the same set (see
#'   \emph{inprex}).
#' @param inprex Character. Prefix in the name of raster layers representing
#'   predicted distribution functions belonging to the same set (i.e.,same
#'   classification unit). If spatial signatures for multiple sets are to be
#'   calculated, a vector of strings must be provided. See \strong{Details} and
#'   \strong{Examples}.
#' @param outname Character. Output layer/file name for the raster layer(s) of
#'   spatial signature. If the spatial signatures for multiple classification
#'   units are to be calculated, then a vector of strings must be provided.
#' @param fun Function. The mathematical function must take a vector of values
#'   and return a single value (e.g., mean, max, sum, etc.). See
#'   \code{\link[terra]{app}}. Default: mean
#' @param to.disk Boolean. Write the output raster layer(s) of spatial signature
#'   to disk? See details about parallel processing. Default: FALSE
#' @param outdir Character. If \emph{to.disk = TRUE}, string specifying the path
#'   for the output  raster layer(s) of spatial signature. Default: "."
#' @param extension Character. If \emph{to.disk = TRUE}, String specifying the
#'   extension for the output raster layer(s) of spatial signature. Default:
#'   ".tif"
#' @param overwrite Boolean. If \emph{to.disk = TRUE}, should raster layers in
#'   disk and with same name as the output raster layer(s) of spatial signature
#'   be overwritten? Default: FALSE
#' @param ... If \emph{to.disk = TRUE}, additional arguments as for
#'   \code{\link[terra]{writeRaster}}.
#'
#' @return
#' Single-layer or multi-layer SpatRaster with the spatial signature(s)
#' calculated from the set(s) of predicted distribution functions.
#'
#' @details
#' Raster layers of predicted distribution functions belonging to the same
#' classification unit must be identified by a unique prefix in their layer
#' names (argument \emph{inprex}). This prefix is used as a string pattern to
#' find all the predicted distribution functions belonging to one classification
#' unit. Consequently, a unique prefix must be defined for each additional
#' classification unit to distinguish between predicted distribution functions
#' for different classification units. Similarly, an additional string (or
#' vector of strings) of file/layer name(s) must be provided to distinguish
#' between the resulting spatial signatures for different classification units
#' (argument \emph{outname}). The length of \emph{outname} must match that from
#' \emph{inprex}.
#'
#' When writing the output raster layers of spatial signature to disk, a
#' parallel backend can be registered before running this function with
#' \code{\link[doParallel]{registerDoParallel}} to speed-up computation. Note
#' that this is only helpful when calculating spatial signatures for many
#' classification units.
#'
#' From a spatial analysis standpoint, the aggregation of predicted distribution
#' functions into spatial signature is similar to the application of fuzzy
#' aggregation operators commonly used in GIS-based multi-criteria decision
#' analysis. Moreover, The use of descriptive statistics to calculate
#' \emph{signatures} for landscape-related classification units can be traced
#' back to the works of Pike and Rozema (1975), and Pike (1988).
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Multi-layer SpatRast with predicted distribution functions
#' ## 3 continuous variables and 5 classification units, = 15 functions
#' ft <- list.files(path = p, pattern = "topo_", full.names = TRUE)
#' t <- terra::rast(ft)
#' # Vector with the prefix for each set of predicted distribution functions
#' ## 5 classification units = 5 sets
#' it <- paste("topo_", seq(1, 5), "_", sep = "")
#' # Vector of names for output raster layers of spatial signature
#' ## 5 spatial signatures, one per classification unit
#' ot <- paste("topography_", seq(1, 5), sep = "")
#' # Calculate spatial signatures
#' tsig <- signature(pdif.rast = t, inprex = it, outname = ot)
#' # Plot spatial signatures
#' if(interactive()){plot(tsig, col = hcl.colors(100, "Oslo", rev = TRUE))}
#'
#' @export
#' @family
#' Landscape Correspondence Metrics
#' @rdname
#' signature
#' @references
#' R. Pike. The geometric signature: quantifying landslide-terrain types from
#' digital elevation models. Mathematical geology, 20(5):491–511, 1988.
#' \doi{https://doi.org/10.1007/BF00890333}
#'
#' R. Pike and W. Rozema. Spectral analysis of landforms. Annals of the
#' Association of American Geographers,65(4):499–516, 1975.
#' \doi{https://doi.org/10.1111/j.1467-8306.1975.tb01058.x}
#'
signature <- function(pdif.rast, inprex, outname, fun = mean,
                      to.disk = FALSE, outdir = ".", extension = ".tif",
                      overwrite = FALSE, ...)
{

  #-----Binding variables to prevent devtools::check() notes-----#
  i <- NULL
  #--------------------------------------------------------------#

  # Define processing scheme (disk [parallel] VS memory [sequential])
  `%ps%` <- if(to.disk == TRUE) { foreach::`%dopar%` } else { foreach::`%do%` }

  # Loop function
  ssig <- foreach::foreach(i = 1:base::length(inprex)) %ps% {

    # Find sets of predicted distribution functions
    rlays <- pdif.rast[[base::grep(pattern = base::paste("^",
                                                         inprex[i],
                                                         sep = ""
                                                        ),
                                   x = base::names(pdif.rast)
                                  )
                      ]]

    # Check that at least one predicted distribution function has been found
    if (terra::nlyr(rlays) < 1) {

      NULL

    } else {

      if(to.disk == TRUE) {

        # > Disk-based processing < #

        # File name for output raster layer of spatial signature
        ssig <- base::paste(outname[i], extension, sep = "")

        # Aggregation of predicted distribution functions into spatial signature
        terra::app(rlays,
                   fun = fun,
                   na.rm = TRUE,
                   filename = base::file.path(outdir, ssig),
                   overwrite = overwrite,
                   wopt = base::list(names = outname[i], ...)
                  )
        gc()

        # Retrieve file name for raster layer of spatial signature
        ssig <- ssig

      } else {

        # > Memory-based processing < #

        # Aggregation of predicted distribution functions into spatial signature
        ssig <- terra::app(rlays, fun = fun, na.rm = TRUE)
        gc()

        # Rename and retrieve raster layer of spatial signature
        base::names(ssig) <- outname[i]
        terra::varnames(ssig) <- outname[i]
        ssig <- ssig

      }

    }

  }

  if(to.disk == TRUE) {

    # Retrieve raster layers of spatial signature from disk
    ssig <- base::unlist(ssig)
    ssig <- base::paste("^", ssig, "$", sep = "")
    ssig <- base::lapply(ssig, list.files, path = outdir, full.names = TRUE)
    ssig <- terra::rast(base::unlist(ssig))

  } else {

    # Retrieve raster layers of spatial signature from memory
    ssig <- terra::rast(ssig)

  }

}
