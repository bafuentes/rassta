#' @title
#' Create Dummy Layers from Categorical Raster Layers
#'
#' @description
#' Given a SpatRaster whose cell values represent categories (e.g., soil types,
#' land use/cover classes), a dummy layer indicating the presence/absence of
#' each category is created, and optionally written on disk. Each category in
#' the raster layer must be represented by a unique integer cell value. Output
#' values indicating the presence or absence of categories in the dummy layers
#' may be set using \emph{preval} and \emph{absval} arguments, respectively.
#'
#' @param ca.rast SpatRaster, as in \code{\link[terra]{rast}}. Single-layer
#'   SpatRaster whose (integer) cell values represent numeric IDs for
#'   categories.
#' @param vals Integer. Vector or sequence of values representing the categories
#'   for which dummy layers will be created. If NULL, all categories will be
#'   processed. Default: NULL
#' @param preval Integer. Value indicating presence of category. Default: 100
#' @param absval Integer. Value indicating absence of category. Default: 0
#' @param to.disk Boolean. Write output dummy layers to disk? Default: FALSE
#' @param outdir Character. If \emph{to.disk = TRUE}, string specifying the path
#'   for the output dummy raster layers. Default: "."
#' @param extension Character. If \emph{to.disk = TRUE}, String specifying the
#'   extension for the output raster layers (see \strong{Details}). Default:
#'   ".tif"
#' @param ... If \emph{to.disk = TRUE}, additional arguments as for
#'   \code{\link[terra]{writeRaster}}.
#'
#' @return
#' Single-layer or multi-layer SpatRaster with dummy layer(s).
#'
#' @details
#' This function mainly differs from \code{\link[terra]{segregate}} in
#' that presence and absence values can be set arbitrarily, thus allowing for
#' values other than those traditionally used in one-hot/dummy encoding (i.e.,
#' 1 and 0).
#'
#' Please note that the argument \emph{extension} does not correspond to the
#' argument \emph{filetype} in \code{\link[terra]{writeRaster}}. However,
#' \code{\link[terra]{writeRaster}} should recognize the appropriate extension
#' for the output raster layers from the \emph{extension} argument. For
#' instance, by setting \emph{extension = ".tif"},
#' \code{\link[terra]{writeRaster}} will recognize the extension as
#' \emph{GeoTiff}, which is the GDAL driver name.
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Single-layer SpatRaster of geologic units
#' f <- list.files(path = p, pattern = "geology2.tif", full.names = TRUE)
#' geol <- terra::rast(f)
#' # Dummy layer from geologic unit 1
#' dums <- dummies(ca.rast = geol, vals = 1, preval = 100, absval = 0)
#'
#' @export
#' @family
#' Miscellaneous Functions
#' @rdname
#' dummies
#' @seealso
#' \code{\link[terra]{segregate}}
#'
dummies <- function(ca.rast, vals = NULL, preval = 100, absval = 0,
                    to.disk = FALSE, outdir = ".", extension = ".tif", ...)
{

  #-----Binding variables to prevent devtools::check() notes-----#
  i <- NULL
  #--------------------------------------------------------------#

  # If vals = NULL, get unique cell values (i.e., categories)
  if(base::is.null(vals)) {

    val <- base::as.integer(terra::unique(ca.rast)[[1]])
    val <- val[!base::is.na(val)]

  } else {

    val <- base::as.integer(vals)

  }
  gc()

  # Create placeholder SpatRaster for dummies
  d <- terra::rast()

  # Loop through each category
  `%do%` <- foreach::`%do%`
  foreach::foreach(i = val) %do% {

    # Set layer name for dummy
    layname <- base::paste(base::names(ca.rast),
                           "_",
                           as.character(i),
                           sep = ""
                          )

    # Create re-classification matrix
    rcm <- base::matrix(c(i, preval), nrow = 1, ncol = 2)

    # Write to disk?
    if (to.disk == TRUE) {

      # Set file name for dummy layer
      dumname <- base::paste(layname, extension, sep = "")

      # Binarization through re-classification
      dx <- terra::classify(ca.rast,
                            rcm,
                            others = absval,
                            filename = file.path(outdir, dumname),
                            datatype = "INT4S",
                            ...
                          )

      # Set layer name for dummy
      base::names(dx) <- layname
      terra::varnames(dx) <- layname

      # Add dummy to placeholder
      terra::add(d) <- dx
      base::remove(dx)
      gc()

    } else {

      # Binarization through re-classification
      dx <- terra::classify(ca.rast, rcm, others = absval)

      # Set layer name for dummy
      base::names(dx) <- layname
      terra::varnames(dx) <- layname

      # Add dummy to placeholder
      terra::add(d) <- dx
      base::remove(dx)
      gc()

    }

  }

  # Return SpatRaster with dummy layers
  d

}
