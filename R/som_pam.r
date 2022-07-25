#' @title
#' Rasterization of Self-Organizing Map and Partitioning Around Medoids
#'
#' @description
#' Creation of a rasterized representation of the outputs from the
#' self-organizing map (SOM) and partitioning around medoids (PAM). Given a
#' reference raster layer, each winning unit of the SOM and clustering value
#' from the PAM will be mapped on the corresponding cell in the reference layer
#' and across the geographic space supported by such layer. Note that this
#' function is a follow-up of the \code{\link{som_gap}} function.
#'
#' @param ref.rast SpatRaster, as in \code{\link[terra]{rast}}. This raster
#'   layer will serve as a reference of the index of valid cells and the
#'   geographic support for the rasterized representation of SOM's winning units
#'   and PAM's clustering. See \emph{Details} for some notes about efficiency.
#' @param kohsom SOM Object of class \strong{kohonen}, see
#'   \code{\link[kohonen]{supersom}}. The following components must be present
#'   in the SOM object (1) \emph{unit.classif} = winning units for all
#'   observations, and (2) \emph{codes} = matrix of codebook vectors.
#' @param k Integer (positive value). Number of clusters to form from the
#'   codebook vectors of the SOM, where \emph{k < SOM's codebook vectors}.
#' @param metric Character. Distance function for PAM. Options are "euclidean",
#'   and "manhattan". Default: "manhattan"
#' @param stand Boolean. For the PAM function, do SOM's codebook vectors need to
#'   be standardized? Default: FALSE
#' @param ... Additional arguments as for \code{\link[cluster]{pam}}. See
#'   \strong{Details}.
#'
#' @return
#' \strong{sompam}: Object of class \strong{pam}. See ?pam.object for details.
#'
#' \strong{sompam.rast}: Multi-layer SpatRaster, as in
#' \code{\link[terra]{rast}}. The first raster layer corresponds to the SOM's
#' winning units. The second raster layer corresponds to the clustered SOM's
#' codebook vectors by PAM.
#'
#' @details
#' As in \code{\link{som_gap}}, this function calls \code{\link[cluster]{pam}}
#' to perform the clustering of SOM's codebook vectors. The SOM object must
#' belong to the class \strong{kohonen}, as in \code{\link[kohonen]{supersom}}.
#'
#' Note that in order for \code{\link{som_pam}} to perform efficiently, the
#' reference SpatRaster \emph{ref.rast} must be a single-layer SpatRaster with
#' the same cell size, number of rows, number of columns, and index of valid
#' cells as those in the multi-layer SpatRaster object used in
#' \code{\link{som_gap}}. If a multi-layer SpatRaster (with each layer possibly
#' having a different index of valid cells) is used as the \emph{ref.rast}, the
#' efficiency of \code{\link{som_pam}} (i.e., running time and/or memory
#' allocation) may be degraded when handling large SpatRaster objects.
#'
#' For this function to work as intended, the additional argument
#' \emph{cluster.only} in \code{\link[cluster]{pam}} must remain as FALSE, which
#' is the default.
#'
#' @examples
#' require(terra)
#' # Multi-layer SpatRaster with topographic variables
#' p <- system.file("exdat", package = "rassta")
#' ft <- list.files(path = p, pattern = "^height|^slope|^wetness",
#'                  full.names = TRUE
#'                 )
#' t <- rast(ft)
#' # Scale topographic variables (mean = 0, StDev = 1)
#' ts <- scale(t)
#' # Self-organizing map and gap statistic for optimum k
#' set.seed(963)
#' tsom <- som_gap(var.rast = ts, xdim = 8, ydim = 8, rlen = 150,
#'                mode = "online", K.max = 6, B = 300, spaceH0 = "original",
#'                method = "globalSEmax"
#'               )
#' # Optimum k
#' tsom$Kopt
#' # PAM clustering of topographic SOM's codebook vectors
#' tpam <- som_pam(ref.rast = t[[1]], kohsom = tsom$SOM, k = tsom$Kopt)
#' # Plot topographic variables, SOM grid and PAM clustering
#' if(interactive()){plot(c(t, tpam$sompam.rast))}
#'
#' @export
#' @family
#' Functions for Landscape Stratification
#' @rdname
#' som_pam
#'
som_pam <- function(ref.rast, kohsom, k, metric = "manhattan", stand = FALSE,
                   ...)
{

  # Partitioning around medoids (PAM) of SOM codes
  m.sompam <- cluster::pam(x = kohonen::getCodes(kohsom),
                           k = k,
                           diss = FALSE,
                           metric = metric,
                           stand = stand,
                           ...
                          )

  # Get template SpatRaster layers
  ## For Self-Organizing Map
  if(terra::nlyr(ref.rast) > 1) {

    # Sum approach to reduce memory allocation...
    #... (as compared to idx <- stats::complete.cases(terra::values(x)))
    r.som <- terra::app(x = ref.rast, fun = sum)

  } else {

    r.som <- ref.rast

  }
  base::names(r.som) <- "SOM"
  terra::varnames(r.som) <- "SOM"
  ## For PAM clustering
  r.sompam <- r.som
  base::names(r.sompam) <- "SOMPAM"
  terra::varnames(r.sompam) <- "SOMPAM"

  # Rasterize SOM
  r.som[terra::not.na(r.som)] <- kohsom$unit.classif

  # Rasterize SOM-based PAM
  r.sompam[terra::not.na(r.som)] <- m.sompam$clustering[kohsom$unit.classif]

  # Multi-layer SpatRaster
  terra::add(r.som) <- r.sompam

  # Free some memory
  gc()

  # Return list of components
  list(sompam = m.sompam, sompam.rast = r.som)

}
