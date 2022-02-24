#' @title
#' Predictive Modeling Engine
#'
#' @description
#' Modeling of spatially varying phenomena based on landscape similarity to
#' stratification units. If each stratification unit across geographic space
#' represents a distinct landscape configuration (in terms of multiple landscape
#' factors and/or factor scales), and if each landscape configuration influences
#' a phenomenon in a distinct way, then the spatial variability of that
#' phenomenon can be assessed across a landscape by relating each geographic
#' location to each distinct landscape configuration. Therefore, the more
#' similar a geographic location is to the landscape configuration represented
#' by a given stratification unit, then also the more similar the response of a
#' phenomenon will be at that location to the typical response for conditions
#' within the given stratification unit. Both continuous and categorical
#' response variables are supported. For categorical responses, each category
#' must be identified by an integer value.
#'
#' @param res.type Character. Type of response to model. Options are "cont" for
#'   continuous, and "cat" for categorical response. Default: "cont"
#' @param ls.rast SpatRaster, as in \code{\link[terra]{rast}}. Multi-layer
#'   SpatRaster representing landscape similarities to stratification units.
#'   Only similarities for units with a representative observation are allowed.
#'   Character prefix in the file name of similarities is allowed.
#' @param n.win Integer. Positive number indicating how many winning
#'   stratification units should be considered. See \strong{Details}. Default: 3
#' @param su.repobs Data frame. The first column of this data frame must contain
#'   only the numeric code for the stratification units (without prefix). Each
#'   additional column must contain the value of the representative response
#'   observation for each stratification unit. Multiple response variables are
#'   allowed (one per column). Note that all response variables in the data
#'   frame must share the same type (\emph{res.type}). See example on issues
#'   related to \strong{non-explicit column names}.
#' @param tiles SpatVector, as in \code{\link[terra]{vect}}. Spatial vector of
#'   polygon geometry with the boundaries of the area of interest. This vector
#'   can be subdivided in regions (i.e., tiles) to balance memory allocation and
#'   processing speed (see \strong{Details}). If this vector is tiled, then its
#'   attribute table must only contain an ID column with a unique identifier for
#'   each tile (1,2,...,n). Additionally, This vector must have the same
#'   coordinate reference system as \emph{ls.rast}.
#' @param parallel Boolean. Perform parallel processing? A parallel backend
#'   needs to be registered beforehand with
#'   \code{\link[doParallel]{registerDoParallel}}. Moreover, a tiled spatial
#'   vector should be supplied for \emph{tiles}. Default: FALSE
#' @param outdir Character. String specifying the path for the output raster
#'   tiles/layer(s) of modeled response(s). Default: "."
#' @param tile.rm Boolean. Should the tiles of modeled response(s) be removed
#'   from disk after the tile merging process? Default: TRUE
#' @param extension Character. String specifying the extension for the output
#'   raster layer(s) of modeled response(s). Default: ".tif"
#' @param verbose Boolean. Show warning messages in the console? Default: FALSE
#' @param ... Additional arguments as for \code{\link[terra]{writeRaster}}.
#'
#' @return
#' Multi-layer or single-layer SpatRaster with modeled response(s).
#'
#' @details
#' The predictive modeling process is cell-wise, which means that it operates on
#' a cell-by-cell basis. For a given cell occurring in the geographic space
#' supported by a raster layer, the predictive modeling engine first identifies
#' the \emph{n} stratification units to which the given cell is most similar
#' (i.e., 'winning stratification units'). The engine is able to identify the
#' winning stratification units thanks to the user-provided set of landscape
#' similarity layers \emph{ls.rast}. Subsequently, the response value from the
#' representative observation for each winning stratification unit is
#' identified. In the case of a continuous response, a weighted average of
#' representative response values is performed. For each representative response
#' value, the weight is proportional to the corresponding stratification unit's
#' landscape similarity value in the given cell. The result of the weighted
#' average is assigned as the response value in the given cell. In the case of a
#' categorical response, the modal value from the representative response values
#' of the \emph{n} winning stratification units is assigned to the given cell.
#'
#' Note that the name for each raster layer in \emph{ls.rast} should match the
#' numeric code of the corresponding stratification unit, which is obtained from
#' the column of numeric codes in \emph{su.repobs}. Nevertheless, raster layer
#' names in \emph{ls.rast} with a character prefix in the numeric code and/or
#' file extension should work fine (e.g., "su_1101.tif" instead of "1101"). If
#' the landscape similarity layers in \emph{ls.rast} were created with
#' \code{\link{similarity}}, then raster layer names will not have any prefix
#' nor extension as part of the numeric code.
#'
#' When dealing with large geographic spaces, high raster resolutions (i.e.,
#' small cell sizes), or both, a considerable amount of memory is required to
#' perform the modeling process. To reduce memory usage, the predictive modeling
#' engine performs tile-based processing of landscape similarity layers and
#' \strong{writes results directly on disk}. Tile-based processing increases the
#' computational time, thus parallelization is allowed by setting up a parallel
#' backend. If parallelization is enabled, then care must be taken with the size
#' of the tiles since larger sizes will have a greater impact on memory usage.
#' Consequently, the parallel, tile-based processing will be less useful.
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Multi-layer SpatRaster of landscape similarities
#' fls <- list.files(path = p, pattern = "su_", full.names = TRUE)
#' ls <- terra::rast(fls)
#' # Numeric code and representative response value for stratification units
#' fro <-list.files(path = p, pattern = "repobs.csv", full.names = TRUE)
#' ro <- read.csv(fro)
#' # Extract only those stratification units with representative value
#' ls <- ls[[as.character(ro$SU)]]
#' # SpatVector with processing tiles
#' fti <- list.files(path = p, pattern = "tiles.shp", full.names = TRUE)
#' ti <- terra::vect(fti)
#' # Directory for temporary files
#' o <- tempdir()
#' # Perform predictive modeling of continuous response
#' r <- engine(res.type = "cont", ls.rast = ls, n.win = 2, su.repobs = ro,
#'             tiles = ti, outdir = o, overwrite = TRUE
#'            )
#' # Plot modeled response
#' if(interactive()){plot(r)}
#' # Clean temporary files
#' file.remove(list.files(path = o, pattern = "soc.tif", full.names = TRUE))
#' #
#' #-------
#' # A note on non-explicit response's names (obtained from su.repobs):
#'
#' ## This will result in incorrectly modeled response values
#' x <- c("SOM", "SOM_30cm", "SOM_45cm")    # SOM = soil organic matter
#' grep(x[1], x)                            # Non explicit
#' grep(x[2], x)                            # Explicit
#' grep(x[3], x)                            # Explicit
#'
#' ## This will result in correct values
#' x <- c("SOM_15cm", "SOM_30cm", "SOM_45cm")
#' grep(x[1], x)                            # Explicit
#' grep(x[2], x)                            # Explicit
#' grep(x[3], x)                            # Explicit
#'
#' @export
#' @family
#' Functions for Predictive Modeling
#' @rdname
#' engine
#' @seealso
#' \code{\link{similarity}}, \code{\link{observation}}
#'
engine <- function(res.type = "cont", ls.rast, n.win = 3, su.repobs, tiles,
                   parallel = FALSE, outdir = ".", tile.rm = TRUE,
                   extension = ".tif", verbose = FALSE, ...)
{

  #-----Binding variables to prevent devtools::check() notes-----#
  i <- j <- k <- l <- m <- NULL
  #--------------------------------------------------------------#

  # Define processing scheme (disk [parallel] VS memory [sequential])
  `%ps%` <- if(parallel == TRUE) { foreach::`%dopar%` } else { foreach::`%do%` }

  # According to response type (continuous or categorical)
  if (res.type == "cont") {

    # Rewrite name of id field for tiles
    base::names(tiles) <- "id"

    # Detect number of responses to infer
    n.resp <- base::ncol(su.repobs) - 1 # minus id column

    # Loop for tiles
    tname <- foreach::foreach(i = 1:base::length(tiles)) %ps% {

      # Landscape similarity matrix from SUs' landscape similarities...
      # ...Only pixels within tile, each column is a landscape similarity layer
      rastile <- terra::as.data.frame(terra::mask(ls.rast,
                                                  tiles[tiles$id == i, ]
                                                ),
                                      na.rm = TRUE
                                    )

      # Define functions for (row-wise) descending sorting -> Equivalent...
      # ...to pixel-wise sorting of SUs' landscape similarity layers
      ## Find the column index(es) corresponding to n winning SU(s)
      fun0 <- function(x) base::order(x, decreasing = TRUE)[1:n.win]
      ## Find landscape similarity values from the n winning SU(s)
      fun1 <- function(x) x[base::order(x, decreasing = TRUE)][1:n.win]

      # Apply functions
      ## Column index for n winning SU(s)
      x <- base::apply(rastile, 1, fun0)
      ## Landscape similarity values for n winning SU(s)
      y <- base::apply(rastile, 1, fun1)

      # Construct table from column indexes and landscape similarity...
      # ...values of n winning SU(s)
      z <- data.table::as.data.table(t(base::rbind(x, y)))
      ## Rename column(s) for n winning SU(s)' column indexes
      base::colnames(z)[1:n.win] <- base::paste("su_max_",
                                                base::seq(1:n.win),
                                                "_ind",
                                                sep = ""
                                              )
      ## Rename column(s) for n winning SU(s)' landscape similarity values
      base::colnames(z)[(n.win+1):(n.win*2)] <- base::paste("su_max_",
                                                            base::seq(1:n.win),
                                                            "_ls",
                                                            sep = ""
                                                          )

      # Add columns to original landscape similarity matrix
      rastile <- base::cbind(rastile, z)

      # Remove unnecessary objects
      base::remove(fun0, fun1, x, y, z)

      # Numeric codes for the n winning SU(s) -> Forcing no prefix in codes
      `%do%` <- foreach::`%do%`
      sucodes <- foreach::foreach(j = 1:n.win) %do% {
        sumax_col <- terra::nlyr(ls.rast) + j
        suinds <- base::unlist(base::list(rastile[, sumax_col]))
        sucodes <- base::colnames(rastile)[(suinds)]
        sucodes <- stringi::stri_replace_all_regex(sucodes, "[a-zA-punct]", "")
        sucodes <- base::as.numeric(sucodes)
      }

      # Set names for column(s) with numeric codes of n winning SU(s)
      ## String
      newnames <- base::paste("su_max_", base::seq(1:n.win), "_code", sep = "")
      ## Name from string
      base::names(sucodes) <- newnames

      # Merge column(s) of numeric codes for n winning SU(s)...
      # ...with landscape similarity matrix
      sucodes <- data.table::as.data.table(sucodes)
      rastile <- base::cbind(rastile, sucodes)

      # Remove unnecessary objects
      base::remove(sumax_col, suinds, sucodes)

      # Get the n winning SU(s)' representative observation response values
      `%do%` <- foreach::`%do%`
      v <- foreach::foreach(k = 1:n.win) %do% {
        # Merging landscape similarity matrix with with table of SU(s)'...
        # ...representative observation response values -> By column of n...
        # ...winning SU' numeric codes
        v <- data.table::merge.data.table(rastile,
                                          su.repobs,
                                          by.x = newnames[k],
                                          by.y = base::colnames(su.repobs)[1],
                                          sort = FALSE,
                                          no.dups = FALSE
                                        )[,
                            (base::ncol(rastile)+1):(base::ncol(rastile)+n.resp)
                          ]
      }

      # Format table of n winning SU(s)' representative observation...
      # ...response values
      v <- data.table::as.data.table(v)

      # Assing column names based on n winning SU(s) and response(s)
      base::colnames(v) <- base::paste("su_max",
                                       base::rep(1:n.win, each = n.resp),
                                       base::colnames(su.repobs)[-1],
                                       sep = "_"
                                      )

      # Bind landscape similarity matrix with table of SUs' representative...
      # ...observation response values -> Columns corresponding to landscape...
      # ...similarity layers and column indexes of winning SU(s) are discarded
      rastile <- base::cbind(rastile[,
        (terra::nlyr(ls.rast) + n.win + 1):(terra::nlyr(ls.rast) + n.win * 2)],
        v
        )
      base::remove(v)

      # Get name(s) for column(s) with landscape similarity values of n...
      # ...winning SU(s)
      w <- base::colnames(rastile)[1:n.win]

      # Get name(s) for column(s) with SU(s)' representative observation...
      # ...response values
      x <- base::colnames(rastile)[(n.win+1):(base::length(rastile))]

      # Get reference raster for writing outputs (crop to processing tile)...
      # ...-> Pixel values in this raster will be overwritten by modeled...
      # ...response values!
      raslay <- terra::mask(ls.rast[[1]], tiles[tiles$id == i, ])

      # Get expression(s) for the product of n winning SU's response value...
      # ...and landscape similarity value (i.e., weight) -> Equivalent to a...
      # ...list of numerators for multiple weighted average expressions
      `%do%` <- foreach::`%do%`
      y <- foreach::foreach(l = 1:base::length(x)) %do% {
        y <- stringdist::amatch(x[l], w, maxDist = 10e3)
        y <- base::paste(x[l], w[y], sep = " * ")
      }
      ## Get single list (one weighted average's numerator per response)
      y <- base::unlist(y)

      # Loop through response(s) to model
      `%do%` <- foreach::`%do%`
      foreach::foreach(m = base::colnames(su.repobs)[-1]) %do% {

        # Get numerator's expression for response -> THIS IS NOT SAFE...
        # ...FOR NON-EXPLICIT RESPONSE NAMES
        ## Response's name matching
        z <- y[base::grep(pattern = m, x = y)]
        ## String collapsing
        z <- base::paste(z, collapse = ' + ')

        # Form full expression for weighted average -> Nested paste()...
        # ...is for denominator
        z <- base::paste('(', z,')',
                         "/",
                         '(',
                         base::paste(base::colnames(rastile)[1:n.win],
                                     collapse = ' + '
                                    ),
                         ')',
                         sep = ""
                        )

        # Model response through weighted average -> Full expression applied...
        # ...to landscape similarity matrix
        `%>%` <- dplyr::`%>%`
        `:=` <- rlang::`:=`
        z <- rastile %>% dplyr::transmute(z := !! rlang::parse_expr(z))

        # Rewrite reference raster pixel values with modeled response values
        raslay[!base::is.na(raslay)] <- base::as.numeric(base::unlist(z))

        # Set names for tile of modeled response
        ## Layer name
        layname <- base::paste(m, "_tile", base::as.character(i), sep = "")
        ## File name
        tname <- base::paste(layname, extension, sep = "")

        # Save raster tile of modeled response
        terra::writeRaster(raslay,
                           base::file.path(outdir, tname),
                           datatype = 'FLT4S',
                           names = layname,
                           ...
                          )

        # Retrieve file name for tile of modeled response stored in disk
        tname <- tname
      }

    }

    # List of tiles for all modeled response(s)
    tname <- base::unlist(tname)

    # For each modeled response, merge all tiles into single-layer SpatRaster
    ## [Parallel] processing for multiple responses
    response <- foreach::foreach(i = base::colnames(su.repobs)[-1]) %ps% {

      # Only tiles for a single response
      ## Base name of tiles from same modeled response
      tres <- base::paste("^", i, "_tile", sep = "")
      ## List of tiles from same modeled response
      tres <- base::grep(pattern = i, tname, value = TRUE)
      ## Read files from list of tiles from same modeled response
      tres <- base::lapply(tres, list.files, path = outdir, full.names = TRUE)

      # If tiles = 1
      if (base::length(tres) == 1) {

        # SpatRaster (tile) with modeled response
        tcol <- terra::rast(tres[[1]])
        resname <- base::paste(i, extension, sep = "")
        tcol <- terra::writeRaster(tcol,
                                   filename = base::file.path(outdir, resname),
                                   datatype = 'FLT4S',
                                   names = i,
                                   ...
                                  )

        # Remove tile
        base::file.remove(base::file.path(outdir, tname))

        # Retrieve file name of modeled response stored in disk
        response <- resname

      } else {

        # Create collection of tiles from same modeled response
        ## Sequential reading of tiles as SpatRaster objects
        `%do%` <- foreach::`%do%`
        tcol <- foreach::foreach(t = tres) %do% { terra::rast(t) }
        ## SpatRaster collection
        tcol <- terra::sprc(tcol)

        # Single-layer SpatRaster of modeled response
        ## File name
        resname <- base::paste(i, extension, sep = "")
        ## Reduce tiles collection into single-layer SpatRaster
        terra::merge(tcol,
                     filename = base::file.path(outdir, resname),
                     datatype = 'FLT4S',
                     names = i,
                     ...
                    )

        # Remove tiles?
        if(tile.rm == TRUE) {
          base::file.remove(base::file.path(outdir, tname))
        } else {
          NULL
        }

        # Retrieve file name of modeled response stored in disk
        response <- resname

      }

    }

    # Retrieve raster layer(s) of modeled response(s) from disk
    response <- base::unlist(response)
    response <- base::paste("^", response, "$", sep = "")
    response <- base::lapply(response,
                             list.files,
                             path = outdir,
                             full.names = TRUE
                            )
    response <- terra::rast(base::unlist(response))

  } else if (res.type == "cat") {

    # Rewrite name of id field for tiles
    base::names(tiles) <- "id"

    # Detect number of responses to infer
    n.resp <- base::ncol(su.repobs) - 1 # minus id column

    # Loop for tiles
    tname <- foreach::foreach(i = 1:base::length(tiles)) %ps% {

      # Landscape similarity matrix from SUs' landscape similarities...
      # ...Only pixels within tile, each column is a landscape similarity layer
      rastile <- terra::as.data.frame(terra::mask(ls.rast,
                                                  tiles[tiles$id == i, ]
                                                ),
                                      na.rm = TRUE
                                    )

      # Define functions for (row-wise) descending sorting -> Equivalent...
      # ...to pixel-wise sorting of SUs' landscape similarity layers
      ## Find the column index(es) corresponding to n winning SU(s)
      fun0 <- function(x) base::order(x, decreasing = TRUE)[1:n.win]
      ## Find landscape similarity values from the n winning SU(s)
      fun1 <- function(x) x[base::order(x, decreasing = TRUE)][1:n.win]

      # Apply functions
      ## Column index for n winning SU(s)
      x <- base::apply(rastile, 1, fun0)
      ## landscape similarity values for n winning SU(s)
      y <- base::apply(rastile, 1, fun1)

      # Construct table from column indexes and landscape similarity...
      # ...values of n winning SU(s)
      z <- data.table::as.data.table(t(base::rbind(x, y)))
      ## Rename column(s) for n winning SU(s)' column indexes
      base::colnames(z)[1:n.win] <- base::paste("su_max_",
                                                base::seq(1:n.win),
                                                "_ind",
                                                sep = ""
                                              )
      ## Rename column(s) for n winning SU(s)' landscape similarity values
      base::colnames(z)[(n.win+1):(n.win*2)] <- base::paste("su_max_",
                                                            seq(1:n.win),
                                                            "_ls",
                                                            sep = ""
                                                          )

      # Add columns to original landscape similarity matrix
      rastile <- base::cbind(rastile, z)

      # Remove unnecessary objects
      base::remove(fun0, fun1, x, y, z)

      # Numeric codes for the n winning SU(s) (forcing no prefix in codes)
      `%do%` <- foreach::`%do%`
      sucodes <- foreach::foreach(j = 1:n.win) %do% {
        sumax_col <- terra::nlyr(ls.rast) + j
        suinds <- base::unlist(base::list(rastile[, sumax_col]))
        sucodes <- base::colnames(rastile)[(suinds)]
        sucodes <- stringi::stri_replace_all_regex(sucodes, "[a-zA-punct]", "")
        sucodes <- base::as.numeric(sucodes)
      }

      # Set names for column(s) with numeric codes of n winning SU(s)
      ## String
      newnames <- base::paste("su_max_", base::seq(1:n.win), "_code", sep = "")
      ## Name from string
      base::names(sucodes) <- newnames

      # Merge column(s) of numeric codes for n winning SU(s)...
      # ...with landscape similarity matrix
      sucodes <- data.table::as.data.table(sucodes)
      rastile <- base::cbind(rastile, sucodes)

      # Remove unnecessary objects
      base::remove(sumax_col, suinds, sucodes)

      # Get the n winning SU(s)' representative observation response values
      `%do%` <- foreach::`%do%`
      v <- foreach::foreach(k = 1:n.win) %do% {
        # Merging landscape similarity matrix with with table of SU(s)'...
        # ...representative observation response values -> By column of n...
        # ...winning SU' numeric codes
        v <- data.table::merge.data.table(rastile,
                                          su.repobs,
                                          by.x = newnames[k],
                                          by.y = base::colnames(su.repobs)[1],
                                          sort = FALSE,
                                          no.dups = FALSE
                                        )[,
                            (base::ncol(rastile)+1):(base::ncol(rastile)+n.resp)
                          ]
      }

      # Format table of n winning SU(s)' representative observation...
      # ...response values
      v <- data.table::as.data.table(v)

      # Assign column names based on n winning SU(s) and response(s)
      base::colnames(v) <- base::paste("su_max",
                                       base::rep(1:n.win, each = n.resp),
                                       base::colnames(su.repobs)[-1],
                                       sep = "_"
                                      )

      # Remove landscape similarity matrix
      base::remove(rastile)

      # Get reference raster for writing outputs (crop to processing tile)
      # -> Pixel values in this raster will be overwritten by modeled...
      # ...response values!
      raslay <- terra::mask(ls.rast[[1]], tiles[tiles$id == i, ])

      # Loop through response(s) to model
      `%do%` <- foreach::`%do%`
      foreach::foreach(l = base::colnames(su.repobs)[-1]) %do% {

        # Model response through modal value across SU(s)'s representative...
        # ...values
        cols <- base::grep(pattern = l, x = base::colnames(v))
        z <- v[, cols]
        ## First: Greatest landscape similarity
        modfun <- function(x){ terra::modal(x, ties = "first") }
        z <- base::apply(z, 1, modfun)

        # Rewrite reference raster pixels values with modeled response values
        raslay[!base::is.na(raslay)] <- z

        # Set names for tile of modeled response
        ## Layer name
        layname <- base::paste(l, "_tile", base::as.character(i), sep = "")
        ## File name
        tname <- base::paste(layname, extension, sep = "")

        # Save raster tile of modeled response
        terra::writeRaster(raslay,
                           base::file.path(outdir, tname),
                           datatype = 'INT4S',
                           names = layname,
                           ...
                          )

        # Retrieve file name for tile of modeled response stored in disk
        tname <- tname

      }

    }

    # List of tiles for all modeled response(s)
    tname <- base::unlist(tname)

    # For each modeled response, merge all tiles into single-layer SpatRaster
    ## [Parallel] processing for multiple responses
    response <- foreach::foreach(i = base::colnames(su.repobs)[-1]) %ps% {

      # Only tiles for a single response
      ## Base name of tiles from same modeled response
      tres <- base::paste("^", i, "_tile", sep = "")
      ## List of tiles from same modeled response
      tres <- base::grep(pattern = i, tname, value = TRUE)
      ## Read files from list of tiles from same modeled response
      tres <- base::lapply(tres, list.files, path = outdir, full.names = TRUE)

      # If tiles = 1
      if (base::length(tres) == 1) {

        # SpatRaster (tile) with modeled response
        tcol <- terra::rast(tres[[1]])
        resname <- base::paste(i, extension, sep = "")
        tcol <- terra::writeRaster(tcol,
                                   filename = base::file.path(outdir, resname),
                                   datatype = 'FLT4S',
                                   names = i,
                                   ...
                                  )

        # Remove tile
        base::file.remove(base::file.path(outdir, tname))

        # Retrieve file name of modeled response stored in disk
        response <- resname

      } else {

        # Create collection of tiles from same modeled response
        ## Sequential reading of tiles as SpatRaster objects
        `%do%` <- foreach::`%do%`
        tcol <- foreach::foreach(t = tres) %do% { terra::rast(t) }
        ## SpatRaster collection
        tcol <- terra::sprc(tcol)

        # Single-layer SpatRaster of modeled response
        ## File name
        resname <- base::paste(i, extension, sep = "")
        ## Reduce tiles collection into single-layer SpatRaster
        terra::merge(tcol,
                     filename = base::file.path(outdir, resname),
                     datatype = 'INT4S',
                     names = i,
                     ...
                    )

        # Remove tiles?
        if(tile.rm == TRUE) {
          base::file.remove(base::file.path(outdir, tname))
        } else {
          NULL
        }

        # Retrieve file name of modeled response stored in disk
        response <- resname

      }

    }

    # Retrieve raster layer(s) of modeled response(s) from disk
    response <- base::unlist(response)
    response <- base::paste("^", response, "$", sep = "")
    response <- base::lapply(response,
                             list.files,
                             path = outdir,
                             full.names = TRUE
                            )
    response <- terra::rast(base::unlist(response))

  } else {

    if(verbose == TRUE){
      base::warning("Nothing was done. Please select a valid response type.")
    }

  }

}
