#' @title
#' Predict Distribution Functions Across Geographic Space
#'
#' @description
#' Predicts constrained, univariate distribution functions across the geographic
#' space supported by raster layers. For a given continuous variable used to
#' create a classification unit, this function first calculates a user-defined
#' distribution function for that variable using only observations selected from
#' within the classification unit. In this way, the distribution function is
#' univariate and constrained. Subsequently, a \emph{locally-estimated
#' scatterplot smoothing} is fitted (see \code{\link[stats]{loess}}). The LOESS
#' is fitted using the variable’s observations as explanatory values and the
#' values from the distribution function as the response values. Finally, the
#' fitted LOESS is predicted on the complete geographic space supported by the
#' raster layer of the given variable. This process is iterated for all of the
#' continuous variables and classification units. Each resulting layer can be
#' thought of as a landscape correspondence measurement between an \emph{XY}
#' location in geographic space and the landscape configuration represented by a
#' given classification unit in terms of a specific variable. The following
#' distribution functions are currently supported: the probability density
#' function (PDF), the empirical cumulative density function (ECDF), and the
#' inverse of the empirical cumulative density function (iECDF). Please refer to
#' \strong{Details} for more information about how each distribution function is
#' calculated. Also, see details on parallel processing.
#'
#' @param cuvar.rast SpatRaster, as in \code{\link[terra]{rast}}. Multi-layer
#'   SpatRaster containing \emph{n} continuous raster layers (i.e., variables)
#'   and one raster layer of classification units with integer cell values
#'   (i.e., Numeric identifiers).
#' @param cu.ind Integer. Position (index) of the raster layer of classification
#'   units in \emph{cuvar.rast}.
#' @param cu Integer. Vector of integer values that correspond to the numeric
#'   identifiers of the units in the raster layer of classification units.
#' @param vars Character. Vector of strings containing the names of the \emph{n}
#'   continuous variables in \emph{cuvar.rast}. These names have to be
#'   sequentially repeated according to the number of classification units (See
#'   \strong{Examples}).
#' @param dif Character. Vector of strings containing the distribution function
#'   to calculate for each continuous variable within each classification unit.
#'   The function will match the position of the name of the distribution
#'   function with that of the name of the continuous variable in \emph{vars}.
#' @param hist.type Character. Type of histogram to calculate. Options are
#'   "regular", "irregular" (unequally-sized bins, computationally demanding),
#'   and "combined" (the one with greater penalized likelihood is returned). See
#'   \code{\link[histogram]{histogram}}. Default: "regular"
#' @param hist.pen Character. Penalty to apply when calculating the histogram
#'   (see \code{\link[histogram]{histogram}}). Default: "default"
#' @param grid.mult Numeric. Multiplying factor to increase/decrease the size of
#'   the "optimal" grid size for the \emph{Kernel Density Estimate} (KDE).
#'   Default: 1
#' @param kern Character. Type of kernel to use for the KDE. Default: "normal"
#' @param quant.sep Numeric. Spacing between quantiles for the calculation of
#'   the ECDF and iECDF. Quantiles are in the range of 0-1 thus spacing must be
#'   a decimal. Default: 0.01
#' @param span Numeric. Degree of smoothing for the LOESS. Default: 0.6
#' @param to.disk Boolean. Write the output raster layers of predicted
#'   distribution function to disk? See details about parallel processing.
#'   Default: FALSE
#' @param outdir Character. If \emph{to.disk = TRUE}, string specifying the path
#'   for the output raster layers of predicted distribution function. Default:
#'   "."
#' @param prefix Character. If \emph{to.disk = TRUE}, string specifying a prefix
#'   for the file names of the output raster layers of predicted distribution
#'   function. Default: ""
#' @param extension Character. If \emph{to.disk = TRUE}, string specifying the
#'   extension for the output raster layers of predicted distribution function.
#'   Default: ".tif"
#' @param verbose Boolean. Show warning messages in the console? Default: FALSE
#' @param ... If \emph{to.disk = TRUE}, additional arguments as for
#'   \code{\link[terra]{writeRaster}}.
#'
#' @return
#' Single-layer or multi-layer SpatRaster with the predicted distribution
#' function for each variable and for each classification unit.
#'
#' @details
#' To calculate the PDF, this function uses the binned KDE for observations
#' drawn from the breaks of a regular/irregular histogram. The "optimal" number
#' of bins for the histogram is defined by calling the function
#' \code{\link[histogram]{histogram}} (Mildenberger et al., 2019) with the
#' user-defined penalty \emph{hist.pen}. Subsequently, the optimal number of
#' bins is treated as equivalent to the "optimal" grid size for the binned KDE.
#' The grid size can be adjusted by specifying the multiplying factor
#' \emph{grid.mult}. Lastly, the "optimal" bandwidth for the binned KDE is
#' calculated by applying the \emph{direct plugin} method of Sheather and Jones
#' (1991). For the calculation of optimal bandwidth and for the binned KDE, the
#' package \strong{KernSmooth} is called. To calculate both the ECDF and the
#' iECDF, this function calls the \code{\link[stats]{ecdf}} function on
#' equally-spaced quantiles. The spacing between quantiles can be manually
#' adjusted via \emph{quant.sep}. In the case of iECDF, the ECDF is inverted by
#' applying the formula: \emph{iECDF = ((x - max(ECDF)) * -1) + min(ECDF)};
#' where \emph{x} corresponds to each value of the ECDF.
#'
#' The "cu", "vars", and "dif" parameters of this function are configured such
#' that the output table from \code{\link{select_functions}} can be used
#' directly as input. (see \strong{Examples}).
#'
#' When writing output raster layer to disk, multiple distribution functions can
#' be predicted in parallel if a parallel backend is registered beforehand with
#' \code{\link[doParallel]{registerDoParallel}}. Keep in mind that the function
#' may require a large amount of memory when using a parallel backend with large
#' raster layers (i.e., high resolution and/or large spatial coverage).
#'
#' @examples
#' require(terra)
#' p <- system.file("exdat", package = "rassta")
#' # Multi-layer SpatRaster of topographic variables
#' ## 3 continuous variables
#' ftva <- list.files(path = p, pattern = "^height|^slope|^wetness",
#'                    full.names = TRUE
#'                   )
#' tva <- terra::rast(ftva)
#' # Single-layer SpatRaster of topographic classification units
#' ## Five classification units
#' ftcu <- list.files(path = p, pattern = "topography.tif", full.names = TRUE)
#' tcu <- terra::rast(ftcu)
#' # Add the classification units to the SpatRaster of topographic variables
#' tcuvars <- c(tcu, tva)
#' # Data frame with source for "cu", "vars", and "dif"
#' ftdif <- list.files(path = p, pattern = "topodif.csv", full.names = TRUE)
#' tdif <- read.csv(ftdif)
#' # Check structure of source data frame
#' head(tdif)
#' # Predict distribution functions
#' ## 1 distribution function per variable and classification unit = 1
#' tpdif <- predict_functions(cuvar.rast = tcuvars, cu.ind = 1,
#'                            cu = tdif$Class.Unit[1:3],
#'                            vars = tdif$Variable[1:3],
#'                            dif = tdif$Dist.Func[1:3],
#'                            grid.mult = 3, span = 0.9
#'                          )
#' # Plot predicted distribution functions
#' if(interactive()){plot(tpdif, col = hcl.colors(100, "Oslo", rev = TRUE))}
#'
#' @export
#' @family
#' Landscape Correspondence Metrics
#' @rdname
#' predict_functions
#' @references
#' T. Mildenberger, Y. Rozenholc, and D. Zasada. histogram: Construction of
#' Regular and Irregular Histograms with Different Options for Automatic Choice
#' of Bins, 2019. \url{https://CRAN.R-project.org/package=histogram}
#'
#' S. Sheather and M. Jones. A reliable data-based bandwidth selection method
#' for kernel density estimation. Journal of the Royal Statistical Society.
#' Series B. Methodological, 53:683–690, 1991.
#'
predict_functions <- function(cuvar.rast, cu.ind, cu, vars, dif,
                              hist.type = "regular", hist.pen = "default",
                              grid.mult = 1, kern = "normal", quant.sep = 0.01,
                              span = 0.6, to.disk = FALSE, outdir = ".",
                              prefix = "", extension = ".tif", verbose = FALSE,
                              ...)
{

  #-----Binding variables to prevent devtools::check() notes-----#
  i <- NULL
  #--------------------------------------------------------------#

  # Matrix from SpatRaster
  rasmat <- stats::na.omit(terra::values(cuvar.rast))

  # Inputs as vectors
  cu <- as.vector(cu)
  vars <- as.vector(vars)
  dif <- as.vector(dif)

  # Define processing scheme (disk [parallel] VS memory [sequential])
  `%ps%` <- if(to.disk == TRUE) { foreach::`%dopar%` } else { foreach::`%do%` }

  # Loop function
  pdif <- foreach::foreach(i = 1:base::length(dif)) %ps% {

    if (dif[i] == 'PDF') {

      # Extract variable observations within specific CU
      var <- rasmat[base::which(rasmat[, cu.ind] == cu[i]), vars[i]]

      # Estimation of optimum number of regular bins for histogram
      var_hist <- histogram::histogram(var,
                                       type = hist.type,
                                       penalty = hist.pen,
                                       plot = FALSE,
                                       greedy = TRUE,
                                       verbose = FALSE
                                      )

      # Get number of bins as optimum grid size for KDE
      optgridsize <- base::length(var_hist[["breaks"]]) * grid.mult

      # Estimation of optimum bandwidth via the direct plugin method (DPI)
      optbw <- KernSmooth::dpik(var,
                                scalest = "iqr",
                                level = 2,
                                kernel = kern,
                                gridsize = optgridsize
                              )

      # KDE (binned)
      var_kde <- KernSmooth::bkde(var,
                                  kernel = kern,
                                  bandwidth = base::as.numeric(optbw),
                                  gridsize = optgridsize,
                                  truncate = FALSE
                                )

      # Create dataframe with variables for LOESS
      loess_df <- base::data.frame(x = c(1:base::length(var_kde[["x"]])),
                                   y = c(1:base::length(var_kde[["y"]]))
                                  )
      loess_df$x <- var_kde[["x"]]
      loess_df$y <- var_kde[["y"]]

      # Normalize KDE (0-100)
      loess_df$y <- scales::rescale(loess_df$y,
                                    to = c(0, 100),
                                    from = base::range(loess_df$y,
                                                       na.rm = TRUE,
                                                       finite = TRUE
                                                      )
                                  )
      gc()

    } else if (dif[i] == 'ECDF') {

      # Extract variable observations within specific CU
      var <- rasmat[base::which(rasmat[, cu.ind] == cu[i]), vars[i]]

      # Construction of ECDF
      var_ecdf <- stats::ecdf(var)

      # Create dataframe with quantiles for loess
      loess_df <- base::data.frame("x" = (stats::quantile(var,
                                                        probs = base::seq(0, 1,
                                                                      quant.sep
                                                                    )
                                                        )
                                        ),
                                   row.names = NULL
                                  )

      # ECDF to extracted quantiles
      loess_df$y <- var_ecdf(loess_df$x)

      # Normalize ECDF (0-100)
      loess_df$y <- loess_df$y * 100
      gc()

    } else if (dif[i] == 'iECDF') {

      # Extract variable observations within specific CU
      var <- rasmat[base::which(rasmat[, cu.ind] == cu[i]), vars[i]]

      # Construction of ECDF
      var_ecdf <- stats::ecdf(var)

      # Create data frame with quantiles for LOESS
      loess_df <- base::data.frame("x" = (stats::quantile(var,
                                                        probs = base::seq(0, 1,
                                                                      quant.sep
                                                                    )
                                                        )
                                        ),
                                   row.names = NULL
                                  )

      # ECDF to extracted quantiles
      loess_df$y <- var_ecdf(loess_df$x)

      # Normalize ECDF (0-100)
      loess_df$y <- loess_df$y * 100

      # Invert ECDF
      loess_df$y <- ((loess_df$y - max(loess_df$y)) * -1) + min(loess_df$y)
      gc()

    } else {

      if(verbose == TRUE){
     base::warning("Nothing was done. No distribution function was recognized.")
      }

    }

    # Fit LOESS
    loess_fit <- stats::loess(formula = y ~ x,
                              data = loess_df,
                              model = TRUE,
                              degree = 1,
                              span = span,
                              family = "gaussian",
                              method = "loess",
                              control = stats::loess.control(surface = "direct",
                                                            statistics = "exact"
                                                          )
                            )

    # Predict LOESS on variable across CUs (i.e., entire geographic space)
    loess_pred <- stats::predict(loess_fit, rasmat[, vars[i]])

    # Constrain range of LOESS predictions
    loess_pred <- ifelse(loess_pred > 100, 100.000, loess_pred)
    loess_pred <- ifelse(loess_pred < 0, 0, loess_pred)

    # Map LOESS predictions across entire geographic space supported by...
    # ...corresponding raster layer
    r <- cuvar.rast[[1]]
    r[!is.na(r)] <- loess_pred
    gc()

    # Layer name for output raster layer of predicted distribution function
    cname <- base::paste(prefix, cu[i], sep = "")
    lname <- base::paste(cname, vars[i], sep = "_")
    base::names(r) <- lname
    terra::varnames(r) <- lname

    if(to.disk == TRUE) {

      # > Disk-based writing < #

      # File name for output raster layer of predicted distribution function
      rastname <- base::paste(lname, extension, sep = "")

      # Write output raster layer of predicted distribution function
      terra::writeRaster(r,
                         base::file.path(outdir, rastname),
                         datatype = 'FLT4S',
                         ...
                        )
      gc()

      # Retrieve file name for raster layer of predicted distribution function
      rastname <- rastname

    } else {

      # > Memory-based writing < #

      # Retrieve raster layer of predicted distribution function

      r <- r

    }

  }

  if(to.disk == TRUE) {

    # Retrieve raster layers of predicted distribution function from disk
    pdif <- base::unlist(pdif)
    pdif <- base::paste("^", pdif, "$", sep = "")
    pdif <- base::lapply(pdif, list.files, path = outdir, full.names = TRUE)
    pdif <- terra::rast(base::unlist(pdif))

  } else {

    # Retrieve raster layers of predicted distribution function from memory
    pdif <- terra::rast(pdif)

  }

}
