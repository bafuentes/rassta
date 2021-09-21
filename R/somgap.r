#' @title
#' Self-Organizing Map and Selection of k
#'
#' @description
#' Produces a low-dimensional representation of the input feature space for
#' subsequent estimation of the "optimal" number of clusters (\emph{k}) in a
#' multivariate dataset. The dimensionality reduction is based on the
#' self-organizing map technique (SOM) of Kohonen (1982; 1990), and implemented
#' in R by the function \code{\link[kohonen]{supersom}} of Wehrens and
#' Kruisselbrink (2018). To estimate the optimal \emph{k}, the partitioning
#' around medoids (PAM) of Kaufman and Rousseeuw (1990), coupled with the gap
#' statistic of Tibshirani et al. (2001), is performed on the SOM's codebook
#' vectors. This is achieved by internally calling \code{\link[cluster]{pam}}
#' and \code{\link[cluster]{clusGap}} (Maechler et al., 2021). See
#' \emph{Details} for a brief theoretical background.
#'
#' @param var.rast SpatRaster, as in \code{\link[terra]{rast}}. This Multi-layer
#'   SpatRaster must contain \emph{n} continuous variables from which the SOM
#'   will be created.
#' @param xdim Integer. Horizontal dimension of the SOM's grid. Default: 12
#' @param ydim Integer. Vertical dimension of the SOM's grid. Default: 12
#' @param topo Character. Topology of the SOM's grid. Options = "rectangular",
#'   "hexagonal". Default: "hexagonal"
#' @param neighbourhood.fct Character. Neighborhood of the SOM's grid. Options =
#'   "bubble", "gaussian". Default: "gaussian"
#' @param rlen Integer. Number of times the complete dataset will be presented
#'   to the SOM's network. Default: 600
#' @param dist.fcts Character. Vector of length 2 containing the distance
#'   functions to use for SOM (First element, options = "sumofsquares",
#'   "euclidean", "manhattan") and for PAM (second element, options =
#'   "euclidean", "manhattan"). Default: c("sumofsquares", "manhattan")
#' @param mode Character. Type of learning algorithm. Options are “online",
#'   "batch", and "pbatch". Default: "pbatch"
#' @param K.max Integer. Maximum number of clusters to consider, must be at
#'   least two (2).
#' @param stand Boolean. For PAM function, does SOM's codebook vectors need to
#'   be standardized? Default: FALSE
#' @param B Integer. Number of bootstrap samples for the gap statistic. Default:
#'   500
#' @param d.power Integer. Positive Power applied to euclidean distances for the
#'   gap statistic. Default: 2
#' @param spaceH0 Character. Space of the reference distribution for the gap
#'   statistic. Options = "scaledPCA", "original" (See \strong{Details}).
#'   Default: "original"
#' @param method Character. Optimal k selection criterion for the gap statistic.
#'   Options = "globalmax", "firstmax", "Tibs2001SEmax", "firstSEmax",
#'   "globalSEmax". See  \code{\link[cluster]{clusGap}} for more details.
#'   Default: "globalSEmax"
#' @param SE.factor Numeric. Factor to feed into the standard error rule for the
#'   gap statistic. Only applicable for methods based on standard error (SE).
#'   See \code{\link[cluster]{clusGap}} for more details. Default: 1
#' @param ... Additional arguments as for \code{\link[kohonen]{supersom}}.
#'
#' @return
#' \strong{SOM}: An object of class \strong{kohonen} (see
#' \code{\link[kohonen]{supersom}}). The components of class kohonen returned by
#' this function are: (1) \emph{data} = original input matrix, (2)
#' \emph{unit.classif} = winning units for all observations, (3)
#' \emph{distances} = distance between each observation and its corresponding
#' winning unit, (4) \emph{grid} = object of class \strong{somgrid} (see
#' \code{\link[kohonen]{somgrid}}), (5) \emph{codes} = matrix of codebook
#' vectors, (6) \emph{changes} = matrix of mean average deviations from codebook
#' vectors, (7) \emph{dist.fcts} = selected distance function, and other
#' arguments passed to \code{\link[kohonen]{supersom}} (e.g., \emph{radius},
#' \emph{distance.weights}, etc.). Note that components 1, 2, and 3 will only be
#' returned if \emph{keep.data = TRUE}, which is the default.
#'
#' \strong{SOMdist}: Object of class \strong{dist}. Matrix of pairwise distances
#' calculated from the SOM's codebook vectors.
#'
#' \strong{SOMgap}: Object of class \strong{clusGap}. The main component of
#' class clusGap returned by this function is \emph{Tab}, which is a matrix of
#' the gap statistic results (see \code{\link[cluster]{clusGap}}). Additional
#' components are the arguments passed to the function (i.e., \emph{spaceH0},
#' \emph{B}), the PAM function, \emph{n} (number of observations) and
#' \emph{call} (the clusGap call-type object).
#'
#' \strong{Kopt}: Optimal \emph{k}, as selected by arguments \emph{method} and
#' (possibly) \emph{SE.factor}.
#'
#' @details
#' The clustering of SOM's codebook vectors has been proposed in several works,
#' notably in that from Vesanto and Alhoniemi (2000). These authors proposed a
#' two-stage clustering routine as an efficient method to reduce computational
#' load, while obtaining satisfactory correspondence between the clustered
#' codebook vectors and the clustered original feature space.
#'
#' The main purpose of this function is to allow the use of clustering and
#' k-selection algorithms that may result prohibitive for large datasets, such
#' as matrices derived from raster layers commonly used during geocomputational
#' routines. Thus, the SOM's codebook vectors can be subsequently used for the
#' calculation of distance matrices, which given the large size of their input
#' feature space, may otherwise be impossible to create due to insufficient
#' memory allocation capacity. Similarly, robust clustering algorithms that
#' require full pairwise distance matrices (e.g., hierarchical clustering, PAM)
#' and/or eigenvalues (e.g., spectral clustering) may also be performed on SOM's
#' codebook vectors.
#'
#' Note that \code{\link[kohonen]{supersom}} will internally equalize the
#' importance (i.e., weights) of variables such that differences in scale will
#' not affect distance calculations. This behavior can be prevented by setting
#' \emph{normalizeDataLayers = FALSE} in additional arguments passed to
#' \code{\link[kohonen]{supersom}}. Moreover, custom weights can also be passed
#' through the additional argument \emph{user.weights}. In such case, user
#' weights are applied on top of the internal weights.
#'
#' When working with large matrices, the additional SOM argument
#' \emph{keep.data} may be set to FALSE. However, note that by doing so, the
#' suggested follow-up function for raster products \code{\link{sompam}} will
#' not work since it requires both original data and winning units.
#'
#' For the gap statistic, \emph{method = "scaledPCA"} has resulted in errors for
#' R sessions with BLAS/LAPACK supported by the Intel Math Kernel Library (MKL).
#'
#' @examples
#' require(terra)
#' # Multi-layer SpatRaster with topographic variables
#' p <- system.file("exdat", package = "rassta")
#' tf <- list.files(path = p, pattern = "^height|^slope|^wetness",
#'                  full.names = TRUE
#'                 )
#' t <- rast(tf)
#' # Scale topographic variables (mean = 0, StDev = 1)
#' ts <- scale(t)
#' # Self-organizing map and gap statistic for optimum k
#' set.seed(963)
#' tsom <- somgap(var.rast = ts, xdim = 8, ydim = 8, rlen = 150,
#'                mode = "online", K.max = 6, B = 300, spaceH0 = "original",
#'                method = "globalSEmax"
#'               )
#' # Optimum k
#' tsom$Kopt
#'
#' @export
#' @family
#' Functions for Landscape Stratification
#' @rdname
#' somgap
#' @references
#' L. Kaufman and P. Rousseeuw. Finding groups in data: an introduction to
#' cluster analysis. John Wiley & Sons, 1990.
#' \doi{https://doi.org/10.1002/9780470316801}
#'
#' T. Kohonen. Self-organized formation of topologically correct feature maps.
#' Biological cybernetics, 43 (1):59–69, 1982.
#' \doi{https://doi.org/10.1007/bf00337288}
#'
#' T. Kohonen. The self-organizing map. Proceedings of the IEEE,
#' 78(9):1464–1480, 1990. \doi{https://doi.org/10.1016/s0925-2312(98)00030-7}
#'
#' M. Maechler, P. Rousseeuw, A. Struyf, M. Hubert, and K. Hornik. cluster:
#' Cluster Analysis Basics and Extensions, 2021.
#' \url{https://CRAN.R-project.org/package=cluster}
#'
#' R. Tibshirani, G. Walther, and T. Hastie. Estimating the number of clusters
#' in a data set via the gap statistic. Journal of the Royal Statistical
#' Society: Series B (Statistical Methodology), 63(2):411–423, 2001.
#' \doi{https://doi.org/10.1111/1467-9868.00293}
#'
#' J. Vesanto and E. Alhoniemi. Clustering of the self-organizing map. IEEE
#' Transactions on Neural Networks, 11(3):586–600, 2000.
#' \doi{https://doi.org/10.1109/72.846731}
#'
#' R. Wehrens and J. Kruisselbrink. Flexible self-organizing maps in kohonen
#' 3.0. Journal of Statistical Software, 87(1):1–18, 2018.
#' \doi{https://doi.org/10.18637/jss.v087.i07}
#'
somgap <- function(var.rast, xdim = 12, ydim = 12, topo = "hexagonal",
                   neighbourhood.fct = "gaussian", rlen = 600,
                   dist.fcts = c("sumofsquares", "manhattan"), mode = "pbatch",
                   K.max, stand = FALSE, B = 500, d.power = 2,
                   spaceH0 = "original", method = "globalSEmax",
                   SE.factor = 1, ...)
{

  # Build SOM grid
  som_grid = kohonen::somgrid(xdim = xdim,
                              ydim = ydim,
                              topo = topo,
                              neighbourhood.fct = neighbourhood.fct
                            )

  # Construct matrix from SpatRast
  rm <- terra::as.matrix(var.rast)
  rm <- rm[stats::complete.cases(rm), ]

  # Get SOM
  som_map = kohonen::supersom(data = rm,
                              grid = som_grid,
                              rlen = rlen,
                              dist.fcts = dist.fcts[1],
                              mode = mode,
                              ...
                            )

  # Create matrix from SOM codes and save output
  som_codes <- kohonen::getCodes(som_map)

  # Get distance matrix from SOM codes
  som_dist <- kohonen::object.distances(som_map, type = "codes")

  # Define function for Partitioning Around Medoids (PAM)-based clustering
  pamgap <- function(som_codes, K.max) {
    base::list(cluster = cluster::pam(som_codes,
                                      k = K.max,
                                      diss = FALSE,
                                      metric = dist.fcts[2],
                                      stand = stand,
                                      cluster.only = TRUE,
                                      do.swap = TRUE,
                                      keep.diss = FALSE,
                                      keep.data = FALSE
                                    )
              )
  }

  # Gap statistic
  som_pam_gap <- cluster::clusGap(som_codes,
                                  FUNcluster = pamgap,
                                  K.max = K.max,
                                  B = B,
                                  d.power = d.power,
                                  spaceH0 = spaceH0
                                )

  # Selection of k
  k_opt <- cluster::maxSE(f = som_pam_gap[["Tab"]][, 3],
                          SE.f = som_pam_gap[["Tab"]][ ,4],
                          method = method,
                          SE.factor = SE.factor
                        )

  # Return list of components
  list(SOM = som_map, SOMdist = som_dist, SOMgap = som_pam_gap, Kopt = k_opt)

}
