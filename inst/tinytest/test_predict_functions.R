
# Create SpatRasters of continuous variables following a single Brownian motion
set.seed(963)
x <- rnorm(n = 99, sd = sqrt(0.01))
x <- c(1, cumsum(x))
v1 <- terra::rast(ncol = 10, nrow = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
v1[] <- 0 # To suppress warning
terra::values(v1) <- x
v2 <- terra::flip(terra::flip(v1, direction = "horizontal"))

# Create SpatRaster of classification units
set.seed(963)
a <- stats::kmeans(terra::as.matrix(c(v1, v2)), 2, iter.max = 100)
cu <- v1
terra::values(cu) <- a$cluster

# Multi-layer SpatRaster with variables and classification units
cuvar <- c(cu, v1, v2)
base::names(cuvar) <- c("cu", "v1", "v2")

# Predict distribution function for first variable and classification unit
a <- predict_functions(cuvar.rast = cuvar, cu.ind = 1, cu = 2, vars = "v1",
                       dif = "PDF", grid.mult = 6, span = 1
                      )

# Manual prediction of distribution function
m <- terra::as.matrix(cuvar)
mcu <- m[base::which(m[, 1] == 2), "v1"]
mhist <- histogram::histogram(mcu, type = "regular", plot = FALSE,
                              greedy = TRUE, verbose = FALSE
                            )
optbw <- KernSmooth::dpik(mcu, scalest = "iqr", level = 2,
                          kernel = "normal",
                          gridsize = base::length(mhist[["breaks"]]) * 6
                        )
var_kde <- KernSmooth::bkde(mcu, kernel = "normal",
                            bandwidth = base::as.numeric(optbw),
                            gridsize = base::length(mhist[["breaks"]]) * 6,
                            truncate = FALSE
                          )
loess_df <- base::data.frame(x = c(1:base::length(var_kde[["x"]])),
                             y = c(1:base::length(var_kde[["y"]]))
                            )
loess_df$x <- var_kde[["x"]]
loess_df$y <- var_kde[["y"]]
loess_df$y <- scales::rescale(loess_df$y,
                              to = c(0, 100),
                              from = base::range(loess_df$y,
                                                 na.rm = TRUE,
                                                 finite = TRUE
                                                )
                            )
loess_fit <- stats::loess(formula = y ~ x, data = loess_df, model = TRUE,
                          degree = 1, span = 1, family = "gaussian",
                          method = "loess",
                          control = stats::loess.control(surface = "direct",
                                                         statistics = "exact"
                                                        )
                        )
loess_pred <- stats::predict(loess_fit, m[, 2])
loess_pred <- ifelse(loess_pred > 100, 100.000, loess_pred)
loess_pred <- ifelse(loess_pred < 0, 0, loess_pred)

# Test equality of predicted distribution functions
expect_equal(as.vector(loess_pred), as.vector(terra::values(a)))
