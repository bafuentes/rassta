
# Create SpatRasters of predicted distribution functions following a single...
# ...Brownian motion
set.seed(963)
x <- rnorm(n = 99, sd = sqrt(0.01))
x <- c(1, cumsum(x))
v1 <- terra::rast(ncol = 10, nrow = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
v1[] <- 0 # To suppress warning
terra::values(v1) <- x
v2 <- terra::flip(terra::flip(v1), direction = "horizontal")

# Multi-layer SpatRaster with predicted distribution functions
predif <- c(v1, v2)
base::names(predif) <- c("1_v1", "1_v2")

# Calculate spatial signature
sig <- signature(pdif.rast = predif, inprex = "1_", outname = "sig")

# Manual calculation of spatial signature
msig <- (terra::as.matrix(v1) + terra::as.matrix(v2))/2

# Test equality of spatial signatures
expect_equal(as.vector(msig), as.vector(terra::values(sig)))
