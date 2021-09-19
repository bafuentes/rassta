
# Create SpatRasters of continuous variables following a single Brownian motion
set.seed(963)
x <- rnorm(n = 99, sd = sqrt(0.01))
x <- c(1, cumsum(x))
v1 <- terra::rast(ncol = 10, nrow = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
v1[] <- 0 # To suppress warning
terra::values(v1) <- x
v2 <- terra::flip(terra::flip(v1, direction = "horizontal"))
vars <- c(v1, v2)
names(vars) <- c("v1", "v2")

# Create SpatRaster of classification units
set.seed(963)
a <- stats::kmeans(terra::as.matrix(c(v1, v2)), 3, iter.max = 100)
cu <- v1
terra::values(cu) <- a$cluster
names(cu) <- "cu"

# Select distribution functions for all variables and classification units
a <- select_functions(cu.rast = cu, var.rast = vars)
a <- a$distfun
a$Class.Unit <- as.integer(a$Class.Unit)

# Manual selection of distribution functions
m <- terra::as.matrix(c(cu, vars))
m <- as.data.frame(m)
mecdf1 <- m[which.max(m$v1), 1]
miecdf1 <- m[which.min(m$v1), 1]
mpdf1 <- setdiff(terra::unique(cu, incomparables = TRUE)[[1]],
                 c(mecdf1, miecdf1)
                )
mecdf2 <- m[which.max(m$v2), 1]
miecdf2 <- m[which.min(m$v2), 1]
mpdf2 <- setdiff(terra::unique(cu, incomparables = TRUE)[[1]],
                 c(mecdf2, miecdf2)
                )

# Test equality of classification units according to distribution functions
expect_equal(mecdf1,
             as.vector(as.matrix(a[which(a[,2] == "v1" & a[,3] == "ECDF"), 1]))
            )
expect_equal(miecdf1,
             as.vector(as.matrix(a[which(a[,2] == "v1" & a[,3] == "iECDF"), 1]))
            )
expect_equal(mpdf1,
             as.vector(as.matrix(a[which(a[,2] == "v1" & a[,3] == "PDF"), 1]))
            )
expect_equal(mecdf2,
             as.vector(as.matrix(a[which(a[,2] == "v2" & a[,3] == "ECDF"), 1]))
            )
expect_equal(miecdf2,
             as.vector(as.matrix(a[which(a[,2] == "v2" & a[,3] == "iECDF"), 1]))
            )
expect_equal(mpdf2,
             as.vector(as.matrix(a[which(a[,2] == "v2" & a[,3] == "PDF"), 1]))
            )
