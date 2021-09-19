
# Create SpatRaster of stratification units (3 blocks of categorical values)
su <- terra::rast(ncol = 9, nrow = 9, xmin=0, xmax=1, ymin=0, ymax=1)
su[] <- 0 # To suppress warning
terra::values(su)[1:27] <- 11
terra::values(su)[28:54] <- 22
terra::values(su)[55:81] <- 33
names(su) <- "SU"

# Create SpatRasters of spatial signatures following a single Brownian motion
set.seed(963)
x <- rnorm(n = 80, sd = sqrt(0.01))
x <- c(1, cumsum(x))
v1 <- su
terra::values(v1) <- x
v2 <- v3 <- v5 <- v1
terra::values(v3)[28:54] <- terra::values(v1)[1:27]
terra::values(v3)[1:27] <- terra::values(v1)[54:28]
v4 <- v3
terra::values(v5)[55:81] <- terra::values(v1)[1:27]
terra::values(v5)[1:27] <- terra::values(v1)[81:55]
v6 <- v5
sigs <- c(v1,v2,v3,v4,v5,v6)
names(sigs) <- c("clim_1", "terr_1", "clim_2", "terr_2", "clim_3", "terr_3")

# Calculate landscape similarity to stratification units
ls <- similarity(su.rast = su,
                 su.code = list(clim = c(1,1), terr = c(2,2)),
                 sig.rast = sigs
                )

# Manual calculation of landscape similarity to stratification units
mls1 <- (terra::values(v1) + terra::values(v2))/2
mls2 <- (terra::values(v3) + terra::values(v4))/2
mls3 <- (terra::values(v5) + terra::values(v6))/2

# Test equality of landscape similarities
expect_equal(as.vector(mls1), as.vector(terra::values(ls$landsim[[1]])))
expect_equal(as.vector(mls2), as.vector(terra::values(ls$landsim[[2]])))
expect_equal(as.vector(mls3), as.vector(terra::values(ls$landsim[[3]])))
