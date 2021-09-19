# Create SpatRaster with 2 blocks of categorical values (1 & 2)
r <- terra::rast(ncol = 4, nrow = 4, xmin=0, xmax=1, ymin=0, ymax=1)
r[] <- 0 # To suppress warning
terra::values(r)[1:8] <- 1
terra::values(r)[9:16] <- 2

# Test equality of binary layers
expect_equal(terra::values(r * 100)[1:8], terra::values(dummies(r)[[1]])[1:8])
expect_equal(terra::values(r + 98)[9:16], terra::values(dummies(r)[[2]])[9:16])
