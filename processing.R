
#########cropping rasters against red sea

library(terra)

input_folder <- "C:/Users/nasirh/OneDrive - KAUST/repositories/redseaecosystem/species_rasters"

shapefile_path <- "C:/Users/nasirh/OneDrive - KAUST/repositories/redseaecosystem/red_sea_shape"

output_folder <- "C:/Users/nasirh/OneDrive - KAUST/repositories/redseaecosystem/red_sea_species_rasters"

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

# Load boundary
boundary <- vect(shapefile_path)

# Find files in the input folder, even if extensions are hidden in Explorer
input_files <- list.files(
  input_folder,
  full.names = TRUE,
  recursive = FALSE
)

input_files <- input_files[!dir.exists(input_files)]

# Process rasters
for (file in input_files) {
  
  cat("\nProcessing:", basename(file), "\n")
  
  tryCatch({
    
    r <- rast(file)
    
    # Check CRS
    if (is.na(crs(r)) || crs(r) == "") {
      stop("Raster has no defined CRS.")
    }
    
    if (is.na(crs(boundary)) || crs(boundary) == "") {
      stop("Shapefile has no defined CRS.")
    }
    
    # Reproject boundary into raster CRS
    boundary_r <- project(boundary, crs(r))
    
    # Check for spatial overlap
    overlap <- intersect(ext(r), ext(boundary_r))
    
    if (is.null(overlap)) {
      cat("Skipped: raster does not overlap the boundary.\n")
      next
    }
    
    # Crop, retaining edge pixels
    r_crop <- crop(r, boundary_r, snap = "out")
    
    # Retain all pixels touched by the polygon
    r_clip <- mask(
      r_crop,
      boundary_r,
      touches = TRUE
    )
    
    # Save using original filename
    output_file <- file.path(
      output_folder,
      basename(file)
    )
    
    writeRaster(
      r_clip,
      output_file,
      overwrite = TRUE,
      gdal = c("COMPRESS=DEFLATE")
    )
    
    cat("Saved:", output_file, "\n")
    
    rm(r, boundary_r, r_crop, r_clip)
    gc(verbose = FALSE)
    
  }, error = function(e) {
    cat("Failed:", basename(file), "-", conditionMessage(e), "\n")
  })
}

cat("\nFinished processing rasters.\n")




#############checking file count 
# Folders
input_folder <- "C:/Users/nasirh/OneDrive - KAUST/repositories/redseaecosystem/species_rasters"

output_folder <- "C:/Users/nasirh/OneDrive - KAUST/repositories/redseaecosystem/red_sea_species_rasters"

# Get filenames from both folders
input_files <- list.files(input_folder, full.names = FALSE)
output_files <- list.files(output_folder, full.names = FALSE)

# Combine filenames and remove duplicates
all_files <- unique(c(input_files, output_files))

# Remove file extensions, if present
file_names <- tools::file_path_sans_ext(all_files)

# Extract family (everything before the first underscore)
families <- sub("_.*$", "", file_names)

# Remove empty entries
families <- families[nzchar(families)]

# Count unique families
unique_families <- sort(unique(families))

cat("Number of unique families:", length(unique_families), "\n")

# Print family names
print(unique_families)

# Count files per family
family_counts <- sort(table(families), decreasing = TRUE)

print(family_counts)


length(unique(sub(
  "_.*$", "",
  tools::file_path_sans_ext(
    c(
      list.files(input_folder),
      list.files(output_folder)
    )
  )
)))



#########creating grid


library(terra)

# --------------------------------------------------
# 1. Paths
# --------------------------------------------------

input_folder <- "C:/Users/nasirh/OneDrive - KAUST/repositories/redseaecosystem/red_sea_species_rasters"

output_file <- file.path(
  input_folder,
  "red_sea_species_grid.gpkg"
)

# --------------------------------------------------
# 2. Find raster files
# --------------------------------------------------

raster_files <- list.files(
  input_folder,
  pattern = "\\.(tif|tiff)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(raster_files) == 0) {
  stop("No TIFF rasters found in the input folder.")
}

cat("Rasters found:", length(raster_files), "\n")

# --------------------------------------------------
# 3. Read metadata for each raster
# --------------------------------------------------

raster_info <- do.call(
  rbind,
  lapply(raster_files, function(f) {
    
    r <- rast(f)
    e <- ext(r)
    
    data.frame(
      file = f,
      xmin = xmin(e),
      xmax = xmax(e),
      ymin = ymin(e),
      ymax = ymax(e),
      resolution_x = res(r)[1],
      resolution_y = res(r)[2],
      ncols = ncol(r),
      nrows = nrow(r),
      extent_area = (xmax(e) - xmin(e)) *
        (ymax(e) - ymin(e))
    )
  })
)

# --------------------------------------------------
# 4. Select raster with largest extent area
# --------------------------------------------------

reference_index <- which.max(raster_info$extent_area)

reference_file <- raster_info$file[reference_index]
reference_raster <- rast(reference_file)

cat("\nReference raster:\n", basename(reference_file), "\n")
cat("Extent area:", raster_info$extent_area[reference_index], "\n")
cat("Resolution:", res(reference_raster), "\n")
cat("Dimensions:", ncol(reference_raster), "columns x",
    nrow(reference_raster), "rows\n")
cat("Extent:\n")
print(ext(reference_raster))

# --------------------------------------------------
# 5. Check that all rasters use the same resolution
#    and grid alignment as the reference
# --------------------------------------------------

check_geometry <- function(f) {
  
  r <- rast(f)
  
  same_crs <- same.crs(r, reference_raster)
  same_res <- isTRUE(all.equal(
    res(r), res(reference_raster),
    tolerance = 1e-10
  ))
  
  # Check whether the pixel origins align
  same_origin <- isTRUE(all.equal(
    origin(r), origin(reference_raster),
    tolerance = 1e-8
  ))
  
  data.frame(
    file = f,
    same_crs = same_crs,
    same_resolution = same_res,
    same_origin = same_origin
  )
}

geometry_checks <- do.call(
  rbind,
  lapply(raster_files, check_geometry)
)

problems <- subset(
  geometry_checks,
  !same_crs | !same_resolution | !same_origin
)

if (nrow(problems) > 0) {
  print(problems)
  stop("Some rasters do not share the reference grid geometry.")
}

cat("\nAll rasters have matching CRS, resolution and origin.\n")

# --------------------------------------------------
# 6. Confirm expected grid dimensions
# --------------------------------------------------

expected_cells <- ncell(reference_raster)

cat("Expected number of grid cells:", expected_cells, "\n")

# --------------------------------------------------
# 7. Create exactly one polygon per raster cell
# --------------------------------------------------

# Create a raster with a unique ID for every cell
cell_id_raster <- reference_raster

values(cell_id_raster) <- seq_len(ncell(cell_id_raster))

# Convert cells to individual polygons
grid_vector <- as.polygons(
  cell_id_raster,
  aggregate = FALSE,
  values = TRUE,
  na.rm = FALSE
)

# Rename the raster value column to cell_id
names(grid_vector) <- "cell_id"

# Verify
cat("Expected cells:", ncell(reference_raster), "\n")
cat("Created polygons:", nrow(grid_vector), "\n")

stopifnot(nrow(grid_vector) == ncell(reference_raster))


# --------------------------------------------------
# 8. Retain complete grid squares intersecting Red Sea
# --------------------------------------------------

boundary_path <- "C:/Users/nasirh/OneDrive - KAUST/repositories/redseaecosystem/red_sea_shape"

# Load Red Sea boundary
red_sea_boundary <- vect(boundary_path)

# Ensure matching CRS
if (!same.crs(grid_vector, red_sea_boundary)) {
  red_sea_boundary <- project(
    red_sea_boundary,
    crs(grid_vector)
  )
}

# Dissolve boundary features
red_sea_boundary <- aggregate(red_sea_boundary)

cat("Original grid squares:", nrow(grid_vector), "\n")

# Find squares that intersect the boundary
overlapping_parts <- terra::intersect(
  grid_vector,
  red_sea_boundary
)

# Get IDs of intersecting squares
overlapping_ids <- unique(overlapping_parts$cell_id)

# Retain the original complete squares
grid_red_sea <- grid_vector[
  grid_vector$cell_id %in% overlapping_ids,
]

cat("Retained squares:", nrow(grid_red_sea), "\n")
cat(
  "Removed squares:",
  nrow(grid_vector) - nrow(grid_red_sea),
  "\n"
)

# Confirm that the original square geometries remain intact
stopifnot(
  all(grid_red_sea$cell_id %in% grid_vector$cell_id)
)



# --------------------------------------------------
# 9. Create row and column labels
# --------------------------------------------------

# Get one centroid per polygon
cell_centres <- centroids(grid_red_sea)

# Extract centroid coordinates
coords <- crds(cell_centres, df = TRUE)

# Assign one coordinate per grid cell
grid_red_sea$row_y <- coords[, 2]
grid_red_sea$col_x <- coords[, 1]

# Identify unique grid rows and columns
row_values <- sort(unique(grid_red_sea$row_y), decreasing = TRUE)
col_values <- sort(unique(grid_red_sea$col_x), decreasing = FALSE)

# Assign row and column indices
grid_red_sea$row_id <- match(
  grid_red_sea$row_y,
  row_values
)

grid_red_sea$col_id <- match(
  grid_red_sea$col_x,
  col_values
)

# Convert row numbers into letters:
# 1 = A, 2 = B, ..., 26 = Z, 27 = AA, etc.
number_to_letters <- function(n) {
  vapply(n, function(x) {
    label <- ""
    
    while (x > 0) {
      remainder <- (x - 1) %% 26
      label <- paste0(LETTERS[remainder + 1], label)
      x <- (x - 1) %/% 26
    }
    
    label
  }, character(1))
}

# Create labels such as A1, A2, B1, B2
grid_red_sea$grid_label <- paste0(
  number_to_letters(grid_red_sea$row_id),
  grid_red_sea$col_id
)

# --------------------------------------------------
# 10. Remove temporary coordinate/index fields
# --------------------------------------------------

grid_red_sea$row_y <- NULL
grid_red_sea$col_x <- NULL
grid_red_sea$row_id <- NULL
grid_red_sea$col_id <- NULL

# --------------------------------------------------
# 11. Export the complete, labelled squares
# --------------------------------------------------

writeVector(
  grid_red_sea,
  output_file,
  filetype = "GPKG",
  overwrite = TRUE
)

cat("\nFiltered grid saved to:\n", output_file, "\n")
cat("Retained cells:", nrow(grid_red_sea), "\n")
cat("Example labels:\n")
print(head(grid_red_sea$grid_label, 10))
