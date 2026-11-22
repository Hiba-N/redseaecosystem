# ============================================================
# RED SEA: PROBABILITY-WEIGHTED NUTRIENT MAPPING
# ============================================================
#
# Method:
#   Probability-weighted mean nutrient concentration
#
# Formula:
#   Weighted mean = sum(P_i * C_i) / sum(P_i)
#
# Where:
#   P_i = AquaMaps occurrence probability for species i
#   C_i = Nutrient concentration for species i
#
# Nutrient units are retained exactly as provided in Excel.
# No unit conversion is performed.
#
# ============================================================


# ------------------------------------------------------------
# 1. LIBRARIES AND SETUP
# ------------------------------------------------------------

library(terra)
library(dplyr)
library(readxl)
library(stringr)


# ------------------------------------------------------------
# 2. INPUT AND OUTPUT PATHS
# ------------------------------------------------------------

project_folder <- paste0(
  "C:/Users/nasirh/OneDrive - KAUST/",
  "repositories/redseaecosystem"
)

# Excel nutrient workbook
excel_path <- file.path(
  project_folder,
  "Nutrition_species_workbook_Mili_update.xlsx"
)

# Red Sea species probability rasters
raster_folder <- file.path(
  project_folder,
  "all_probabilities_clipped"
)

# Labelled Red Sea grid
grid_path <- file.path(
  project_folder,
  "red_sea_species_grid.gpkg"
)

# Main output folder
output_folder <- file.path(
  project_folder,
  "probability_weighted_nutrients"
)

# Output subfolders
matched_raster_folder <- file.path(
  output_folder,
  "matched_species_rasters"
)

nutrient_raster_folder <- file.path(
  output_folder,
  "weighted_nutrient_rasters"
)

diagnostic_folder <- file.path(
  output_folder,
  "diagnostic_rasters"
)

# Create folders
folders <- c(
  output_folder,
  matched_raster_folder,
  nutrient_raster_folder,
  diagnostic_folder
)

invisible(lapply(
  folders,
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))


# ------------------------------------------------------------
# 3. DEFINE NUTRIENT COLUMNS AND EXPECTED UNITS
# ------------------------------------------------------------

# Source column names must match the Excel headers exactly.
#
# Expected units are used for validation only.
# No conversion is performed.
#
# The output columns retain the same names and units.

nutrient_map <- data.frame(
  
  source_col = c(
    "AFCD protein (g)",
    "AFCD omega3_epa_dha (g)",
    "AFCD vitamin_a (mcg)",
    "AFCD vitamin_b12 (mcg)",
    "AFCD vitamin_c (mg)",
    "AFCD vitamin_b6 (mg)",
    "AFCD vitamin_d (mcg)",
    "AFCD calcium (mg)",
    "AFCD iron_total (mg)",
    "AFCD magnesium (mg)",
    "AFCD selenium (mcg)",
    "AFCD zinc (mg)",
    "AFCD iodine (mcg)"
  ),
  
  output_col = c(
    "AFCD protein (g)",
    "AFCD omega3_epa_dha (g)",
    "AFCD vitamin_a (mcg)",
    "AFCD vitamin_b12 (mcg)",
    "AFCD vitamin_c (mg)",
    "AFCD vitamin_b6 (mg)",
    "AFCD vitamin_d (mcg)",
    "AFCD calcium (mg)",
    "AFCD iron_total (mg)",
    "AFCD magnesium (mg)",
    "AFCD selenium (mcg)",
    "AFCD zinc (mg)",
    "AFCD iodine (mcg)"
  ),
  
  expected_unit = c(
    "g",
    "g",
    "mcg",
    "mcg",
    "mg",
    "mg",
    "mcg",
    "mg",
    "mg",
    "mg",
    "mcg",
    "mg",
    "mcg"
  ),
  
  stringsAsFactors = FALSE
)

nutrient_columns <- nutrient_map$output_col


# ------------------------------------------------------------
# 4. READ EXCEL NUTRIENT DATA
# ------------------------------------------------------------

nutrient_raw <- read_excel(
  excel_path,
  sheet = "Saudi seafood species",
  skip = 1
)

# Check required columns
required_columns <- c(
  "Scientific name",
  nutrient_map$source_col
)

missing_columns <- setdiff(
  required_columns,
  names(nutrient_raw)
)

if (length(missing_columns) > 0) {
  
  stop(
    "Missing Excel columns: ",
    paste(missing_columns, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 5. VALIDATE NUTRIENT UNITS
# ------------------------------------------------------------

# Extract the unit from each source column header.

header_units <- stringr::str_match(
  nutrient_map$source_col,
  "\\(([^()]*)\\)\\s*$"
)[, 2]

# Compare header units against expected units.

unit_check <- data.frame(
  Nutrient = nutrient_map$output_col,
  Header_Unit = header_units,
  Expected_Unit = nutrient_map$expected_unit,
  stringsAsFactors = FALSE
)

unit_check$Valid <- (
  tolower(trimws(unit_check$Header_Unit)) ==
    tolower(trimws(unit_check$Expected_Unit))
)

cat("\nNUTRIENT UNIT VALIDATION\n")
print(unit_check)

if (any(!unit_check$Valid)) {
  
  stop(
    "Nutrient unit validation failed. ",
    "Check the Excel column headers and nutrient_map."
  )
}

cat("\nAll nutrient units validated successfully.\n")


# ------------------------------------------------------------
# 6. EXTRACT AND STANDARDISE NUTRIENT DATA
# ------------------------------------------------------------

nutrient_data <- nutrient_raw %>%
  
  select(all_of(required_columns)) %>%
  
  mutate(
    `Scientific name` = str_squish(
      str_replace_all(
        as.character(`Scientific name`),
        "\u00A0",
        " "
      )
    )
  )

# Convert nutrient columns to numeric.
# This does NOT change their measurement units.

for (i in seq_len(nrow(nutrient_map))) {
  
  source_col <- nutrient_map$source_col[i]
  
  original_values <- nutrient_data[[source_col]]
  
  character_values <- trimws(
    as.character(original_values)
  )
  
  # Remove thousands separators
  cleaned_values <- gsub(
    ",",
    "",
    character_values,
    fixed = TRUE
  )
  
  numeric_values <- suppressWarnings(
    as.numeric(cleaned_values)
  )
  
  # Identify non-empty values that could not be converted.
  invalid_values <- (
    !is.na(original_values) &
      character_values != "" &
      is.na(numeric_values)
  )
  
  if (any(invalid_values)) {
    
    warning(
      sum(invalid_values),
      " non-numeric values found in ",
      source_col,
      ". These will be treated as missing nutrient measurements."
    )
  }
  
  nutrient_data[[source_col]] <- numeric_values
}


# ------------------------------------------------------------
# 7. REMOVE INVALID SPECIES NAMES AND CHECK DUPLICATES
# ------------------------------------------------------------

nutrient_data <- nutrient_data %>%
  
  filter(
    !is.na(`Scientific name`),
    `Scientific name` != ""
  )


# Normalise species names for matching.

normalise_name <- function(x) {
  
  x <- as.character(x)
  
  x <- str_replace_all(
    x,
    "_",
    " "
  )
  
  tolower(
    str_squish(x)
  )
}

nutrient_data <- nutrient_data %>%
  
  mutate(
    match_key = normalise_name(`Scientific name`)
  )


# Check duplicate scientific names.

if (anyDuplicated(nutrient_data$match_key)) {
  
  duplicate_names <- nutrient_data %>%
    count(match_key) %>%
    filter(n > 1)
  
  print(duplicate_names)
  
  stop(
    "Duplicate scientific names in nutrient workbook. ",
    "Resolve duplicates before proceeding."
  )
}


# Save standardised nutrient data.

write.csv(
  nutrient_data %>%
    select(-match_key),
  
  file.path(
    output_folder,
    "red_sea_fish_nutrients_standardised.csv"
  ),
  
  row.names = FALSE,
  na = ""
)


# Save nutrient unit information.

write.csv(
  unit_check,
  
  file.path(
    output_folder,
    "nutrient_unit_validation.csv"
  ),
  
  row.names = FALSE
)

message("Nutrient data extracted and validated.")


# ------------------------------------------------------------
# 8. CREATE AQUAMAPS RASTER LOOKUP
# ------------------------------------------------------------

raster_files <- list.files(
  raster_folder,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

if (length(raster_files) == 0) {
  
  stop(
    "No GeoTIFF files found in: ",
    raster_folder
  )
}


# Match raster filenames to scientific names.

raster_lookup <- data.frame(
  
  raster_file = raster_files,
  
  raster_name = tools::file_path_sans_ext(
    basename(raster_files)
  ),
  
  stringsAsFactors = FALSE
  
) %>%
  
  mutate(
    match_key = normalise_name(raster_name)
  )


# Check for duplicate raster matching keys.

if (anyDuplicated(raster_lookup$match_key)) {
  
  duplicate_rasters <- raster_lookup %>%
    count(match_key) %>%
    filter(n > 1)
  
  print(duplicate_rasters)
  
  stop(
    "Multiple raster files have the same normalised species name. ",
    "Resolve duplicates before proceeding."
  )
}

cat("\nRaster files available:", nrow(raster_lookup), "\n")


# ------------------------------------------------------------
# 9. MATCH NUTRIENT DATA TO AQUAMAPS RASTERS
# ------------------------------------------------------------

matched_species <- nutrient_data %>%
  
  left_join(
    raster_lookup %>%
      select(
        match_key,
        raster_file
      ),
    
    by = "match_key"
  )


# Identify species with at least one nutrient measurement.

has_nutrient_data <- rowSums(
  !is.na(
    matched_species[, nutrient_columns, drop = FALSE]
  )
) > 0


# Retain species with:
# 1. A matching AquaMaps raster.
# 2. At least one available nutrient measurement.

matched_species <- matched_species[
  !is.na(matched_species$raster_file) &
    has_nutrient_data,
  ,
  drop = FALSE
]


# Check matching results.

cat("\nSPECIES MATCHING SUMMARY\n")
cat("------------------------------\n")

cat(
  "Species in nutrient workbook:",
  nrow(nutrient_data),
  "\n"
)

cat(
  "Species with matching rasters:",
  sum(!is.na(
    nutrient_data$match_key %in%
      raster_lookup$match_key
  )),
  "\n"
)

cat(
  "Species retained for analysis:",
  nrow(matched_species),
  "\n"
)


if (nrow(matched_species) == 0) {
  
  stop(
    "No species have both nutrient data and matching rasters."
  )
}


# Save matched species nutrient table.

write.csv(
  matched_species %>%
    select(
      `Scientific name`,
      all_of(nutrient_columns),
      raster_file
    ),
  
  file.path(
    output_folder,
    "red_sea_fish_nutrients_with_matching_rasters.csv"
  ),
  
  row.names = FALSE,
  na = ""
)


# ------------------------------------------------------------
# 10. LOAD GRID AND CREATE REFERENCE TEMPLATE
# ------------------------------------------------------------

# Load Red Sea grid.

grid <- terra::vect(grid_path)

if (terra::crs(grid) == "") {
  
  stop("The Red Sea grid has no defined CRS.")
}


# Use the first matched raster as the reference
# for resolution, origin and CRS.

reference_raster <- terra::rast(
  matched_species$raster_file[1]
)

if (terra::crs(reference_raster) == "") {
  
  stop("Reference raster has no defined CRS.")
}


# Project grid into reference raster CRS.

grid_projected <- terra::project(
  grid,
  terra::crs(reference_raster)
)


# Extend the reference raster to cover the entire grid extent.
# This preserves the reference raster's resolution and alignment.

template <- terra::extend(
  reference_raster,
  terra::ext(grid_projected),
  snap = "out"
)


# Mask the template to the Red Sea grid.

template <- terra::mask(
  template,
  grid_projected
)


# Remove unnecessary reference raster values.
# The template now defines the geometry for all output rasters.

template <- terra::init(
  template,
  0
)


cat("\nREFERENCE TEMPLATE CREATED\n")
cat("------------------------------\n")
cat("CRS:", terra::crs(template), "\n")
cat("Resolution:", terra::res(template), "\n")
cat("Rows:", terra::nrow(template), "\n")
cat("Columns:", terra::ncol(template), "\n")


# ------------------------------------------------------------
# 11. INITIALISE NUTRIENT NUMERATORS AND DENOMINATORS
# ------------------------------------------------------------

# One numerator and denominator raster for each nutrient.
#
# Numerator:
# Sum of probability * nutrient concentration.
#
# Denominator:
# Sum of occurrence probabilities for species
# with available measurements for that nutrient.

numerator <- setNames(
  
  lapply(
    nutrient_columns,
    function(x) terra::init(template, 0)
  ),
  
  nutrient_columns
)


denominator <- setNames(
  
  lapply(
    nutrient_columns,
    function(x) terra::init(template, 0)
  ),
  
  nutrient_columns
)


# Count of species contributing a positive probability
# to each nutrient at each pixel.

species_count <- setNames(
  
  lapply(
    nutrient_columns,
    function(x) terra::init(template, 0)
  ),
  
  nutrient_columns
)


# ------------------------------------------------------------
# 12. PROCESS SPECIES PROBABILITY RASTERS
# ------------------------------------------------------------

total_species <- nrow(matched_species)

for (i in seq_len(total_species)) {
  
  sp_name <- matched_species$`Scientific name`[i]
  
  raster_path <- matched_species$raster_file[i]
  
  cat(
    "\n[", i, "/", total_species, "] ",
    round(i / total_species * 100, 1),
    "% | ",
    sp_name,
    "\n",
    sep = ""
  )
  
  
  # ----------------------------------------------------------
  # 12.1 Read species raster
  # ----------------------------------------------------------
  
  r <- terra::rast(raster_path)
  
  if (terra::nlyr(r) != 1) {
    
    stop(
      "Expected a single-layer raster for: ",
      sp_name
    )
  }
  
  if (terra::crs(r) == "") {
    
    stop(
      "Raster has no defined CRS: ",
      sp_name
    )
  }
  
  
  # ----------------------------------------------------------
  # 12.2 Align raster to template geometry
  # ----------------------------------------------------------
  
  if (!terra::same.crs(r, template)) {
    
    # Reproject and align if CRS differs.
    
    r <- terra::project(
      r,
      template,
      method = "near"
    )
    
  } else if (!terra::compareGeom(
    r,
    template,
    stopOnError = FALSE
  )) {
    
    # Resample if CRS is identical but geometry differs.
    
    r <- terra::resample(
      r,
      template,
      method = "near"
    )
  }
  
  
  # Confirm exact geometry.
  
  if (!terra::compareGeom(
    r,
    template,
    stopOnError = FALSE
  )) {
    
    stop(
      "Raster could not be aligned to template: ",
      sp_name
    )
  }
  
  
  # ----------------------------------------------------------
  # 12.3 Mask to Red Sea grid
  # ----------------------------------------------------------
  
  r <- terra::mask(
    r,
    grid_projected
  )
  
  
  # ----------------------------------------------------------
  # 12.4 Save matched species raster
  # ----------------------------------------------------------
  
  matched_output_path <- file.path(
    matched_raster_folder,
    basename(raster_path)
  )
  
  terra::writeRaster(
    r,
    filename = matched_output_path,
    overwrite = TRUE,
    gdal = c(
      "COMPRESS=LZW",
      "TILED=YES"
    )
  )
  
  
  # ----------------------------------------------------------
  # 12.5 Handle missing occurrence probabilities
  # ----------------------------------------------------------
  
  # Assumption:
  # NA values within the Red Sea grid are treated as
  # zero occurrence probability.
  #
  # This assumes that absent cells in the AquaMaps raster
  # represent no predicted occurrence rather than missing data.
  
  probability <- terra::ifel(
    is.na(r),
    0,
    r
  )
  
  
  # Ensure cells outside the Red Sea grid remain NA.
  
  probability <- terra::mask(
    probability,
    grid_projected
  )
  
  
  # ----------------------------------------------------------
  # 12.6 Update nutrient calculations
  # ----------------------------------------------------------
  
  for (nutrient in nutrient_columns) {
    
    concentration <- matched_species[[nutrient]][i]
    
    
    # Skip species with missing nutrient measurements.
    #
    # Such species contribute neither to the numerator
    # nor to the denominator for this nutrient.
    
    if (is.na(concentration)) {
      next
    }
    
    
    # Numerator:
    # Probability multiplied by nutrient concentration.
    
    numerator[[nutrient]] <-
      
      numerator[[nutrient]] +
      
      probability * concentration
    
    
    # Denominator:
    # Sum of probabilities for species with this
    # nutrient measurement available.
    
    denominator[[nutrient]] <-
      
      denominator[[nutrient]] +
      
      probability
    
    
    # Count species with positive occurrence probability.
    
    species_count[[nutrient]] <-
      
      species_count[[nutrient]] +
      
      terra::ifel(
        probability > 0,
        1,
        0
      )
    
  }
  
  
  # Release temporary objects.
  
  rm(r, probability)
  
  gc(verbose = FALSE)
  
}


# ------------------------------------------------------------
# 13. CALCULATE FINAL WEIGHTED MEAN NUTRIENT RASTERS
# ------------------------------------------------------------

for (nutrient in nutrient_columns) {
  
  cat("\nCalculating weighted mean:", nutrient, "\n")
  
  
  # ----------------------------------------------------------
  # 13.1 Calculate probability-weighted mean
  # ----------------------------------------------------------
  
  weighted_raster <- terra::ifel(
    
    denominator[[nutrient]] > 0,
    
    numerator[[nutrient]] /
      denominator[[nutrient]],
    
    NA
    
  )
  
  
  # Mask outside the Red Sea grid.
  
  weighted_raster <- terra::mask(
    weighted_raster,
    grid_projected
  )
  
  
  # Set raster layer name.
  
  names(weighted_raster) <- nutrient
  
  
  # ----------------------------------------------------------
  # 13.2 Save weighted mean nutrient raster
  # ----------------------------------------------------------
  
  output_name <- paste0(
    gsub(
      "[^A-Za-z0-9_-]",
      "_",
      nutrient
    ),
    "_weighted_mean.tif"
  )
  
  
  terra::writeRaster(
    
    weighted_raster,
    
    filename = file.path(
      nutrient_raster_folder,
      output_name
    ),
    
    overwrite = TRUE,
    
    gdal = c(
      "COMPRESS=LZW",
      "TILED=YES"
    )
    
  )
  
  
  # ----------------------------------------------------------
  # 13.3 Save summed occurrence probability
  # ----------------------------------------------------------
  
  # This shows the total occurrence probability
  # contributing to the weighted mean.
  
  probability_name <- paste0(
    gsub(
      "[^A-Za-z0-9_-]",
      "_",
      nutrient
    ),
    "_summed_probability.tif"
  )
  
  
  probability_raster <- terra::mask(
    denominator[[nutrient]],
    grid_projected
  )
  
  
  terra::writeRaster(
    
    probability_raster,
    
    filename = file.path(
      diagnostic_folder,
      probability_name
    ),
    
    overwrite = TRUE,
    
    gdal = c(
      "COMPRESS=LZW",
      "TILED=YES"
    )
    
  )
  
  
  # ----------------------------------------------------------
  # 13.4 Save contributing species count
  # ----------------------------------------------------------
  
  # This shows how many species with available nutrient
  # measurements have positive occurrence probability
  # in each pixel.
  
  count_name <- paste0(
    gsub(
      "[^A-Za-z0-9_-]",
      "_",
      nutrient
    ),
    "_contributing_species_count.tif"
  )
  
  
  count_raster <- terra::mask(
    species_count[[nutrient]],
    grid_projected
  )
  
  
  terra::writeRaster(
    
    count_raster,
    
    filename = file.path(
      diagnostic_folder,
      count_name
    ),
    
    overwrite = TRUE,
    
    gdal = c(
      "COMPRESS=LZW",
      "TILED=YES"
    )
    
  )
  
  
  message("Saved: ", output_name)
  
}


# ------------------------------------------------------------
# 14. FINAL SPECIES MATCHING SUMMARY
# ------------------------------------------------------------

matched_names <- matched_species$match_key

unmatched_species <- nutrient_data %>%
  
  filter(
    !match_key %in% matched_names
  ) %>%
  
  select(
    `Scientific name`
  )


write.csv(
  
  unmatched_species,
  
  file.path(
    output_folder,
    "species_without_matching_rasters.csv"
  ),
  
  row.names = FALSE
)


# ------------------------------------------------------------
# 15. FINAL SUMMARY
# ------------------------------------------------------------

cat("\n========================================\n")
cat("NUTRIENT MAPPING COMPLETE\n")
cat("========================================\n")


cat(
  "Species in nutrient workbook: ",
  nrow(nutrient_data),
  "\n"
)

cat(
  "Species with matching rasters: ",
  nrow(matched_species),
  "\n"
)

cat(
  "Species without matching rasters: ",
  nrow(unmatched_species),
  "\n"
)

cat(
  "Nutrient layers created: ",
  length(nutrient_columns),
  "\n"
)


cat("\nOUTPUT LOCATIONS\n")
cat("------------------------------\n")

cat(
  "Standardised nutrient data:\n",
  output_folder,
  "\n\n"
)

cat(
  "Matched species rasters:\n",
  matched_raster_folder,
  "\n\n"
)

cat(
  "Weighted mean nutrient rasters:\n",
  nutrient_raster_folder,
  "\n\n"
)

cat(
  "Diagnostic rasters:\n",
  diagnostic_folder,
  "\n"
)


cat("\n========================================\n")
cat("ALL PROCESSING FINISHED\n")
cat("========================================\n")




# ============================================================
# 16. CONVERT WEIGHTED NUTRIENT RASTERS TO PERCENTAGE OF RDA
# ============================================================

# ------------------------------------------------------------
# 16.1 PATHS
# ------------------------------------------------------------

project_folder <- paste0(
  "C:/Users/nasirh/OneDrive - KAUST/",
  "repositories/redseaecosystem"
)

output_folder <- file.path(
  project_folder,
  "probability_weighted_nutrients"
)

input_folder <- file.path(
  output_folder,
  "weighted_nutrient_rasters"
)

percent_rda_folder <- file.path(
  output_folder,
  "percent_RDA_rasters_capped"
)

dir.create(
  percent_rda_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 16.3 DAILY REFERENCE VALUES
# ------------------------------------------------------------

rda_reference <- data.frame(
  
  nutrient = c(
    "AFCD protein (g)",
    "AFCD omega3_epa_dha (g)",
    "AFCD vitamin_a (mcg)",
    "AFCD vitamin_b12 (mcg)",
    "AFCD vitamin_c (mg)",
    "AFCD vitamin_b6 (mg)",
    "AFCD vitamin_d (mcg)",
    "AFCD calcium (mg)",
    "AFCD iron_total (mg)",
    "AFCD magnesium (mg)",
    "AFCD selenium (mcg)",
    "AFCD zinc (mg)",
    "AFCD iodine (mcg)"
  ),
  
  reference_value = c(
    47.823,
    0.4333,
    767.727,
    2.1272727,
    73.8636,
    1.331818,
    15,
    1025.5,
    11.97,
    299.77273,
    50.2273,
    8.909,
    173
  ),
  
  unit = c(
    "g", "g",
    "mcg", "mcg",
    "mg", "mg",
    "mcg", "mg",
    "mg", "mg",
    "mcg", "mg",
    "mcg"
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 16.4 PROCESSING SETTINGS
# ------------------------------------------------------------

# Start with TRUE to test calcium only.
# After confirming that it works, change to FALSE.

# Keep values above 100%.
cap_at_100 <- TRUE

nutrients_to_process <- seq_len(nrow(rda_reference))



# ------------------------------------------------------------
# 16.5 PROCESS RASTERS ONE AT A TIME
# ------------------------------------------------------------

for (i in nutrients_to_process) {
  
  nutrient <- rda_reference$nutrient[i]
  
  reference_value <- rda_reference$reference_value[i]
  
  cat("\n----------------------------------------\n")
  cat("Processing:", nutrient, "\n")
  cat("Reference:", reference_value, rda_reference$unit[i], "\n")
  
  
  # Create input filename.
  
  safe_name <- gsub(
    "[^A-Za-z0-9_-]",
    "_",
    nutrient
  )
  
  input_name <- paste0(
    safe_name,
    "_weighted_mean.tif"
  )
  
  input_path <- file.path(
    input_folder,
    input_name
  )
  
  
  if (!file.exists(input_path)) {
    
    stop(
      "Input raster does not exist: ",
      input_path
    )
    
  }
  
  
  # Output filename.
  
  output_name <- paste0(
    safe_name,
    "_percent_RDA.tif"
  )
  
  output_path <- file.path(
    percent_rda_folder,
    output_name
  )
  
  
  # Read the nutrient raster.
  
  cat("Reading raster...\n")
  
  r <- terra::rast(input_path)
  
  
  # Confirm raster is readable.
  
  if (nlyr(r) != 1) {
    
    stop(
      "Expected one raster layer for: ",
      nutrient
    )
    
  }
  
  
  # Calculate percentage of daily reference intake.
  #
  # Percentage = (weighted nutrient value / reference) * 100
  
  cat("Calculating percentage...\n")
  
  percent_raster <- r / reference_value * 100
  
  
  # Optional cap at 100%.
  
  if (cap_at_100) {
    
    percent_raster <- terra::clamp(
      percent_raster,
      lower = 0,
      upper = 100,
      values = TRUE
    )
    
  }
  
  
  # Set layer name.
  
  names(percent_raster) <- paste0(
    safe_name,
    "_percent_RDA"
  )
  
  
  # Write output directly.
  
  cat("Writing output raster...\n")
  
  terra::writeRaster(
    
    percent_raster,
    
    filename = output_path,
    
    overwrite = TRUE,
    
    wopt = list(
      datatype = "FLT4S",
      gdal = c(
        "COMPRESS=LZW",
        "TILED=YES"
      )
    )
    
  )
  
  
  # Release objects before loading the next raster.
  
  rm(r, percent_raster)
  
  gc(verbose = FALSE)
  
  
  cat("Successfully saved:", output_name, "\n")
  
}


# ------------------------------------------------------------
# 16.6 SAVE REFERENCE TABLE
# ------------------------------------------------------------

write.csv(
  
  rda_reference,
  
  file.path(
    percent_rda_folder,
    "daily_reference_values.csv"
  ),
  
  row.names = FALSE
  
)


# ============================================================
# 17. COMPLETION
# ============================================================

cat("\n========================================\n")
cat("PERCENTAGE OF DAILY INTAKE PROCESSING\n")
cat("========================================\n")

cat(
  "Rasters processed:",
  length(nutrients_to_process),
  "\n"
)

cat(
  "Output directory:",
  percent_rda_folder,
  "\n"
)

cat("========================================\n")


# ------------------------------------------------------------
# 18. CALCULATE OVERALL NUTRIENT DENSITY
# ------------------------------------------------------------

# Input and output folders
percent_rda_folder <- file.path(
  output_folder,
  "percent_RDA_rasters_capped"
)

overall_output_folder <- file.path(
  output_folder,
  "overall_nutrient_density"
)

dir.create(
  overall_output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

# List all 13 percentage-RDA rasters
raster_files <- list.files(
  percent_rda_folder,
  pattern = "\\.tif$",
  full.names = TRUE
)

# Check that all 13 nutrient rasters are available
if (length(raster_files) != 13) {
  stop("Expected 13 percentage-RDA rasters, but found ",
       length(raster_files))
}

# Load nutrient rasters
nutrient_stack <- rast(raster_files)

# Cap each nutrient at 100%
nutrient_stack <- clamp(
  nutrient_stack,
  lower = 0,
  upper = 100,
  values = TRUE
)

# Calculate arithmetic mean for each cell
overall_nutrient_density <- app(
  nutrient_stack,
  fun = mean,
  na.rm = FALSE
)

# Name output layer
names(overall_nutrient_density) <- "overall_nutrient_density"

# Save final raster
writeRaster(
  overall_nutrient_density,
  filename = file.path(
    overall_output_folder,
    "overall_nutrient_density.tif"
  ),
  overwrite = TRUE
)

# ------------------------------------------------------------
# 19. REPORT RESULTS
# ------------------------------------------------------------

print(global(
  overall_nutrient_density,
  fun = c("min", "max", "mean"),
  na.rm = TRUE
))

cat(
  "\nOverall nutrient density raster saved to:\n",
  overall_output_folder,
  "\n"
)




# ============================================================
# RED SEA FISH NUTRIENT-RICHNESS SIMULATIONS
#
# For every pixel:
#   1. Select 10 species using occurrence probabilities
#      as sampling weights.
#   2. Generate all 1,023 non-empty combinations.
#   3. Calculate mean nutrient concentration per 100 g.
#   4. Repeat for 1,000 iterations.
#   5. Save simulation results and graphs in that pixel's
#      individual folder.
#
# Outputs per pixel:
#   - 1 compressed CSV
#   - 13 nutrient boxplots
#   - 13 species-richness response curves
# ============================================================


# ============================================================
# 1. LOAD LIBRARIES
# ============================================================

library(terra)
library(dplyr)
library(ggplot2)


# ============================================================
# 2. SIMULATION SETTINGS
# ============================================================

set.seed(123)

n_iterations <- 1000
n_species_selected <- 10

# Set to NULL to process every pixel.
# For a test, change to 3 to process only the first 3 pixels.

pixels_to_process <- NULL


# ============================================================
# 3. OUTPUT FOLDER
# ============================================================

output_folder <- file.path(
  getwd(),
  "nutrient_richness_simulations"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 4. CHECK INPUT DATA
# ============================================================

# These objects must already exist:
#
# probability_matrix
# nutrient_matrix
# valid_pixel_ids

if (!exists("probability_matrix")) {
  stop("probability_matrix does not exist.")
}

if (!exists("nutrient_matrix")) {
  stop("nutrient_matrix does not exist.")
}

if (!exists("valid_pixel_ids")) {
  stop("valid_pixel_ids does not exist.")
}


# Convert to matrices
probability_matrix <- as.matrix(probability_matrix)
nutrient_matrix <- as.matrix(nutrient_matrix)

# Ensure nutrient matrix is numeric
storage.mode(nutrient_matrix) <- "double"

# Check pixel dimensions
if (length(valid_pixel_ids) != nrow(probability_matrix)) {
  stop("valid_pixel_ids does not match probability_matrix.")
}

# Check species dimensions
if (ncol(probability_matrix) != nrow(nutrient_matrix)) {
  stop("Species dimensions do not match between the two matrices.")
}

# If species names are available, ensure the rows of the
# nutrient matrix follow the same order as probability columns.

if (
  !is.null(colnames(probability_matrix)) &&
  !is.null(rownames(nutrient_matrix))
) {
  
  if (!setequal(
    colnames(probability_matrix),
    rownames(nutrient_matrix)
  )) {
    stop("Species names do not match between the two matrices.")
  }
  
  nutrient_matrix <- nutrient_matrix[
    colnames(probability_matrix),
    ,
    drop = FALSE
  ]
}

# Check nutrients
if (ncol(nutrient_matrix) != 13) {
  stop("Expected exactly 13 nutrient columns.")
}

if (is.null(colnames(nutrient_matrix))) {
  stop("nutrient_matrix must have nutrient column names.")
}

nutrient_names <- colnames(nutrient_matrix)

cat("\n")
cat("====================================================\n")
cat("INPUT DATA CHECK\n")
cat("====================================================\n")

cat("Number of pixels:", nrow(probability_matrix), "\n")
cat("Number of species:", ncol(probability_matrix), "\n")
cat("Number of nutrients:", ncol(nutrient_matrix), "\n")
cat("Iterations per pixel:", n_iterations, "\n")


# ============================================================
# 5. SELECT PIXELS TO PROCESS
# ============================================================

if (is.null(pixels_to_process)) {
  
  pixels_to_process <- seq_len(
    nrow(probability_matrix)
  )
  
} else {
  
  pixels_to_process <- seq_len(
    min(pixels_to_process, nrow(probability_matrix))
  )
}

cat(
  "Pixels selected for processing:",
  length(pixels_to_process),
  "\n"
)



# ============================================================
# 6. PRE-CALCULATE ALL POSSIBLE COMBINATIONS
# ============================================================

# Each combination matrix will have 10 columns,
# one for each selected species.
#
# 1 = species is included in the combination
# 0 = species is not included.
#
# This allows matrix multiplication:
#
# membership (number of combinations x 10)
# selected_nutrients (10 x 13)
#
# Result:
# number of combinations x 13 nutrients


combination_matrices <- lapply(
  1:n_species_selected,
  function(k) {
    
    # Generate combinations of species indices
    combination_indices <- utils::combn(
      n_species_selected,
      k
    )
    
    # Create membership matrix
    membership <- matrix(
      0,
      nrow = ncol(combination_indices),
      ncol = n_species_selected
    )
    
    # Mark included species with 1
    for (i in seq_len(ncol(combination_indices))) {
      
      membership[
        i,
        combination_indices[, i]
      ] <- 1
    }
    
    membership
  }
)


# Number of combinations at each richness level
combination_counts <- choose(
  n_species_selected,
  1:n_species_selected
)

# Total combinations = 1,023
total_combinations <- sum(
  combination_counts
)

# Richness level for every combination
combination_richness <- rep(
  1:n_species_selected,
  times = combination_counts
)

# Combination numbering
combination_number <- seq_len(
  total_combinations
)


# Check dimensions
for (k in 1:n_species_selected) {
  
  cat(
    "Richness:", k,
    "| Matrix dimensions:",
    dim(combination_matrices[[k]]),
    "\n"
  )
}

cat(
  "\nTotal combinations per iteration:",
  total_combinations,
  "\n"
)



# ============================================================
# 7. PREPARE FINAL SUMMARY TABLE
# ============================================================

pixel_summary <- data.frame(
  pixel_number = integer(),
  original_raster_cell = integer(),
  status = character(),
  output_file = character(),
  processing_minutes = numeric(),
  stringsAsFactors = FALSE
)


# ============================================================
# 8. LOOP THROUGH PIXELS
# ============================================================

for (pixel in pixels_to_process) {
  
  start_time <- Sys.time()
  
  # Original raster cell identifier
  original_cell <- valid_pixel_ids[pixel]
  
  # Sequential pixel numbering
  pixel_id <- sprintf(
    "pixel_%03d",
    pixel
  )
  
  
  # ==========================================================
  # 8A. CREATE INDIVIDUAL PIXEL FOLDERS
  # ==========================================================
  
  pixel_folder <- file.path(
    output_folder,
    pixel_id
  )
  
  results_folder <- file.path(
    pixel_folder,
    "pixel_simulation_results"
  )
  
  boxplot_folder <- file.path(
    pixel_folder,
    "boxplots"
  )
  
  response_folder <- file.path(
    pixel_folder,
    "species_richness_response_curves"
  )
  
  dir.create(
    results_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    boxplot_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    response_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  # ==========================================================
  # 8B. PIXEL INFORMATION
  # ==========================================================
  
  cat("\n")
  cat("====================================================\n")
  cat("PROCESSING:", pixel_id, "\n")
  cat("Original raster cell:", original_cell, "\n")
  cat("====================================================\n")
  
  
  # ==========================================================
  # 8C. GET OCCURRENCE PROBABILITIES
  # ==========================================================
  
  pixel_probabilities <- as.numeric(
    probability_matrix[pixel, ]
  )
  
  # Only species with positive, finite probabilities
  eligible_species <- which(
    is.finite(pixel_probabilities) &
      pixel_probabilities > 0
  )
  
  cat(
    "Species with positive occurrence probability:",
    length(eligible_species),
    "\n"
  )
  
  # Skip pixels with fewer than 10 eligible species
  if (length(eligible_species) < n_species_selected) {
    
    cat("SKIPPED: Fewer than 10 eligible species.\n")
    
    pixel_summary <- rbind(
      pixel_summary,
      data.frame(
        pixel_number = pixel,
        original_raster_cell = original_cell,
        status = "Skipped - fewer than 10 eligible species",
        output_file = NA_character_,
        processing_minutes = NA_real_
      )
    )
    
    next
  }
  
  
  # ==========================================================
  # 8D. CREATE RESULTS ARRAY
  # ==========================================================
  
  # Dimensions:
  #
  # 1,023 combinations
  # 13 nutrients
  # 1,000 iterations
  #
  # Approximately 106 MB of numeric data.
  
  results_array <- array(
    NA_real_,
    dim = c(
      total_combinations,
      ncol(nutrient_matrix),
      n_iterations
    ),
    dimnames = list(
      NULL,
      nutrient_names,
      NULL
    )
  )
  
  
  # ==========================================================
  # 8E. CREATE COMPRESSED CSV
  # ==========================================================
  
  output_file <- file.path(
    results_folder,
    paste0(
      pixel_id,
      "_simulation_results.csv.gz"
    )
  )
  
  output_connection <- gzfile(
    output_file,
    open = "wt"
  )
  
  on.exit(close(output_connection), add = TRUE)

  
  
  # ==========================================================
  # 9. RUN 1,000 ITERATIONS
  # ==========================================================
  
  for (iteration in seq_len(n_iterations)) {
    
    
    # --------------------------------------------------------
    # STEP 1. SELECT 10 SPECIES
    # --------------------------------------------------------
    
    # Selection is weighted by occurrence probability.
    #
    # Sampling is without replacement.
    #
    # Each iteration selects a new group of 10 species.
    
    selected_species <- sample(
      eligible_species,
      size = n_species_selected,
      replace = FALSE,
      prob = pixel_probabilities[eligible_species]
    )
    
    
    # --------------------------------------------------------
    # STEP 2. GET NUTRIENT VALUES
    # --------------------------------------------------------
    
    selected_nutrients <- nutrient_matrix[
      selected_species,
      ,
      drop = FALSE
    ]
    
    # Identify missing and non-finite values
    missing_indicator <- !is.finite(
      selected_nutrients
    )
    
    # Replace missing values with zero for summation.
    # They will not be included in the nutrient denominator.
    
    selected_nutrients[missing_indicator] <- 0
    
    
    # --------------------------------------------------------
    # STEP 3. CALCULATE ALL 1,023 COMBINATIONS
    # --------------------------------------------------------
    
    iteration_results <- matrix(
      NA_real_,
      nrow = total_combinations,
      ncol = ncol(nutrient_matrix)
    )
    
    colnames(iteration_results) <- nutrient_names
    
    combination_start <- 1
    
    for (k in 1:n_species_selected) {
      
      membership <- combination_matrices[[k]]
      
      combination_end <- (
        combination_start +
          nrow(membership) - 1
      )
      
      # Sum nutrient values for every combination
      nutrient_sums <- membership %*%
        selected_nutrients
      
      # Count available nutrient observations
      nutrient_counts <- membership %*%
        (!missing_indicator * 1)
      
      # Arithmetic mean:
      # each fish contributes equally.
      
      combination_means <- nutrient_sums /
        nutrient_counts
      
      # Assign NA if no species in a combination
      # has an available value for that nutrient.
      
      combination_means[
        nutrient_counts == 0
      ] <- NA_real_
      
      # Store results
      iteration_results[
        combination_start:combination_end,
      ] <- combination_means
      
      combination_start <- combination_end + 1
    }
    
    
    # --------------------------------------------------------
    # STEP 4. STORE RESULTS IN ARRAY
    # --------------------------------------------------------
    
    results_array[, , iteration] <- iteration_results
    
    
    # --------------------------------------------------------
    # STEP 5. WRITE ITERATION RESULTS TO CSV
    # --------------------------------------------------------
    
    iteration_output <- data.frame(
      iteration = iteration,
      species_richness = combination_richness,
      combination = combination_number,
      iteration_results,
      check.names = FALSE
    )
    
    write.table(
      iteration_output,
      file = output_connection,
      sep = ",",
      row.names = FALSE,
      col.names = (iteration == 1),
      na = ""
    )
    
    
    # --------------------------------------------------------
    # STEP 6. PROGRESS
    # --------------------------------------------------------
    
    if (iteration %% 100 == 0) {
      
      cat(
        pixel_id,
        "- completed iteration",
        iteration,
        "of",
        n_iterations,
        "\n"
      )
    }
  }
  
  
  # Close compressed CSV
  close(output_connection)
  
  
  # ==========================================================
  # 10. GENERATE 13 BOXPLOTS
  # ==========================================================
  
  cat("\nGenerating boxplots for", pixel_id, "...\n")
  
  for (nutrient_index in seq_along(nutrient_names)) {
    
    nutrient_name <- nutrient_names[nutrient_index]
    
    # Extract values for this nutrient
    nutrient_values <- results_array[
      ,
      nutrient_index,
      ,
      drop = FALSE
    ]
    
    # Prepare plotting data
    boxplot_data <- data.frame(
      species_richness = factor(
        rep(
          combination_richness,
          times = n_iterations
        ),
        levels = 1:10
      ),
      nutrient_value = as.vector(
        nutrient_values
      )
    )
    
    # Remove missing values
    boxplot_data <- boxplot_data[
      is.finite(boxplot_data$nutrient_value),
    ]
    
    # Create boxplot
    p_box <- ggplot(
      boxplot_data,
      aes(
        x = species_richness,
        y = nutrient_value
      )
    ) +
      
      geom_boxplot(
        fill = "grey80",
        colour = "black",
        outlier.shape = NA
      ) +
      
      labs(
        title = paste(
          nutrient_name,
          "-",
          pixel_id
        ),
        subtitle = paste(
          "Nutrient concentration across all combinations"
        ),
        x = "Species richness",
        y = paste(
          nutrient_name,
          "per 100 g"
        )
      ) +
      
      theme_classic() +
      
      theme(
        plot.title = element_text(
          face = "bold",
          size = 13
        )
      )
    
    
    # Safe filename
    nutrient_file_name <- gsub(
      "[^A-Za-z0-9_-]",
      "_",
      nutrient_name
    )
    
    # Save boxplot inside this pixel's folder
    ggsave(
      filename = file.path(
        boxplot_folder,
        paste0(
          pixel_id,
          "_",
          nutrient_file_name,
          "_boxplot.png"
        )
      ),
      plot = p_box,
      width = 8,
      height = 6,
      dpi = 300
    )
    
    rm(
      nutrient_values,
      boxplot_data,
      p_box
    )
  }
  
  
  # ==========================================================
  # 11. GENERATE 13 SPECIES-RICHNESS RESPONSE CURVES
  # ==========================================================
  
  cat(
    "\nGenerating response curves for",
    pixel_id,
    "...\n"
  )
  
  
  for (nutrient_index in seq_along(nutrient_names)) {
    
    nutrient_name <- nutrient_names[nutrient_index]
    
    
    # --------------------------------------------------------
    # STEP 1. CALCULATE ITERATION-LEVEL MEANS
    # --------------------------------------------------------
    
    # Rows = richness levels 1 to 10
    # Columns = iterations 1 to 1,000
    
    iteration_means <- matrix(
      NA_real_,
      nrow = n_species_selected,
      ncol = n_iterations
    )
    
    
    for (k in 1:n_species_selected) {
      
      # Identify combinations with richness k
      richness_rows <- which(
        combination_richness == k
      )
      
      # Extract values for those combinations
      richness_values <- results_array[
        richness_rows,
        nutrient_index,
        ,
        drop = FALSE
      ]
      
      # Convert to matrix:
      # rows = combinations
      # columns = iterations
      
      richness_matrix <- matrix(
        richness_values,
        nrow = length(richness_rows),
        ncol = n_iterations
      )
      
      # Average all combinations within each iteration.
      #
      # Each iteration contributes ONE mean value
      # for each richness level.
      
      iteration_means[k, ] <- colMeans(
        richness_matrix,
        na.rm = TRUE
      )
    }
    
    
    # --------------------------------------------------------
    # STEP 2. CALCULATE MEAN AND 95% CONFIDENCE INTERVAL
    # --------------------------------------------------------
    
    response_summary <- data.frame(
      species_richness = 1:n_species_selected,
      mean = rep(NA_real_, n_species_selected),
      lower_ci = rep(NA_real_, n_species_selected),
      upper_ci = rep(NA_real_, n_species_selected),
      n_iterations = rep(0L, n_species_selected)
    )
    
    for (k in 1:n_species_selected) {
      
      values <- iteration_means[k, ]
      
      # Remove missing and non-finite values
      values <- values[
        is.finite(values)
      ]
      
      n_valid <- length(values)
      
      response_summary$n_iterations[k] <- n_valid
      
      if (n_valid > 1) {
        
        mean_value <- mean(values)
        
        standard_error <- sd(values) /
          sqrt(n_valid)
        
        critical_value <- qt(
          0.975,
          df = n_valid - 1
        )
        
        response_summary$mean[k] <- mean_value
        
        response_summary$lower_ci[k] <-
          mean_value -
          critical_value * standard_error
        
        response_summary$upper_ci[k] <-
          mean_value +
          critical_value * standard_error
      }
    }
    
    
    # --------------------------------------------------------
    # STEP 3. CREATE RESPONSE CURVE
    # --------------------------------------------------------
    
    p_response <- ggplot(
      response_summary,
      aes(
        x = species_richness,
        y = mean
      )
    ) +
      
      # 95% confidence interval
      geom_ribbon(
        aes(
          ymin = lower_ci,
          ymax = upper_ci
        ),
        fill = "grey70",
        alpha = 0.5
      ) +
      
      # Mean response line
      geom_line(
        colour = "black",
        linewidth = 0.8
      ) +
      
      # Mean response points
      geom_point(
        colour = "black",
        size = 2
      ) +
      
      scale_x_continuous(
        breaks = 1:10,
        limits = c(1, 10)
      ) +
      
      labs(
        title = paste(
          nutrient_name,
          "-",
          pixel_id
        ),
        subtitle = paste(
          "Mean nutrient concentration with 95% confidence interval"
        ),
        x = "Species richness",
        y = paste(
          nutrient_name,
          "per 100 g"
        )
      ) +
      
      theme_classic() +
      
      theme(
        plot.title = element_text(
          face = "bold",
          size = 13
        )
      )
    
    
    # --------------------------------------------------------
    # STEP 4. SAVE RESPONSE CURVE
    # --------------------------------------------------------
    
    nutrient_file_name <- gsub(
      "[^A-Za-z0-9_-]",
      "_",
      nutrient_name
    )
    
    ggsave(
      filename = file.path(
        response_folder,
        paste0(
          pixel_id,
          "_",
          nutrient_file_name,
          "_response_curve.png"
        )
      ),
      plot = p_response,
      width = 8,
      height = 6,
      dpi = 300
    )
    
    
    # Release nutrient-specific objects
    rm(
      iteration_means,
      response_summary,
      p_response
    )
  }
  
  
  # ==========================================================
  # 12. SAVE PIXEL SUMMARY
  # ==========================================================
  
  processing_minutes <- as.numeric(
    difftime(
      Sys.time(),
      start_time,
      units = "mins"
    )
  )
  
  pixel_summary <- rbind(
    pixel_summary,
    data.frame(
      pixel_number = pixel,
      original_raster_cell = original_cell,
      status = "Completed",
      output_file = output_file,
      processing_minutes = processing_minutes
    )
  )
  
  
  # ==========================================================
  # 13. PIXEL COMPLETED
  # ==========================================================
  
  cat("\n")
  cat("----------------------------------------------------\n")
  cat("COMPLETED:", pixel_id, "\n")
  cat(
    "Processing time:",
    round(processing_minutes, 2),
    "minutes\n"
  )
  cat("Output folder:", pixel_folder, "\n")
  cat("----------------------------------------------------\n")
  
  
  # ==========================================================
  # 14. RELEASE MEMORY
  # ==========================================================
  
  rm(results_array)
  
  gc()
}


# ============================================================
# 15. SAVE FINAL SUMMARY
# ============================================================

summary_file <- file.path(
  output_folder,
  "simulation_summary.csv"
)

write.csv(
  pixel_summary,
  summary_file,
  row.names = FALSE
)


# ============================================================
# 16. FINAL REPORT
# ============================================================

completed_pixels <- sum(
  pixel_summary$status == "Completed"
)

skipped_pixels <- sum(
  pixel_summary$status != "Completed"
)

cat("\n")
cat("====================================================\n")
cat("ALL SIMULATIONS FINISHED\n")
cat("====================================================\n")

cat("Completed pixels:", completed_pixels, "\n")
cat("Skipped pixels:", skipped_pixels, "\n")

cat(
  "Total boxplots:",
  completed_pixels * 13,
  "\n"
)

cat(
  "Total response curves:",
  completed_pixels * 13,
  "\n"
)

cat(
  "Total graph files:",
  completed_pixels * 26,
  "\n"
)

cat("\nMain output folder:\n")
cat(output_folder, "\n")

cat("\nSummary file:\n")
cat(summary_file, "\n")

cat("\n====================================================\n")
