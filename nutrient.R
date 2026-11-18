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
# NEW SIMULATION METHODOLOGY
#
# For each pixel:
#   1. Select 10 species using occurrence probabilities
#   2. Generate all 1,023 non-empty combinations
#   3. Calculate mean values for all 13 nutrients
#   4. Repeat 1,000 times
#   5. Save numerical results
#   6. Create 13 nutrient graphs
# ============================================================


# ------------------------------------------------------------
# 1. SETTINGS
# ------------------------------------------------------------

set.seed(123)

n_iterations <- 1000
n_species_selected <- 10

nutrient_names <- colnames(nutrient_matrix)

n_nutrients <- ncol(nutrient_matrix)
n_pixels <- nrow(probability_matrix)
n_species <- ncol(probability_matrix)

# Check that the nutrient data and probability matrix
# contain the same number of species.

if (nrow(nutrient_matrix) != n_species) {
  stop(
    "ERROR: nutrient_matrix rows do not match ",
    "probability_matrix columns."
  )
}

if (length(valid_pixel_ids) != n_pixels) {
  stop(
    "ERROR: valid_pixel_ids does not match ",
    "the number of rows in probability_matrix."
  )
}

if (n_nutrients != 13) {
  stop("ERROR: Expected 13 nutrient columns.")
}

if (any(!is.finite(nutrient_matrix))) {
  stop("ERROR: nutrient_matrix contains NA or non-finite values.")
}


# ------------------------------------------------------------
# 2. PRE-CALCULATE ALL COMBINATION STRUCTURES
# ------------------------------------------------------------

# For 10 selected species, calculate the number of
# possible combinations for each group size.

combination_counts <- choose(
  n_species_selected,
  1:n_species_selected
)

total_combinations <- sum(combination_counts)

cat(
  "\nTotal combinations per iteration:",
  total_combinations,
  "\n"
)

# Create a membership matrix for each group size.
#
# Each column represents one combination.
# Each row represents one of the 10 selected species.
#
# A value of 1 means that species is included.

combination_matrices <- vector(
  "list",
  n_species_selected
)

for (k in 1:n_species_selected) {
  
  combinations <- utils::combn(
    n_species_selected,
    k
  )
  
  membership <- matrix(
    0,
    nrow = n_species_selected,
    ncol = ncol(combinations)
  )
  
  for (j in seq_len(ncol(combinations))) {
    
    membership[combinations[, j], j] <- 1
    
  }
  
  combination_matrices[[k]] <- membership
  
}


# ------------------------------------------------------------
# 3. COMBINATION OUTPUT INDEXING
# ------------------------------------------------------------

# Each iteration produces 1,023 results.
# The results are ordered from richness 1 to richness 10.

combination_start <- c(
  1,
  head(
    cumsum(combination_counts),
    -1
  ) + 1
)

combination_end <- cumsum(combination_counts)

# Richness label for every combination.

combination_richness <- rep(
  1:n_species_selected,
  times = combination_counts
)

# Combination number within each richness level.

combination_number <- unlist(
  lapply(
    combination_counts,
    seq_len
  ),
  use.names = FALSE
)


# ------------------------------------------------------------
# 4. PIXEL SUMMARY STORAGE
# ------------------------------------------------------------

pixel_summary <- vector(
  "list",
  n_pixels
)


# ------------------------------------------------------------
# 5. MAIN PIXEL LOOP
# ------------------------------------------------------------

for (pixel in seq_len(n_pixels)) {
  
  pixel_start_time <- Sys.time()
  
  original_cell <- valid_pixel_ids[pixel]
  
  pixel_id <- sprintf(
    "pixel_%06d",
    original_cell
  )
  
  cat(
    "\n\n====================================\n"
  )
  
  cat(
    "Processing pixel",
    pixel,
    "of",
    n_pixels,
    "|",
    pixel_id,
    "\n"
  )
  
  cat(
    "====================================\n"
  )
  
  
  # ----------------------------------------------------------
  # 5A. RETRIEVE PROBABILITIES FOR THIS PIXEL
  # ----------------------------------------------------------
  
  # This retrieves values from the existing matrix.
  # It does NOT read the raster files again.
  
  pixel_probabilities <- as.numeric(
    probability_matrix[pixel, ]
  )
  
  # Identify species with positive occurrence probability.
  
  eligible_species <- which(
    is.finite(pixel_probabilities) &
      pixel_probabilities > 0
  )
  
  n_eligible <- length(eligible_species)
  
  cat(
    "Eligible species:",
    n_eligible,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # 5B. CHECK WHETHER 10 SPECIES CAN BE SELECTED
  # ----------------------------------------------------------
  
  if (n_eligible < n_species_selected) {
    
    cat(
      "Skipping pixel:",
      "fewer than 10 eligible species.\n"
    )
    
    pixel_summary[[pixel]] <- data.frame(
      pixel_id = pixel_id,
      original_raster_cell = original_cell,
      eligible_species = n_eligible,
      status = "Skipped: fewer than 10 species",
      output_file = NA_character_,
      processing_minutes = NA_real_
    )
    
    next
    
  }
  
  
  # ----------------------------------------------------------
  # 5C. PREPARE NUMERICAL RESULT STORAGE
  # ----------------------------------------------------------
  
  # Dimensions:
  #
  # 1,023 combinations
  # 13 nutrients
  # 1,000 iterations
  #
  # This allocates the storage once, rather than creating
  # and combining a data frame for every simulation.
  
  results_array <- array(
    NA_real_,
    dim = c(
      total_combinations,
      n_nutrients,
      n_iterations
    )
  )
  
  
  # ----------------------------------------------------------
  # 5D. PREPARE OUTPUT FILE
  # ----------------------------------------------------------
  
  csv_file <- file.path(
    results_folder,
    paste0(pixel_id, "_simulation_results.csv.gz")
  )
  
  output_connection <- gzfile(
    csv_file,
    open = "wt"
  )
  
  
  # ----------------------------------------------------------
  # 5E. REPEAT 1,000 SELECTIONS
  # ----------------------------------------------------------
  
  for (iteration in seq_len(n_iterations)) {
    
    # --------------------------------------------------------
    # STEP 1: SELECT 10 DIFFERENT SPECIES
    # --------------------------------------------------------
    
    # Sampling is weighted by occurrence probability.
    # Sampling is without replacement.
    
    selected_species <- sample(
      eligible_species,
      size = n_species_selected,
      replace = FALSE,
      prob = pixel_probabilities[eligible_species]
    )
    
    
    # --------------------------------------------------------
    # STEP 2: RETRIEVE THEIR 13 NUTRIENT VALUES
    # --------------------------------------------------------
    
    selected_nutrients <- nutrient_matrix[
      selected_species,
      ,
      drop = FALSE
    ]
    
    
    # --------------------------------------------------------
    # STEP 3: CALCULATE ALL 1,023 COMBINATIONS
    # --------------------------------------------------------
    
    for (k in 1:n_species_selected) {
      
      # The membership matrix identifies which of the
      # 10 selected species belong to each combination.
      
      membership <- combination_matrices[[k]]
      
      # Matrix multiplication calculates the sum of each
      # nutrient across every combination.
      #
      # Dividing by k gives the arithmetic mean.
      #
      # Each fish contributes equally.
      
      combination_means <- (
        t(membership) %*% selected_nutrients
      ) / k
      
      # Store the results in the preallocated array.
      
      result_rows <- combination_start[k]:
        combination_end[k]
      
      results_array[
        result_rows,
        ,
        iteration
      ] <- combination_means
      
    }
    
    
    # --------------------------------------------------------
    # STEP 4: SAVE THIS ITERATION'S NUMERICAL RESULTS
    # --------------------------------------------------------
    
    iteration_results <- data.frame(
      iteration = iteration,
      species_richness = combination_richness,
      combination = combination_number,
      as.data.frame(
        results_array[, , iteration, drop = FALSE][, , 1]
      ),
      check.names = FALSE
    )
    
    names(iteration_results)[4:(
      3 + n_nutrients
    )] <- nutrient_names
    
    write.table(
      iteration_results,
      file = output_connection,
      sep = ",",
      row.names = FALSE,
      col.names = (iteration == 1),
      qmethod = "double"
    )
    
    
    # --------------------------------------------------------
    # STEP 5: REPORT PROGRESS
    # --------------------------------------------------------
    
    if (
      iteration %% 100 == 0 ||
      iteration == n_iterations
    ) {
      
      cat(
        "\rPixel:",
        pixel,
        "| Iteration:",
        iteration,
        "/",
        n_iterations
      )
      
      flush.console()
      
    }
    
  }
  
  
  # Close the compressed CSV file.
  
  close(output_connection)
  
  cat("\nNumerical results saved.\n")
  
  
  # ----------------------------------------------------------
  # 5F. CREATE 13 NUTRIENT GRAPHS
  # ----------------------------------------------------------
  
  cat("Creating nutrient graphs...\n")
  
  for (nutrient_index in seq_len(n_nutrients)) {
    
    nutrient_name <- nutrient_names[nutrient_index]
    
    # Extract all 1,000 iterations for each richness level.
    
    plot_data <- lapply(
      1:n_species_selected,
      function(k) {
        
        result_rows <- combination_start[k]:
          combination_end[k]
        
        as.vector(
          results_array[
            result_rows,
            nutrient_index,
            ,
            drop = FALSE
          ]
        )
        
      }
    )
    
    # Create a safe filename.
    
    safe_nutrient_name <- gsub(
      "[^A-Za-z0-9_-]+",
      "_",
      nutrient_name
    )
    
    graph_file <- file.path(
      plot_folder,
      paste0(
        pixel_id,
        "_",
        safe_nutrient_name,
        ".png"
      )
    )
    
    png(
      filename = graph_file,
      width = 1600,
      height = 1000,
      res = 150
    )
    
    boxplot(
      plot_data,
      names = 1:n_species_selected,
      main = paste(
        pixel_id,
        "-",
        nutrient_name
      ),
      xlab = "Number of species in combination",
      ylab = paste(
        "Mean",
        nutrient_name,
        "per 100 g"
      ),
      col = "lightblue",
      border = "grey30",
      outline = FALSE
    )
    
    dev.off()
    
  }
  
  
  # ----------------------------------------------------------
  # 5G. RECORD PIXEL COMPLETION
  # ----------------------------------------------------------
  
  processing_time <- as.numeric(
    difftime(
      Sys.time(),
      pixel_start_time,
      units = "mins"
    )
  )
  
  pixel_summary[[pixel]] <- data.frame(
    pixel_id = pixel_id,
    original_raster_cell = original_cell,
    eligible_species = n_eligible,
    status = "Completed",
    output_file = basename(csv_file),
    processing_minutes = processing_time
  )
  
  cat(
    "\nCompleted:",
    pixel_id,
    "\nTime:",
    round(processing_time, 2),
    "minutes\n"
  )
  
  # Release memory before moving to the next pixel.
  
  rm(results_array)
  gc()
  
}


# ------------------------------------------------------------
# 6. SAVE OVERALL SUMMARY
# ------------------------------------------------------------

pixel_summary_df <- dplyr::bind_rows(
  pixel_summary
)

summary_file <- file.path(
  output_folder,
  "simulation_summary.csv"
)

write.csv(
  pixel_summary_df,
  summary_file,
  row.names = FALSE
)


# ------------------------------------------------------------
# 7. FINAL REPORT
# ------------------------------------------------------------

cat("\n\n====================================\n")
cat("SIMULATION WORKFLOW FINISHED\n")
cat("====================================\n")

cat(
  "Pixels processed:",
  sum(pixel_summary_df$status == "Completed"),
  "\n"
)

cat(
  "Pixels skipped:",
  sum(pixel_summary_df$status != "Completed"),
  "\n"
)

cat(
  "Total combinations per selection:",
  total_combinations,
  "\n"
)

cat(
  "Iterations per pixel:",
  n_iterations,
  "\n"
)

cat(
  "Summary saved to:",
  summary_file,
  "\n"
)
