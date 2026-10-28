
#processes all species rasters, 
#classifies probabilities at the 0.5 threshold, 
#generates the outputs, replaces missing values with 0, 
#and removes grid cells with fewer than 20 species present.

library(terra)
library(dplyr)
library(tidyr)
library(readr)

# --------------------------------------------------
# 1. Paths and settings
# --------------------------------------------------

input_folder <- "C:/Users/nasirh/OneDrive - KAUST/repositories/redseaecosystem/red_sea_species_rasters"

grid_file <- file.path(
  input_folder,
  "red_sea_species_grid.gpkg"
)

output_folder <- file.path(
  input_folder,
  "species_presence_analysis"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

threshold <- 0.5
minimum_richness <- 20

# --------------------------------------------------
# 2. Load grid and create cell-centre points
# --------------------------------------------------

grid <- vect(grid_file)

stopifnot(
  "grid_label" %in% names(grid),
  "cell_id" %in% names(grid)
)

grid_info <- as.data.frame(grid)[, c(
  "cell_id",
  "grid_label"
)]

grid_points <- centroids(grid)

# --------------------------------------------------
# 3. Find all species rasters
# --------------------------------------------------

raster_files <- list.files(
  input_folder,
  pattern = "\\.(tif|tiff)$",
  full.names = TRUE,
  ignore.case = TRUE
)

raster_files <- raster_files[
  !grepl(
    "red_sea_species_grid",
    basename(raster_files),
    ignore.case = TRUE
  )
]

if (length(raster_files) == 0) {
  stop("No species TIFF rasters found.")
}

cat("Number of rasters found:", length(raster_files), "\n")
cat("Number of grid cells:", nrow(grid_info), "\n")

# --------------------------------------------------
# 4. Initialise probability and presence matrices
# --------------------------------------------------

species_names <- make.unique(
  tools::file_path_sans_ext(basename(raster_files))
)

n_cells <- nrow(grid_info)
n_species <- length(raster_files)

presence_matrix <- matrix(
  NA_integer_,
  nrow = n_cells,
  ncol = n_species,
  dimnames = list(NULL, species_names)
)

probability_matrix <- matrix(
  NA_real_,
  nrow = n_cells,
  ncol = n_species,
  dimnames = list(NULL, species_names)
)

# Track successfully processed rasters
processed <- rep(FALSE, n_species)

# --------------------------------------------------
# 5. Extract probabilities and classify species
# --------------------------------------------------

for (i in seq_along(raster_files)) {
  
  file <- raster_files[i]
  species_name <- species_names[i]
  
  cat(
    "\nProcessing", i, "of", n_species,
    ":", species_name, "\n"
  )
  
  tryCatch({
    
    r <- rast(file)
    
    if (nlyr(r) != 1) {
      stop("Expected a single-layer species raster.")
    }
    
    if (!same.crs(r, grid_points)) {
      stop("Raster CRS differs from the grid CRS.")
    }
    
    extracted <- terra::extract(
      r,
      grid_points
    )
    
    probability <- extracted[[2]]
    
    # Store original probability values
    probability_matrix[, i] <- probability
    
    # >= 0.5 = present (1)
    # <  0.5 = absent (0)
    # NA     = missing for now
    presence_matrix[, i] <- ifelse(
      is.na(probability),
      NA_integer_,
      as.integer(probability >= threshold)
    )
    
    processed[i] <- TRUE
    
  }, error = function(e) {
    
    cat(
      "ERROR:", species_name,
      "-", conditionMessage(e), "\n"
    )
    
  })
}

# Stop rather than silently treating failed rasters as absent
if (!all(processed)) {
  
  failed_species <- species_names[!processed]
  
  cat("\nFailed rasters:\n")
  print(failed_species)
  
  stop(
    "Some rasters failed. Fix the errors and rerun ",
    "before producing the final filtered matrix."
  )
}

# --------------------------------------------------
# 6. Create and save the full presence/absence matrix
# --------------------------------------------------

presence_df <- data.frame(
  grid_info,
  as.data.frame(presence_matrix),
  check.names = FALSE
)

write_csv(
  presence_df,
  file.path(
    output_folder,
    "species_presence_absence_matrix.csv"
  ),
  na = ""
)

# --------------------------------------------------
# 7. Save the full probability matrix
# --------------------------------------------------

probability_df <- data.frame(
  grid_info,
  as.data.frame(probability_matrix),
  check.names = FALSE
)

write_csv(
  probability_df,
  file.path(
    output_folder,
    "species_probabilities_by_grid.csv"
  ),
  na = ""
)

# --------------------------------------------------
# 8. Save the list of species classified as present
# --------------------------------------------------

present_long <- as.data.frame(
  presence_matrix,
  check.names = FALSE
) %>%
  mutate(
    cell_id = grid_info$cell_id,
    grid_label = grid_info$grid_label
  ) %>%
  pivot_longer(
    cols = all_of(species_names),
    names_to = "species",
    values_to = "present"
  ) %>%
  filter(present == 1L) %>%
  left_join(
    probability_df %>%
      pivot_longer(
        cols = all_of(species_names),
        names_to = "species",
        values_to = "probability"
      ),
    by = c("cell_id", "grid_label", "species")
  ) %>%
  select(
    grid_label,
    cell_id,
    species,
    probability
  ) %>%
  arrange(grid_label, species)

write_csv(
  present_long,
  file.path(
    output_folder,
    "species_present_by_grid.csv"
  )
)

# --------------------------------------------------
# 9. Calculate richness before filling missing values
# --------------------------------------------------

richness <- data.frame(
  grid_info,
  species_richness = rowSums(
    presence_matrix == 1L,
    na.rm = TRUE
  ),
  species_with_data = rowSums(
    !is.na(presence_matrix)
  )
)

write_csv(
  richness,
  file.path(
    output_folder,
    "species_richness_by_grid.csv"
  )
)

# --------------------------------------------------
# 10. Replace missing values with 0
# --------------------------------------------------

presence_filled <- presence_matrix

presence_filled[is.na(presence_filled)] <- 0L

# --------------------------------------------------
# 11. Filter cells with fewer than 20 species
# --------------------------------------------------

# Use richness calculated from observed probabilities
# before replacing missing values with zero.
keep_cells <- richness$species_richness >= minimum_richness

cat("\n-----------------------------\n")
cat("FILTERING SUMMARY\n")
cat("-----------------------------\n")

cat("Grid cells before filtering:", n_cells, "\n")

cat(
  "Cells removed (<", minimum_richness,
  " species): ",
  sum(!keep_cells), "\n",
  sep = ""
)

cat("Cells retained:", sum(keep_cells), "\n")

filtered_matrix <- data.frame(
  grid_info[keep_cells, , drop = FALSE],
  as.data.frame(
    presence_filled[keep_cells, , drop = FALSE]
  ),
  check.names = FALSE
)

# --------------------------------------------------
# 12. Save final filtered matrix
# --------------------------------------------------

write_csv(
  filtered_matrix,
  file.path(
    output_folder,
    "species_presence_absence_matrix_filtered.csv"
  )
)

# Save richness information for retained cells
write_csv(
  richness[keep_cells, ],
  file.path(
    output_folder,
    "species_richness_by_grid_filtered.csv"
  )
)

# --------------------------------------------------
# 13. Final summary
# --------------------------------------------------

cat("\n=============================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================\n")

cat("Rasters processed:", sum(processed), "\n")
cat("Original grid cells:", n_cells, "\n")
cat("Retained grid cells:", sum(keep_cells), "\n")
cat(
  "Species columns:",
  ncol(filtered_matrix) - length(c("cell_id", "grid_label")),
  "\n"
)

cat("\nOutputs saved in:\n", output_folder, "\n")

cat("\nFirst rows of filtered matrix:\n")
print(head(filtered_matrix))

cat("\nFirst rows of retained richness:\n")
print(head(richness[keep_cells, ]))



# --------------------------------------------------
# Remove species absent from all retained grid cells
# --------------------------------------------------

# Identify species columns with at least one presence
species_occupancy <- colSums(
  filtered_matrix[, species_names, drop = FALSE],
  na.rm = TRUE
)

species_to_keep <- names(species_occupancy)[
  species_occupancy > 0
]

species_to_remove <- names(species_occupancy)[
  species_occupancy == 0
]

# Report results
cat("Species before filtering:", length(species_names), "\n")
cat("Species retained:", length(species_to_keep), "\n")
cat("Species removed (zero occupancy):",
    length(species_to_remove), "\n")

# Create updated matrix, retaining grid identifiers
filtered_matrix <- filtered_matrix[
  ,
  c("cell_id", "grid_label", species_to_keep),
  drop = FALSE
]

# Update species names for subsequent calculations
species_names <- species_to_keep

# Save the updated matrix
write_csv(
  filtered_matrix,
  file.path(
    output_folder,
    "species_presence_absence_matrix_filtered_nonzero.csv"
  )
)


cat("\nUpdated matrix dimensions:",
    nrow(filtered_matrix), "grid cells x",
    length(species_names), "species\n")





# --------------------------------------------------
# 14. Calculate pairwise observed C-scores
#     across ALL retained grid cells
# --------------------------------------------------

# Extract binary species matrix
X <- as.matrix(
  filtered_matrix[, species_names, drop = FALSE]
)

storage.mode(X) <- "numeric"

# Confirm matrix dimensions
cat("Grid cells:", nrow(X), "\n")
cat("Species:", ncol(X), "\n")

# Number of grid cells occupied by each species
Ri <- colSums(X)

# Number of shared cells for every species pair
S <- crossprod(X)

# Get unique species pairs (exclude diagonal and duplicates)
pair_indices <- which(
  upper.tri(S),
  arr.ind = TRUE
)

i <- pair_indices[, 1]
j <- pair_indices[, 2]

# Extract occupancy and shared-site counts
sites_i <- Ri[i]
sites_j <- Ri[j]
shared_sites <- S[pair_indices]

# Calculate observed C-scores
observed_c_score <- (
  (sites_i - shared_sites) *
    (sites_j - shared_sites)
)

# Create output table
c_scores <- data.frame(
  species_i = colnames(X)[i],
  species_j = colnames(X)[j],
  sites_i = as.integer(sites_i),
  sites_j = as.integer(sites_j),
  shared_sites = as.integer(shared_sites),
  observed_c_score = as.numeric(observed_c_score)
)

# Save results
write_csv(
  c_scores,
  file.path(
    output_folder,
    "pairwise_observed_c_scores_all_grid_cells.csv"
  )
)

# Summary
cat("\nC-score calculation complete\n")
cat("Grid cells:", nrow(X), "\n")
cat("Species:", ncol(X), "\n")
cat("Species pairs:", nrow(c_scores), "\n")
cat("Minimum shared sites:", min(c_scores$shared_sites), "\n")
cat("Maximum shared sites:", max(c_scores$shared_sites), "\n")

print(head(c_scores))


# --------------------------------------------------
# 15. Curveball null model for pairwise C-scores
# Tests both segregation and co-occurrence
# --------------------------------------------------

# Install once if necessary:
# install.packages("incidentally")

library(incidentally)

# Use 999 for the main analysis
n_randomizations <- 999
burn_in <- 100

set.seed(123)

# X, pair_indices, i, j, Ri, c_scores, and
# observed_c_score were already created in Step 14.

n_pairs <- nrow(c_scores)

cat("\n-----------------------------\n")
cat("CURVEBALL NULL MODEL\n")
cat("-----------------------------\n")
cat("Grid cells:", nrow(X), "\n")
cat("Species:", ncol(X), "\n")
cat("Species pairs:", n_pairs, "\n")
cat("Burn-in steps:", burn_in, "\n")
cat("Null randomizations:", n_randomizations, "\n")

# --------------------------------------------------
# 15.1 Initialize the randomization chain
# --------------------------------------------------

random_matrix <- X
storage.mode(random_matrix) <- "integer"

# Save original row and column totals
original_row_totals <- rowSums(random_matrix)
original_col_totals <- colSums(random_matrix)

# --------------------------------------------------
# 15.2 Burn-in
# --------------------------------------------------

cat("\nRunning burn-in...\n")

for (b in seq_len(burn_in)) {
  
  random_matrix <- incidentally::curveball(
    random_matrix,
    trades = 5 * nrow(random_matrix)
  )
  
  # Verify that both margins are preserved
  if (b == 1L) {
    stopifnot(
      all(rowSums(random_matrix) == original_row_totals),
      all(colSums(random_matrix) == original_col_totals)
    )
  }
  
  if (b %% 10 == 0) {
    cat("Burn-in step:", b, "/", burn_in, "\n")
  }
}

cat("Burn-in complete.\n")

# --------------------------------------------------
# 15.3 Initialize online null-distribution summaries
# --------------------------------------------------

# Online mean and variance for each species pair
null_mean <- numeric(n_pairs)
null_M2 <- numeric(n_pairs)

# Count null scores in both directions
greater_count <- integer(n_pairs)
lower_count <- integer(n_pairs)

start_time <- Sys.time()

# --------------------------------------------------
# 15.4 Generate randomized C-scores
# --------------------------------------------------

for (iter in seq_len(n_randomizations)) {
  
  # Generate the next randomized matrix
  random_matrix <- incidentally::curveball(
    random_matrix,
    trades = 5 * nrow(random_matrix)
  )
  
  # Number of shared grid cells for each species pair
  shared_random <- crossprod(random_matrix)[pair_indices]
  
  # Calculate randomized C-scores
  # Species occupancy is preserved by Curveball
  randomized_c <- (
    Ri[i] - shared_random
  ) * (
    Ri[j] - shared_random
  )
  
  # Update null mean and variance online
  delta <- randomized_c - null_mean
  
  null_mean <- null_mean + delta / iter
  
  null_M2 <- null_M2 +
    delta * (randomized_c - null_mean)
  
  # Upper tail: observed C-score unusually HIGH
  # Indicates greater-than-expected segregation
  greater_count <- greater_count +
    as.integer(randomized_c >= observed_c_score)
  
  # Lower tail: observed C-score unusually LOW
  # Indicates greater-than-expected co-occurrence
  lower_count <- lower_count +
    as.integer(randomized_c <= observed_c_score)
  
  # Print progress and estimate runtime
  if (iter %% 5 == 0 || iter == n_randomizations) {
    
    elapsed <- as.numeric(
      difftime(Sys.time(), start_time, units = "secs")
    )
    
    estimated_total <- elapsed / iter * n_randomizations
    
    cat(
      "Randomization:", iter, "/", n_randomizations,
      "| Elapsed:", round(elapsed / 60, 2), "minutes",
      "| Estimated total:", round(estimated_total / 60, 2),
      "minutes\n"
    )
  }
}

# --------------------------------------------------
# 15.5 Calculate null statistics and p-values
# --------------------------------------------------

# Null-model standard deviation
null_sd <- sqrt(
  null_M2 / (n_randomizations - 1)
)

# Corrected upper-tail Monte Carlo p-value
p_greater <- (
  greater_count + 1
) / (
  n_randomizations + 1
)

# Corrected lower-tail Monte Carlo p-value
p_less <- (
  lower_count + 1
) / (
  n_randomizations + 1
)

# Standardized effect size
ses <- ifelse(
  null_sd > 0,
  (observed_c_score - null_mean) / null_sd,
  NA_real_
)

# --------------------------------------------------
# 15.6 Correct for multiple testing
# --------------------------------------------------

# Adjust all pairwise tests in both directions together
# This accounts for testing many species pairs
p_adjusted_both <- p.adjust(
  c(p_greater, p_less),
  method = "BH"
)

p_adjusted_BH_greater <- p_adjusted_both[
  seq_len(n_pairs)
]

p_adjusted_BH_less <- p_adjusted_both[
  n_pairs + seq_len(n_pairs)
]

# --------------------------------------------------
# 15.7 Combine results and classify patterns
# --------------------------------------------------

null_results <- c_scores %>%
  mutate(
    null_mean = null_mean,
    null_sd = null_sd,
    c_difference = observed_c_score - null_mean,
    SES = ses,
    
    greater_exceedances = greater_count,
    lower_exceedances = lower_count,
    n_randomizations = n_randomizations,
    
    p_greater = p_greater,
    p_less = p_less,
    
    p_adjusted_BH_greater = p_adjusted_BH_greater,
    p_adjusted_BH_less = p_adjusted_BH_less,
    
    significant_segregation =
      p_adjusted_BH_greater < 0.05,
    
    significant_cooccurrence =
      p_adjusted_BH_less < 0.05,
    
    pattern = case_when(
      significant_segregation &
        !significant_cooccurrence ~
        "Significant segregation",
      
      significant_cooccurrence &
        !significant_segregation ~
        "Significant co-occurrence",
      
      significant_segregation &
        significant_cooccurrence ~
        "Significant in both directions - inspect",
      
      TRUE ~ "No significant pattern"
    )
  )

# --------------------------------------------------
# 15.8 Save results
# --------------------------------------------------

output_file <- file.path(
  output_folder,
  "pairwise_c_scores_null_model_both_directions.csv"
)

write_csv(null_results, output_file)

# --------------------------------------------------
# 15.9 Final summary
# --------------------------------------------------

elapsed_total <- as.numeric(
  difftime(Sys.time(), start_time, units = "mins")
)

cat("\n=============================\n")
cat("NULL MODEL COMPLETE\n")
cat("=============================\n")

cat("Species pairs tested:", nrow(null_results), "\n")
cat("Randomizations:", n_randomizations, "\n")

cat(
  "Pairs with significant segregation:",
  sum(null_results$significant_segregation),
  "\n"
)

cat(
  "Pairs with significant co-occurrence:",
  sum(null_results$significant_cooccurrence),
  "\n"
)

cat(
  "Pairs with neither pattern:",
  sum(
    !null_results$significant_segregation &
      !null_results$significant_cooccurrence
  ),
  "\n"
)

cat(
  "Simulation elapsed time:",
  round(elapsed_total, 2),
  "minutes\n"
)

cat("Results saved to:\n", output_file, "\n")

print(head(null_results))

# Optional: inspect the most significant pairs in each direction

significant_segregation_pairs <- null_results %>%
  filter(significant_segregation) %>%
  arrange(p_adjusted_BH_greater)

significant_cooccurrence_pairs <- null_results %>%
  filter(significant_cooccurrence) %>%
  arrange(p_adjusted_BH_less)

cat(
  "\nSignificant segregation pairs:",
  nrow(significant_segregation_pairs),
  "\n"
)

cat(
  "Significant co-occurrence pairs:",
  nrow(significant_cooccurrence_pairs),
  "\n"
)

write_csv(
  significant_segregation_pairs,
  file.path(output_folder, "significant_segregation_pairs.csv")
)

write_csv(
  significant_cooccurrence_pairs,
  file.path(output_folder, "significant_cooccurrence_pairs.csv")
)
