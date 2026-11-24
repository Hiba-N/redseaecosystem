
library(scales)

'''
red_sea_fishbase <- species_by_ecosystem(
  ecosystem = "Red Sea",
  server = "fishbase"
)

column Species is needed


red_sea_sealifebase <- species_by_ecosystem(
  ecosystem = "Red Sea",
  server = "sealifebase"
)

column Species is needed

nutrient data is at "C:\Users\nasirh\OneDrive - KAUST\repositories\redseaecosystem\probability_weighted_nutrients\red_sea_fish_nutrients_standardised.csv"
with columns 
Scientific name	AFCD protein (g)	AFCD omega3_epa_dha (g)	AFCD vitamin_a (mcg)	AFCD vitamin_b12 (mcg)	AFCD vitamin_c (mg)	AFCD vitamin_b6 (mg)	AFCD vitamin_d (mcg)	AFCD calcium (mg)	AFCD iron_total (mg)	AFCD magnesium (mg)	AFCD selenium (mcg)	AFCD zinc (mg)	AFCD iodine (mcg)

work with nutrient table. 
create column source
if Scientific name is in Species list in red_sea_fishbase source = fishbase
if Scientific name is in Species list inred_sea_sealifebase source = sealifebase

create rda table from following values (these are per 100g)


    47.823 g for protein,
    0.4333 g for Omega-3 (EPA + DHA),
    767.727 mcg for Vitamin A,
    2.1272727 mcg, Vitamin B12
    73.8636 mg, Vitamin C
    1.331818 mg, Vitamin B6
    15 mcg, Vitamin D
    1025.5 mg, "Calcium"
    11.97 mg, "Iron"
    299.77273 mg, "Magnesium"
    50.2273 mcg,   "Selenium"
    8.909 mg,
    173 mcg

  

for fishbase species create one histogram per nutrient that shows % of rda coverage for each species going from 0 to 100 and even above
    specifically mark a vertical line at 10%
    
similarly for sealifebase
    
and then one set of histograms with all species

in total expecting only 13 x 3 histograms please
'''
#general histogram plotting for all nutrients

# ============================================================
# 1. LOAD LIBRARIES
# ============================================================

library(rfishbase)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)


# ============================================================
# 2. GET RED SEA SPECIES LISTS
# ============================================================

red_sea_fishbase <- species_by_ecosystem(
  ecosystem = "Red Sea",
  server = "fishbase"
)

red_sea_sealifebase <- species_by_ecosystem(
  ecosystem = "Red Sea",
  server = "sealifebase"
)


# ============================================================
# 3. EXTRACT SPECIES NAMES
# ============================================================

fishbase_species <- red_sea_fishbase %>%
  pull(Species) %>%
  na.omit() %>%
  unique()

sealifebase_species <- red_sea_sealifebase %>%
  pull(Species) %>%
  na.omit() %>%
  unique()


# ============================================================
# 4. LOAD NUTRIENT DATA
# ============================================================

nutrient_file <- paste0(
  "C:/Users/nasirh/OneDrive - KAUST/repositories/",
  "redseaecosystem/probability_weighted_nutrients/",
  "red_sea_fish_nutrients_standardised.csv"
)

nutrient_data <- read_csv(
  nutrient_file,
  show_col_types = FALSE
)


# ============================================================
# 5. CREATE SOURCE COLUMN
# ============================================================

nutrient_data <- nutrient_data %>%
  mutate(
    source = case_when(
      `Scientific name` %in% fishbase_species &
        `Scientific name` %in% sealifebase_species ~ "both",
      
      `Scientific name` %in% fishbase_species ~ "fishbase",
      
      `Scientific name` %in% sealifebase_species ~ "sealifebase",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(source))


# ============================================================
# 6. DEFINE RDA VALUES PER 100 G
# ============================================================

rda_values <- tibble(
  nutrient = c(
    "protein",
    "omega3_epa_dha",
    "vitamin_a",
    "vitamin_b12",
    "vitamin_c",
    "vitamin_b6",
    "vitamin_d",
    "calcium",
    "iron_total",
    "magnesium",
    "selenium",
    "zinc",
    "iodine"
  ),
  
  rda = c(
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
  )
)


# ============================================================
# 7. DEFINE NUTRIENT COLUMN MAPPING
# ============================================================

nutrient_columns <- c(
  protein = "AFCD protein (g)",
  omega3_epa_dha = "AFCD omega3_epa_dha (g)",
  vitamin_a = "AFCD vitamin_a (mcg)",
  vitamin_b12 = "AFCD vitamin_b12 (mcg)",
  vitamin_c = "AFCD vitamin_c (mg)",
  vitamin_b6 = "AFCD vitamin_b6 (mg)",
  vitamin_d = "AFCD vitamin_d (mcg)",
  calcium = "AFCD calcium (mg)",
  iron_total = "AFCD iron_total (mg)",
  magnesium = "AFCD magnesium (mg)",
  selenium = "AFCD selenium (mcg)",
  zinc = "AFCD zinc (mg)",
  iodine = "AFCD iodine (mcg)"
)


# ============================================================
# 8. CALCULATE % RDA FOR EACH SPECIES
# ============================================================

for (nutrient in names(nutrient_columns)) {
  
  nutrient_col <- nutrient_columns[[nutrient]]
  
  rda_value <- rda_values %>%
    filter(nutrient == !!nutrient) %>%
    pull(rda)
  
  nutrient_data[[paste0(nutrient, "_rda_percent")]] <-
    (nutrient_data[[nutrient_col]] / rda_value) * 100
}


# ============================================================
# 9. DEFINE DATASETS FOR PLOTTING
# ============================================================

fishbase_data <- nutrient_data %>%
  filter(source %in% c("fishbase", "both"))

sealifebase_data <- nutrient_data %>%
  filter(source %in% c("sealifebase", "both"))

all_species_data <- nutrient_data


# ============================================================
# 10. CREATE OUTPUT FOLDER
# ============================================================

output_folder <- file.path(
  dirname(nutrient_file),
  "rda_histograms"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 11. FUNCTION TO CREATE ONE HISTOGRAM
# ============================================================

create_rda_histogram <- function(data, nutrient, dataset_name) {
  
  rda_column <- paste0(nutrient, "_rda_percent")
  
  plot_data <- data %>%
    select(all_of(rda_column)) %>%
    rename(rda_percent = all_of(rda_column)) %>%
    filter(!is.na(rda_percent), is.finite(rda_percent))
  
  nutrient_label <- nutrient_columns[[nutrient]]
  
  nutrient_label <- str_remove(
    nutrient_label,
    "^AFCD "
  )
  
  p <- ggplot(
    plot_data,
    aes(x = rda_percent)
  ) +
    
    geom_histogram(
      bins = 30,
      fill = "steelblue",
      color = "black"
    ) +
    
    geom_vline(
      xintercept = 10,
      linetype = "dashed",
      linewidth = 1,
      color = "red"
    ) +
    
    labs(
      title = paste(
        dataset_name,
        "-",
        nutrient_label
      ),
      
      subtitle = "Percentage of RDA provided per 100 g of fish",
      
      x = "% RDA coverage per 100 g",
      
      y = "Number of species"
    ) +
    
    theme_minimal(base_size = 12) +
    
    theme(
      plot.title = element_text(
        face = "bold"
      )
    )
  
  # Save histogram
  filename <- paste0(
    dataset_name,
    "_",
    nutrient,
    ".png"
  )
  
  ggsave(
    filename = file.path(output_folder, filename),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  return(p)
}


# ============================================================
# 12. GENERATE 13 HISTOGRAMS - FISHBASE
# ============================================================

fishbase_plots <- list()

for (nutrient in names(nutrient_columns)) {
  
  fishbase_plots[[nutrient]] <- create_rda_histogram(
    data = fishbase_data,
    nutrient = nutrient,
    dataset_name = "FishBase"
  )
}


# ============================================================
# 13. GENERATE 13 HISTOGRAMS - SEALIFEBASE
# ============================================================

sealifebase_plots <- list()

for (nutrient in names(nutrient_columns)) {
  
  sealifebase_plots[[nutrient]] <- create_rda_histogram(
    data = sealifebase_data,
    nutrient = nutrient,
    dataset_name = "SeaLifeBase"
  )
}


# ============================================================
# 14. GENERATE 13 HISTOGRAMS - ALL SPECIES
# ============================================================

all_species_plots <- list()

for (nutrient in names(nutrient_columns)) {
  
  all_species_plots[[nutrient]] <- create_rda_histogram(
    data = all_species_data,
    nutrient = nutrient,
    dataset_name = "All_species"
  )
}


# ============================================================
# 15. PRINT SUMMARY
# ============================================================

cat("\n==================================================\n")
cat("RDA HISTOGRAMS COMPLETE\n")
cat("==================================================\n")

cat("\nFishBase species:", nrow(fishbase_data), "\n")
cat("SeaLifeBase species:", nrow(sealifebase_data), "\n")
cat("All species:", nrow(all_species_data), "\n")

cat("\nTotal histograms generated:", 
    length(fishbase_plots) +
      length(sealifebase_plots) +
      length(all_species_plots),
    "\n")

cat("\nOutput folder:\n")
print(output_folder)


#global pmin 

'''
take fishbase + sealifebase species list + nutrient data for each fish

from list of all species take 10 at random.
make all combinations possible from these fish from all counts
eg 
selected fish is 1 through 10
first level combinations (1 fish only)
1,2,3...
second level combinations (2 fish only)
12, 13, 14 ...
all the way up to combinations of 10 (10 levels)

the nutrient data we have for each fish is per 100 g but for these combos
each species contributes an equal ratio to a 100g total combo mix

get pmin for each nutrient for each combo at each level
pmin is minimum grams it takes to fulfill rda requirements for a certain nutrient
average all values within each level

repeat 1000 times, so you have 1000 values for each level

result will be a regression graph based off the 1000 values per level
level on x axis (species count)
y axis rda percentage being covered
'''


# ============================================================
# 1. LOAD LIBRARIES
# ============================================================

library(rfishbase)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)


# ============================================================
# 2. SIMULATION SETTINGS
# ============================================================

set.seed(123)

n_iterations <- 1000
n_species_selected <- 10

# Each mixture represents 100 g total fish
mixture_weight <- 100


# ============================================================
# 3. GET RED SEA SPECIES LISTS
# ============================================================

red_sea_fishbase <- species_by_ecosystem(
  ecosystem = "Red Sea",
  server = "fishbase"
)

red_sea_sealifebase <- species_by_ecosystem(
  ecosystem = "Red Sea",
  server = "sealifebase"
)


# Extract unique species

fishbase_species <- red_sea_fishbase %>%
  pull(Species) %>%
  as.character() %>%
  str_squish() %>%
  unique()

sealifebase_species <- red_sea_sealifebase %>%
  pull(Species) %>%
  as.character() %>%
  str_squish() %>%
  unique()


# ============================================================
# 4. LOAD NUTRIENT DATA
# ============================================================

nutrient_file <- paste0(
  "C:/Users/nasirh/OneDrive - KAUST/repositories/",
  "redseaecosystem/probability_weighted_nutrients/",
  "red_sea_fish_nutrients_standardised.csv"
)

nutrient_data <- read_csv(
  nutrient_file,
  show_col_types = FALSE
) %>%
  mutate(
    `Scientific name` = str_squish(
      as.character(`Scientific name`)
    )
  )


# ============================================================
# 5. CREATE COMBINED SPECIES LIST
# ============================================================

all_red_sea_species <- tibble(
  Species = sort(unique(c(
    fishbase_species,
    sealifebase_species
  )))
) %>%
  filter(
    !is.na(Species),
    Species != ""
  ) %>%
  mutate(
    source = case_when(
      Species %in% fishbase_species &
        Species %in% sealifebase_species ~ "both",
      
      Species %in% fishbase_species ~ "fishbase",
      
      Species %in% sealifebase_species ~ "sealifebase"
    )
  )


# ============================================================
# 6. MATCH SPECIES WITH NUTRIENT DATA
# ============================================================

simulation_data <- nutrient_data %>%
  inner_join(
    all_red_sea_species,
    by = c("Scientific name" = "Species")
  )


# Check for duplicate species in nutrient data

duplicates <- simulation_data %>%
  count(`Scientific name`) %>%
  filter(n > 1)

if (nrow(duplicates) > 0) {
  
  print(duplicates)
  
  stop(
    "Duplicate species found in nutrient data. ",
    "Resolve these before running the simulation."
  )
}


# ============================================================
# 7. DEFINE NUTRIENT COLUMNS
# ============================================================

nutrient_columns <- c(
  protein = "AFCD protein (g)",
  omega3_epa_dha = "AFCD omega3_epa_dha (g)",
  vitamin_a = "AFCD vitamin_a (mcg)",
  vitamin_b12 = "AFCD vitamin_b12 (mcg)",
  vitamin_c = "AFCD vitamin_c (mg)",
  vitamin_b6 = "AFCD vitamin_b6 (mg)",
  vitamin_d = "AFCD vitamin_d (mcg)",
  calcium = "AFCD calcium (mg)",
  iron_total = "AFCD iron_total (mg)",
  magnesium = "AFCD magnesium (mg)",
  selenium = "AFCD selenium (mcg)",
  zinc = "AFCD zinc (mg)",
  iodine = "AFCD iodine (mcg)"
)


# ============================================================
# 8. DEFINE RDA VALUES PER 100 G
# ============================================================

rda_values <- c(
  protein = 47.823,
  omega3_epa_dha = 0.4333,
  vitamin_a = 767.727,
  vitamin_b12 = 2.1272727,
  vitamin_c = 73.8636,
  vitamin_b6 = 1.331818,
  vitamin_d = 15,
  calcium = 1025.5,
  iron_total = 11.97,
  magnesium = 299.77273,
  selenium = 50.2273,
  zinc = 8.909,
  iodine = 173
)


# ============================================================
# 9. PREPARE NUTRIENT MATRIX
# ============================================================

nutrient_matrix <- simulation_data %>%
  select(all_of(unname(nutrient_columns))) %>%
  mutate(
    across(everything(), as.numeric)
  ) %>%
  as.matrix()

# Give matrix columns standard nutrient names

colnames(nutrient_matrix) <- names(nutrient_columns)

species_names <- simulation_data$`Scientific name`

n_available_species <- nrow(nutrient_matrix)


# Check sufficient species

if (n_available_species < n_species_selected) {
  stop("Fewer than 10 matched species with nutrient data.")
}

cat("\nSpecies available for simulation:",
    n_available_species, "\n")


# ============================================================
# 10. PREPARE RESULTS ARRAY
# ============================================================

# Dimensions:
# 1000 iterations x 10 levels x 13 nutrients

pmin_results <- array(
  NA_real_,
  dim = c(
    n_iterations,
    n_species_selected,
    length(nutrient_columns)
  ),
  dimnames = list(
    iteration = NULL,
    level = as.character(1:n_species_selected),
    nutrient = names(nutrient_columns)
  )
)


# ============================================================
# 11. MONTE CARLO SIMULATION
# ============================================================

for (iteration in seq_len(n_iterations)) {
  
  # ----------------------------------------------------------
  # Randomly select 10 species without replacement
  # ----------------------------------------------------------
  
  selected_indices <- sample(
    seq_len(n_available_species),
    size = n_species_selected,
    replace = FALSE
  )
  
  selected_matrix <- nutrient_matrix[
    selected_indices,
    ,
    drop = FALSE
  ]
  
  
  # ----------------------------------------------------------
  # Loop through species combination levels
  # ----------------------------------------------------------
  
  for (level in seq_len(n_species_selected)) {
    
    # Generate every possible combination of this size
    
    combinations <- combn(
      n_species_selected,
      level,
      simplify = FALSE
    )
    
    
    # --------------------------------------------------------
    # Calculate Pmin for every nutrient
    # --------------------------------------------------------
    
    for (nutrient in names(nutrient_columns)) {
      
      # Nutrient concentrations for the selected 10 fish
      
      concentrations <- selected_matrix[, nutrient]
      
      # Calculate mean nutrient concentration for each mixture
      # All species contribute equal proportions to 100 g
      
      mixture_concentrations <- vapply(
        combinations,
        function(indices) {
          
          values <- concentrations[indices]
          
          # Do not treat missing values as zero
          
          if (any(!is.finite(values))) {
            return(NA_real_)
          }
          
          mean(values)
          
        },
        numeric(1)
      )
      
      
      # ------------------------------------------------------
      # Pmin = grams of mixture needed to meet the RDA
      # ------------------------------------------------------
      
      valid_concentrations <- mixture_concentrations[
        is.finite(mixture_concentrations) &
          mixture_concentrations > 0
      ]
      
      pmin_values <- (
        rda_values[nutrient] /
          valid_concentrations
      ) * mixture_weight
      
      
      # Average Pmin across all combinations at this level
      
      if (length(pmin_values) > 0) {
        
        pmin_results[
          iteration,
          level,
          nutrient
        ] <- mean(pmin_values)
        
      }
      
    }
    
  }
  
  
  # Progress reporting
  
  if (iteration %% 50 == 0) {
    
    cat(
      "Completed iteration",
      iteration,
      "of",
      n_iterations,
      "\n"
    )
    
  }
  
}



# ============================================================
# 12. CONVERT RESULTS TO LONG FORMAT
# ============================================================

pmin_long <- as.data.frame(
  as.table(pmin_results),
  responseName = "mean_pmin_g"
) %>%
  rename(
    iteration = iteration,
    species_count = level,
    nutrient = nutrient
  ) %>%
  mutate(
    iteration = as.integer(as.character(iteration)),
    species_count = as.integer(as.character(species_count)),
    nutrient = as.character(nutrient)
  )

# Check results

head(pmin_long)

cat("\nRows generated:", nrow(pmin_long), "\n")
cat("\nNutrients:\n")
print(unique(pmin_long$nutrient))

cat("\nSpecies levels:\n")
print(unique(pmin_long$species_count))



# ============================================================
# 13. CALCULATE RDA COVERAGE
# ============================================================

results <- pmin_long %>%
  mutate(
    
    # RDA coverage provided by a 100 g mixture
    
    rda_coverage_percent = case_when(
      
      is.na(mean_pmin_g) ~ NA_real_,
      
      mean_pmin_g <= 0 ~ NA_real_,
      
      TRUE ~ (mixture_weight / mean_pmin_g) * 100
      
    )
    
  )


# ============================================================
# 14. SUMMARISE THE 1000 ITERATIONS
# ============================================================

summary_results <- results %>%
  group_by(
    species_count,
    nutrient
  ) %>%
  summarise(
    
    mean_coverage = mean(
      rda_coverage_percent,
      na.rm = TRUE
    ),
    
    sd_coverage = sd(
      rda_coverage_percent,
      na.rm = TRUE
    ),
    
    mean_pmin = mean(
      mean_pmin_g,
      na.rm = TRUE
    ),
    
    n_valid_iterations = sum(
      is.finite(rda_coverage_percent)
    ),
    
    .groups = "drop"
    
  )


# ============================================================
# 15. CREATE OUTPUT FOLDER
# ============================================================

output_folder <- file.path(
  dirname(nutrient_file),
  "global_pmin_simulation"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 16. SAVE SIMULATION RESULTS
# ============================================================

write_csv(
  results,
  file.path(
    output_folder,
    "global_pmin_all_iterations.csv"
  )
)

write_csv(
  summary_results,
  file.path(
    output_folder,
    "global_pmin_summary.csv"
  )
)

saveRDS(
  pmin_results,
  file.path(
    output_folder,
    "global_pmin_results.rds"
  )
)


# ============================================================
# 17. GENERATE REGRESSION GRAPHS
# ============================================================

regression_plots <- list()

for (nutrient in names(nutrient_columns)) {
  
  plot_data <- results %>%
    filter(
      nutrient == !!nutrient,
      is.finite(rda_coverage_percent)
    )
  
  nutrient_label <- nutrient_columns[[nutrient]] %>%
    str_remove("^AFCD ")
  
  
  p <- ggplot(
    plot_data,
    aes(
      x = species_count,
      y = rda_coverage_percent
    )
  ) +
    
    # All 1000 iteration-level observations
    
    geom_jitter(
      width = 0.12,
      height = 0,
      alpha = 0.20,
      size = 1
    ) +
    
    # Linear regression
    
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = "red",
      linewidth = 1
    ) +
    
    # Mean coverage at each species level
    
    geom_point(
      data = summary_results %>%
        filter(nutrient == !!nutrient),
      aes(
        x = species_count,
        y = mean_coverage
      ),
      inherit.aes = FALSE,
      size = 3,
      color = "black"
    ) +
    
    scale_x_continuous(
      breaks = 1:10
    ) +
    
    labs(
      title = paste(
        "Global Pmin -",
        nutrient_label
      ),
      
      subtitle = paste(
        "1000 random selections |",
        "10 species per selection"
      ),
      
      x = "Number of species in mixture",
      
      y = "RDA coverage (%)",
      
      caption = paste(
        "Each observation is the mean across all valid",
        "combinations within one iteration and level.",
        "Black points = mean across iterations."
      )
    ) +
    
    theme_minimal(base_size = 12) +
    
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
  
  
  regression_plots[[nutrient]] <- p
  
  
  # Save individual graph
  
  ggsave(
    filename = file.path(
      output_folder,
      paste0("regression_", nutrient, ".png")
    ),
    plot = p,
    width = 9,
    height = 6,
    dpi = 300
  )
  
}


# ============================================================
# 18. FINAL SUMMARY
# ============================================================

cat("\n============================================\n")
cat("GLOBAL PMIN SIMULATION COMPLETE\n")
cat("============================================\n")

cat("Iterations:", n_iterations, "\n")

cat("Species per random selection:",
    n_species_selected, "\n")

cat("Species available:", n_available_species, "\n")

cat("Combination levels:", n_species_selected, "\n")

cat("Nutrients:", length(nutrient_columns), "\n")

cat("Regression graphs:", length(regression_plots), "\n")

cat("\nResults saved to:\n")
cat(output_folder, "\n")

cat("\n============================================\n")





# ============================================================
# 18. GENERATE SPECIES RICHNESS VS PMIN GRAPHS
# ============================================================

pmin_plots <- list()

for (nutrient in names(nutrient_columns)) {
  
  plot_data <- results %>%
    filter(
      nutrient == !!nutrient,
      is.finite(mean_pmin_g)
    )
  
  nutrient_label <- nutrient_columns[[nutrient]] %>%
    str_remove("^AFCD ")
  
  
  p <- ggplot(
    plot_data,
    aes(
      x = species_count,
      y = mean_pmin_g
    )
  ) +
    
    # Individual iteration-level observations
    
    geom_jitter(
      width = 0.12,
      height = 0,
      alpha = 0.20,
      size = 1
    ) +
    
    # Linear regression
    
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = "red",
      linewidth = 1
    ) +
    
    # Mean Pmin across iterations
    
    geom_point(
      data = summary_results %>%
        filter(nutrient == !!nutrient),
      aes(
        x = species_count,
        y = mean_pmin
      ),
      inherit.aes = FALSE,
      size = 3,
      color = "black"
    ) +
    
    scale_x_continuous(
      breaks = 1:10
    ) +
    
    labs(
      title = paste(
        "Species Richness vs Pmin -",
        nutrient_label
      ),
      
      subtitle = "1000 random selections | 10 species per selection",
      
      x = "Species richness (number of species)",
      
      y = "Pmin (grams of fish mixture)",
      
      caption = paste(
        "Each observation represents mean Pmin across",
        "all valid combinations within one iteration.",
        "Black points = mean across iterations."
      )
    ) +
    
    theme_minimal(base_size = 12) +
    
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
  
  
  pmin_plots[[nutrient]] <- p
  
  
  # Save individual graph
  
  ggsave(
    filename = file.path(
      output_folder,
      paste0("pmin_regression_", nutrient, ".png")
    ),
    plot = p,
    width = 9,
    height = 6,
    dpi = 300
  )
  
}


# ============================================================
# 19. PRINT FINAL GRAPH SUMMARY
# ============================================================

cat("\nPmin regression graphs generated:",
    length(pmin_plots), "\n")

cat("\nTotal regression graphs:",
    length(regression_plots) + length(pmin_plots), "\n")

cat("\nOutput folder:\n")
print(output_folder)

fb_tables()







#final summarized graphs


# ============================================================
# 20. COMPARE LINEAR AND QUADRATIC MODELS
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)


# Function to select the best model using AIC

select_best_model <- function(data, response) {
  
  data <- data %>%
    filter(
      is.finite(.data[[response]]),
      is.finite(species_count)
    )
  
  linear_model <- lm(
    reformulate("species_count", response = response),
    data = data
  )
  
  quadratic_model <- lm(
    as.formula(
      paste(response, "~ species_count + I(species_count^2)")
    ),
    data = data
  )
  
  if (AIC(quadratic_model) < AIC(linear_model)) {
    best_model <- quadratic_model
    model_type <- "Quadratic"
  } else {
    best_model <- linear_model
    model_type <- "Linear"
  }
  
  list(
    model = best_model,
    model_type = model_type,
    linear_aic = AIC(linear_model),
    quadratic_aic = AIC(quadratic_model)
  )
}


# ============================================================
# 21. FUNCTION TO GENERATE FITTED CURVES
# ============================================================

generate_fitted_curves <- function(data, response) {
  
  fitted_curves <- list()
  model_comparison <- list()
  
  for (nutrient in names(nutrient_columns)) {
    
    nutrient_data <- data %>%
      filter(nutrient == !!nutrient)
    
    selected <- select_best_model(
      nutrient_data,
      response
    )
    
    # Prediction across species richness 1 to 10
    
    prediction_data <- data.frame(
      species_count = seq(1, 10, length.out = 200)
    )
    
    predictions <- predict(
      selected$model,
      newdata = prediction_data,
      interval = "confidence"
    )
    
    fitted_curves[[nutrient]] <- data.frame(
      species_count = prediction_data$species_count,
      fitted = predictions[, "fit"],
      lower = predictions[, "lwr"],
      upper = predictions[, "upr"],
      nutrient = nutrient
    )
    
    model_comparison[[nutrient]] <- data.frame(
      nutrient = nutrient,
      selected_model = selected$model_type,
      linear_AIC = selected$linear_aic,
      quadratic_AIC = selected$quadratic_aic
    )
    
  }
  
  list(
    curves = bind_rows(fitted_curves),
    comparison = bind_rows(model_comparison)
  )
}


# ============================================================
# 22. PREPARE OBSERVED MEANS
# ============================================================

# Mean coverage and mean Pmin across iterations
# at each species richness level

plot_summary <- results %>%
  group_by(species_count, nutrient) %>%
  summarise(
    mean_coverage = mean(
      rda_coverage_percent,
      na.rm = TRUE
    ),
    
    mean_pmin = mean(
      mean_pmin_g,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ============================================================
# 23. GENERATE BOTH SETS OF REGRESSION CURVES
# ============================================================

coverage_models <- generate_fitted_curves(
  data = results,
  response = "rda_coverage_percent"
)

pmin_models <- generate_fitted_curves(
  data = results,
  response = "mean_pmin_g"
)


coverage_curves <- coverage_models$curves
pmin_curves <- pmin_models$curves


# ============================================================
# 24. NUTRIENT LABELS
# ============================================================

nutrient_labels <- c(
  protein = "Protein",
  omega3_epa_dha = "Omega-3 (EPA + DHA)",
  vitamin_a = "Vitamin A",
  vitamin_b12 = "Vitamin B12",
  vitamin_c = "Vitamin C",
  vitamin_b6 = "Vitamin B6",
  vitamin_d = "Vitamin D",
  calcium = "Calcium",
  iron_total = "Iron",
  magnesium = "Magnesium",
  selenium = "Selenium",
  zinc = "Zinc",
  iodine = "Iodine"
)


library(ggrepel)
# ============================================================
# 25. GRAPH 1: SPECIES RICHNESS VS RDA COVERAGE
# ============================================================

rda_graph <- ggplot() +
  
  # Confidence intervals
  
  geom_ribbon(
    data = coverage_curves,
    aes(
      x = species_count,
      ymin = lower,
      ymax = upper,
      fill = nutrient
    ),
    alpha = 0.08
  ) +
  
  # Fitted regression lines
  
  geom_line(
    data = coverage_curves,
    aes(
      x = species_count,
      y = fitted,
      color = nutrient
    ),
    linewidth = 1.2
  ) +
  
  # Observed mean values
  
  geom_point(
    data = plot_summary,
    aes(
      x = species_count,
      y = mean_coverage,
      color = nutrient
    ),
    size = 1.8,
    alpha = 0.65
  ) +
  
  # Labels for observed RDA coverage
  
  geom_text_repel(
    data = plot_summary,
    aes(
      x = species_count,
      y = mean_coverage,
      label = round(mean_coverage, 1),
      color = nutrient
    ),
    size = 2.5,
    show.legend = FALSE,
    max.overlaps = Inf,
    box.padding = 0.3,
    point.padding = 0.2,
    segment.color = "grey60"
  ) +
  
  scale_x_continuous(
    breaks = 1:10,
    limits = c(1, 10)
  ) +
  
  scale_color_viridis_d(
    option = "turbo",
    labels = nutrient_labels,
    name = "Nutrient"
  ) +
  
  scale_fill_viridis_d(
    option = "turbo",
    labels = nutrient_labels,
    guide = "none"
  ) +
  
  labs(
    title = "Species Richness and Nutritional Contribution",
    subtitle = "RDA coverage of 100 g fish mixtures",
    x = "Species richness (number of species)",
    y = "RDA coverage (%)"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(size = 11),
    
    axis.title = element_text(face = "bold"),
    
    legend.position = "right",
    
    legend.title = element_text(face = "bold"),
    
    legend.text = element_text(size = 9),
    
    panel.grid.minor = element_blank()
  )


# ============================================================
# 26. GRAPH 2: SPECIES RICHNESS VS PMIN
# ============================================================
library(ggbreak)

pmin_graph <- ggplot() +
  
  # Confidence intervals
  
  geom_ribbon(
    data = pmin_curves,
    aes(
      x = species_count,
      ymin = lower,
      ymax = upper,
      fill = nutrient
    ),
    alpha = 0.08
  ) +
  
  # Fitted regression lines
  
  geom_line(
    data = pmin_curves,
    aes(
      x = species_count,
      y = fitted,
      color = nutrient
    ),
    linewidth = 1.2
  ) +
  
  # Observed mean values
  
  geom_point(
    data = plot_summary,
    aes(
      x = species_count,
      y = mean_pmin,
      color = nutrient
    ),
    size = 1.8,
    alpha = 0.65
  ) +

  
  scale_x_continuous(
    breaks = 1:10,
    limits = c(1, 10)
  ) +

  scale_y_continuous(
    trans = scales::asinh_trans(),
    breaks = c(
      50, 250, 500, 750, 1000, 1250,
      1500, 1750, 2000, 2500, 3000,
      4000, 5000, 7500, 10000, 15000, 20000
    ),
    labels = scales::label_comma(),
    limits = c(50, 20000)
  ) +
  
  scale_color_viridis_d(
    option = "turbo",
    labels = nutrient_labels,
    name = "Nutrient"
  ) +
  
  scale_fill_viridis_d(
    option = "turbo",
    labels = nutrient_labels,
    guide = "none"
  ) +
  
  labs(
    title = "Species Richness and Minimum Fish Quantity",
    subtitle = "Minimum mixture quantity required to meet nutrient RDA",
    x = "Species richness (number of species)",
    y = "Pmin (grams of fish mixture)"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    
    plot.subtitle = element_text(size = 11),
    
    axis.title = element_text(face = "bold"),
    
    legend.position = "right",
    
    legend.title = element_text(face = "bold"),
    
    legend.text = element_text(size = 9),
    
    panel.grid.minor = element_blank()
  )


# ============================================================
# 27. SAVE THE TWO FINAL GRAPHS
# ============================================================

ggsave(
  file.path(output_folder, "FINAL_all_nutrients_RDA.png"),
  plot = rda_graph,
  width = 15,
  height = 9,
  dpi = 400
)

ggsave(
  file.path(output_folder, "FINAL_all_nutrients_Pmin.png"),
  plot = pmin_graph,
  width = 15,
  height = 9,
  dpi = 400
)


# ============================================================
# 28. SAVE MODEL SELECTION RESULTS
# ============================================================

model_selection <- bind_rows(
  coverage_models$comparison %>%
    mutate(response = "RDA coverage"),
  
  pmin_models$comparison %>%
    mutate(response = "Pmin")
)

write_csv(
  model_selection,
  file.path(
    output_folder,
    "FINAL_model_selection.csv"
  )
)


# ============================================================
# 29. DISPLAY GRAPHS
# ============================================================

print(rda_graph)
print(pmin_graph)

cat("\nFINAL GRAPHS SAVED SUCCESSFULLY\n")

print(model_selection)

