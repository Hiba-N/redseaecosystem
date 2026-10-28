# install aquamapsdata from GitHub using devtools

install.packages("devtools") 
library("devtools")
install_github("raquamaps/aquamapsdata", dependencies = TRUE)

# initial run-once step required to install remote db locally

library(aquamapsdata)
download_db(force = TRUE)
default_db("sqlite")

library(dplyr)

#example species search
am_search_fuzzy(search_term = "trevally") |> pull(key)

# exact search without parameters returns all results
nrow(am_search_exact())

# exact search giving NULL params shows examples of existing values
# here we see what combinations are present in the dataset for 
# angling, diving, dangerous, highseas, deepwater organisms
am_search_exact(
  angling = NULL, diving = NULL, dangerous = NULL, 
  deepwater = NULL, highseas = NULL, m_invertebrates = NULL)

# exact search without NULL params, specifying values
hits <- 
  am_search_exact(angling = 1, diving = 1, dangerous = 0)

# display results
display <- 
  hits |> mutate(binomen = paste(Genus, Species)) |> 
  select(SpeciesID, binomen, SpecCode, FBname)

knitr::kable(display)


library(leaflet)
library(raster)
library(aquamapsdata)

# get the identifier for the species
key <- am_search_fuzzy("bluespotted")$key
ras <- am_raster(key)

# show the native habitat map
am_map_leaflet(ras, title = "Bluespotted trevally") |> 
  leaflet::fitBounds(lng1 = 100, lat1 = -46, lng2 = 172, lat2 = -2)


keys <- am_search_exact(Genus = "Caranx")$SpeciesID

ras <- am_raster(keys, fun = "count")

am_map_leaflet(ras, title = "Caranx") |> 
  leaflet::fitBounds(lng1 = 100, lat1 = -46, lng2 = 172, lat2 = -2)

am_citation("md")



####acquiring red sea reef fish rasters

library(aquamapsdata)
library(dplyr)
library(readr)
library(terra)

# ---------------------------------------------------------
# 1. Set up folders
# ---------------------------------------------------------

species_folder <- "species_rasters"
family_folder  <- "family_biodiversity"

dir.create(species_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(family_folder, recursive = TRUE, showWarnings = FALSE)


# ---------------------------------------------------------
# 2. Read your CSV files
# ---------------------------------------------------------
source("constants/reef_species.")

reef_species <- read_csv("constants/reef_species.csv")
reef_families <- read_csv("constants/reef_families.csv")

#create species column
reef_species <- reef_species %>%
  mutate(
    Species = sub("^[^ ]+ ", "", Genus)
  )


# ---------------------------------------------------------
# 3. Download species rasters
# ---------------------------------------------------------

for (sp in reef_species$Genus) {
  
  message("Processing species: ", sp)
  
  # Search AquaMaps
  hit <- tryCatch(
    am_search_fuzzy(sp),
    error = function(e) NULL
  )
  
  # Skip if no result
  if (is.null(hit) || nrow(hit) == 0) {
    message("  No AquaMaps match found: ", sp)
    next
  }
  
  # Take the first match
  key <- hit$key[1]
  
  # Download raster
  ras <- tryCatch(
    am_raster(key),
    error = function(e) NULL
  )
  
  # Skip if raster failed
  if (is.null(ras)) {
    message("  Raster failed: ", sp)
    next
  }
  
  # Make a safe filename
  filename <- gsub("[^A-Za-z0-9_-]", "_", sp)
  
  # Save raster
  writeRaster(
    rast(ras),
    filename = file.path(
      species_folder,
      paste0(filename, ".tif")
    ),
    overwrite = TRUE
  )
  
  message("  Saved: ", filename, ".tif")
}


# ---------------------------------------------------------
# 6. Create biodiversity raster for each family
# ---------------------------------------------------------
for (fam in reef_families$Family) {
  
  message("Processing family: ", fam)
  
  # Find all AquaMaps species belonging to this family
  hits <- tryCatch(
    am_search_exact(Family = fam),
    error = function(e) NULL
  )
  
  # Check whether anything was found
  if (is.null(hits) || nrow(hits) == 0) {
    message("  No AquaMaps species found")
    next
  }
  
  # Get SpeciesIDs
  keys <- hits$SpeciesID
  
  # Remove missing/duplicate IDs
  keys <- unique(na.omit(keys))
  
  message("  Species found: ", length(keys))
  
  # Create biodiversity raster
  ras <- tryCatch(
    am_raster(
      keys,
      fun = "count"
    ),
    error = function(e) NULL
  )
  
  if (is.null(ras)) {
    message("  Failed to create raster")
    next
  }
  
  # Make safe filename
  filename <- paste0(
    gsub("[^A-Za-z0-9_-]", "_", fam),
    "_biodiversity.tif"
  )
  
  # Save raster
  writeRaster(
    rast(ras),
    file.path(
      family_folder,
      filename
    ),
    overwrite = TRUE
  )
  
  message("  Saved: ", filename)
}