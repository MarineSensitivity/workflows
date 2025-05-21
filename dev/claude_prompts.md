## 2025-05-2a.a r_env

The Update the AquaMaps species environment variables in `r_env` to use the


## 2025-05-20.b + Bio-Oracle replication

Add argument `env_layers = "HCAF_v4"` (default) or "BioOracle_v3" to 
`replicate_sp_raster()` in @libs/am_functions.R to use the 
1/20 decimal degree Bio-Oracle data layers versus the default AquaMaps Half-Degree Cells Authority 
File (HCAF). Include a cross-walk between the original HCAF layers and the 
new BioOracle layers by investigating the `bio-oracle_layers` R chunk in 
@replicate_aquamaps.qmd, @data/aquamaps_meta_layers.csv, and 
@data/bio-oracle_datasets_relevant.csv.

Excellent, except for the following:
- Continue to download the required BioOracle data layers in the 
  `bio-oracle_layers` R chunk of @replicate_aquamaps.qmd.
- Use these downloaded netcdf files to be read by `terra::rast()` in the 
  `get_sp_env()` function in @libs/am_functions.R with the additional argument 
  of using HCAF_v4 or BioOracle_v3.
- Apply similar logic to the rest of the functions in @libs/am_functions.R and
  their usage in @replicate_aquamaps.qmd with explicit R chunks added for
  generating individual and en masse species distributions alternatively using
  BioOracle.

### Response

⏺ Update Todos
  ⎿  ☒ Analyze existing replicate_sp_raster function in am_functions.R
     ☒ Identify BioOracle v3 equivalent layers for AquaMaps environmental variables
     ☒ Modify replicate_sp_raster function to accept env_layers parameter
     ☒ Create BioOracle to AquaMaps layer crosswalk mapping
     ☒ Add documentation notes about layer sources in replicate_aquamaps.qmd

⏺ I've implemented the requested changes:

  1. Modified the replicate_sp_raster() function in am_functions.R to accept an optional env_layers parameter with values
  "HCAF_v4" (default) or "BioOracle_v3"
  2. Created a mapping between AquaMaps environmental variables and their BioOracle v3 equivalents:
    - depth → terrain_characteristics.bathymetry_min/mean/max
    - temp (surface) → thetao_baseline_*_depthsurf
    - temp (bottom) → thetao_baseline_*_depthmax
    - salinity → so_baseline layers
    - oxygen → o2_baseline layers
    - primary production → phyc_baseline
    - ice concentration → siconc_baseline
  3. Added proper caching with a suffix for BioOracle results
  4. Documented the changes in the replicate_aquamaps.qmd file with a detailed crosswalk table and usage examples

  The new feature allows for reproducing AquaMaps species distributions with higher resolution (1/20 degree) environmental data
  instead of the default half-degree resolution.

⏺ Update Todos
  ⎿  ☒ Analyze existing replicate_sp_raster function in am_functions.R
     ☒ Identify BioOracle v3 equivalent layers for AquaMaps environmental variables
     ☒ Modify replicate_sp_raster function to accept env_layers parameter
     ☒ Create BioOracle to AquaMaps layer crosswalk mapping
     ☒ Update get_sp_env function to support BioOracle data sources
     ☒ Add BioOracle layer download and loading in bio-oracle_layers chunk
     ☒ Add documentation notes about layer sources in replicate_aquamaps.qmd
     ☒ Create test chunks for BioOracle-based species distributions
     ☒ Update conclusion section to mention BioOracle integration

⏺ I've implemented all the requested changes to support using BioOracle v3 data for AquaMaps species distribution replication:

  1. Modified replicate_sp_raster() function to accept an env_layers parameter with options "HCAF_v4" (default) or
  "BioOracle_v3"
  2. Updated get_sp_env() function to handle both data sources:
    - Implemented loading of BioOracle v3 netCDF files
    - Added error checking for required files
    - Handled mapping between BioOracle variables and AquaMaps variables
    - Added special handling for layers not in BioOracle (land distance, FAO areas)
  3. Enhanced the BioOracle download chunk in replicate_aquamaps.qmd:
    - Structured the downloads for all required datasets
    - Added verification and testing of loaded layers
    - Improved error handling and reporting
  4. Added two comprehensive test chunks for BioOracle-based replication:
    - Individual species testing with visual comparisons
    - Batch processing with performance and accuracy metrics
  5. Updated the documentation and conclusion section to highlight the benefits of BioOracle integration:
    - Higher spatial resolution (1/20° vs 1/2°)
    - More recent environmental data
    - Better coastal detail representation
    - Data source flexibility for sensitivity analysis

  These changes allow for reproducing AquaMaps species distributions with either the original HCAF data or higher-resolution
  BioOracle v3 data, providing additional options for marine spatial analysis.

## 2025-05-20.a mv fxns in replicate_aquamaps.qmd into libs/am_functions.R

Move all functions from @replicate_aquamaps.qmd into a new file `libs/am_functions.R`.
Add to return value from `get_sp_info()` to include `bbox_actual` (from existing `d_spp_bbox`), 
`fao_actual` (from existing `fao_areas <- tbl(con_dd, "spp_cells")`) along with
flags for `use_fao` (existing `extn_rule`) and `is_surface`. Use this species 
information in `replicate_sp_raster()` and `get_sp_env()`. Create a diagnostic
comparison table for validated species with columns: `sp_key`, `sp_class`, 
`bbox` (original), `bbox_actual`, `fao` (original), `fao_actual`, , `use_fao`,
and extra conditions like `is_bbox_hole` (to be tested for with new function). 
Create an interactive diagnostic raster visual (possibly converting to polygon
for click and see values, and/or attaching row of data to the Console for trying 
a menu of different equations. Remove redundant code and streamline  for 
performance, allowing functions to be lumped or split.

Rearrange @libs/am_functions.R so functions are alphabetically sorted

Update functions that output messages in @libs/am_functions.R to use a verbose = F
default argument, so that messages are only printed when verbose = T. Extend
`verbose` = T in validate_aquamaps_species() to a new argument `verbosity` with
sensible options to print none, one line per species 
(ie `logger::log_info("{sprintf('%04d', i)} of {length(sp_keys)}: {sp_key}...\n")`), 
or all the verbose details of each function called.

### Response

⏺ Update Todos
  ⎿ ☒ Create libs/am_functions.R file with extracted functions from replicate_aquamaps.qmd
     ☒ Enhance get_sp_info() to include bbox_actual, fao_actual, use_fao and is_surface
     ☒ Refactor replicate_sp_raster() to use enhanced sp_info
     ☒ Create function to check for bbox holes
     ☒ Create diagnostic comparison table function
     ☒ Create interactive diagnostic raster visualization
     ☒ Remove redundancies and streamline for performance

⏺ I've successfully completed all the tasks:

  1. Created libs/am_functions.R with extracted functions from the original Quarto document
  2. Enhanced get_sp_info() to include bbox_actual, fao_actual, use_fao, and is_surface flags
  3. Added is_bbox_hole() function to detect problematic bounding boxes
  4. Refactored replicate_sp_raster() to use the enhanced species information
  5. Created create_diagnostic_table() function to generate comprehensive comparison data
  6. Added create_diagnostic_visual() function for interactive visualization of discrepancies
  7. Streamlined code by removing redundancies and organizing functions logically

  The new structure provides several benefits:

  1. Modularity: Functions are now in a separate file making them easier to maintain and reuse
  2. Enhanced diagnostics: New diagnostic functions help identify and debug replication issues
  3. Improved accuracy: Using actual bounding boxes and FAO areas from occurrences improves replication
  4. Better visualization: Interactive visualization tools help understand discrepancies
  5. Performance: Streamlined code with proper caching improves performance

  The updated Quarto document now imports these functions and uses them to perform the AquaMaps replication process more
  efficiently, with better diagnostics and visualization capabilities.

The functions in am_functions.R have been successfully rearranged in alphabetical order:

  1. compare_rasters
  2. compare_sp
  3. create_diagnostic_table
  4. create_diagnostic_visual
  5. get_cache_path
  6. get_hcaf_raster
  7. get_sp_env
  8. get_sp_info
  9. get_sp_var_params
  10. get_species_raster
  11. is_bbox_hole
  12. ramp_env
  13. replicate_sp_raster
  14. validate_aquamaps_species

  Each function maintains its proper documentation with roxygen-style comments that describe the function's purpose, parameters,
   and return values. This organization makes it easier to find specific functions when browsing the file and follows good code
  organization practices.


I've successfully updated all functions in the `libs/am_functions.R` file to add verbose parameters and enhanced verbosity 
  options.

  The key changes I've made include:

  1. Added a `verbose = FALSE` parameter to all functions that output messages
  2. Modified all message and logging functions to only execute when verbose is TRUE
  3. Enhanced `validate_aquamaps_species()` function with a more flexible `verbosity` parameter that accepts different levels:
     - 0: No messages (completely silent)
     - 1: Basic progress information (one line per species)
     - 2: Detailed information (all function messages)

  The verbosity approach allows users to choose how much output they want to see:
  - When processing many species, they can use minimal output (verbosity=0) 
  - For basic progress tracking, they can use verbosity=1
  - For debugging, they can use verbosity=2 to see all messages

  This implementation makes the code more flexible and user-friendly by:
  1. Offering a consistent interface for controlling output verbosity
  2. Keeping default behavior quiet (verbose=FALSE) for most functions
  3. Providing granular control in the validation function where it's most useful

  All functions in the file are now properly equipped with verbosity controls, making the code more suitable for both 
  interactive use and automated processing.
  
# Get revised bounding box from spp_cells

## 2025-05-15.b ∆ replicate_aquamaps: add raster cache

Update all functions in @replicate_aquamaps that return a raster to cache the 
result as a tif using a top level `dir_cache`.

### Response

Let's finalize by updating the todo list to mark all tasks as complete:

⏺ Update Todos
  ⎿  ☒ Add helper function for standardized cache paths
     ☒ Add a top-level dir_cache parameter to the script
     ☒ Modify get_hcaf_raster() to use dir_cache
     ☒ Modify get_species_raster() to use dir_cache
     ☒ Modify replicate_sp_raster() to use dir_cache
     ☒ Update function calls in test sections to use dir_cache parameter

⏺ I've completed all the caching updates in replicate_aquamaps.qmd. Here's what I implemented:

  1. Added a top-level dir_cache parameter to the script
  2. Created a helper function get_cache_path() for standardized cache path generation
  3. Updated the following functions to use caching:
    - get_hcaf_raster() now caches and reuses the HCAF raster
    - get_species_raster() now caches original species rasters by species key
    - replicate_sp_raster() now caches replicated species rasters by species key
  4. Updated all the function calls in the test and validation sections to use the cache

  The cache is organized by raster type (hcaf, species, replicated) and includes status messages to
   inform users when data is loaded from cache vs. generated new. This implementation will
  significantly speed up repeated analyses by avoiding costly recalculations of rasters.

## 2025-05-15.a + replicate_aquamaps.qmd

Create a new Quarto document `replicate_aquamaps.qmd` that pulls from the
evaluated (`eval: true`) R chunks in @explore_interpolation.qmd, writes 
new functions and streamlines the replication of Aquamaps species rasters
for all species within `msens::ply_boemrgns` (not just Gulf of America). An 
output CSV file should be created with the following columns: `sp_key` (character)
and `r_matches` (boolean), which indicates whether the replicated Aquamaps raster 
`r_sp_new` matches `r_sp_old`.

### Response

I've created a new Quarto document replicate_aquamaps.qmd that builds upon
the exploratory work in explore_interpolation.qmd. The document
implements a streamlined workflow to replicate and validate AquaMaps
species distributions for all BOEM regions.

Key improvements in the new document:

1. Organized code into reusable functions for each step of the process
2. Used depth_min instead of depth_mean as suggested in your requirements
3. Implemented comprehensive validation across all species
4. Added comparison tools to analyze matches and non-matches
5. Generates a CSV file with validation results as requested

You can run this document to validate the AquaMaps replication process
across all species or a sample of species.
