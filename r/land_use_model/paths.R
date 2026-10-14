#################################
# paths.R
# Input and output roots for this repo's land-use model runs (not part of upstream)
#################################
#
# input_wdir: the land-based-solutions Nextcloud folder holding the model inputs (1_input/).
#   Treated as READ-ONLY. The Nextcloud desktop client syncs it, so writing there could
#   overwrite the land-based-solutions team's own files of the same name.
# wdir: root for everything the pipeline writes (2_processing/, 3_output/). A git-ignored
#   folder in this repo.
#
# Every script that writes outputs sources this file instead of defining its own wdir.

source(here::here("r/directories.R"))
input_wdir <- glue::glue("{data_directory}/parallelized")
wdir       <- here::here("land_use_model_runs")

# Refuse to run if the output root is inside a Nextcloud folder (e.g. this repo cloned into one)
# or inside the input folder. The parent is normalized because wdir may not exist yet.
wdir_resolved  <- tolower(file.path(normalizePath(dirname(wdir)), basename(wdir)))
input_resolved <- tolower(normalizePath(input_wdir, mustWork = FALSE))
if (grepl("nextcloud", wdir_resolved) || startsWith(wdir_resolved, input_resolved)) {
  stop("Output root '", wdir, "' is inside a Nextcloud or input folder; refusing to write there. ",
       "Set wdir in r/land_use_model/paths.R to a local, unsynced folder.")
}
rm(wdir_resolved, input_resolved)
