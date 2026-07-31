#!/usr/bin/env Rscript
#################################
# run_pipeline.R
# "Run the parallelized LBCS analysis pipeline end-to-end"
# author: "Robert Heilmayr (emLab)"
#################################
#
# PURPOSE:
# Runs the analysis stages below IN ORDER, each in its OWN fresh Rscript subprocess, with
# timestamped logging and stop-on-first-failure. A separate process per stage matters: 02 and 03
# spin up future::multisession workers and hold multi-GB data, so running them in one shared
# session would accumulate memory and risk cross-stage interference. One subprocess per stage
# guarantees a clean slate each time.
#
# USAGE:
#   Rscript r/land_use_model/run_pipeline.R              # run all stages
#   Rscript r/land_use_model/run_pipeline.R 03 06        # run only stages 03 and 06
#   Rscript r/land_use_model/run_pipeline.R 03_aggregate_results.R   # by name (prefix ok)
#
# Per-stage console output is written live to logs/pipeline_<timestamp>/<stage>.log
# (tail -f that file to watch a stage); a summary is written to .../summary.log and printed at the end.
#
# NOTE: the upstream setup scripts 00_generate_sample_data.R and 00c_run_cobweb.R are NOT included
# here -- they regenerate inputs and are run manually when needed. Edit STAGES to add/skip stages,
# or pass stage selectors as arguments (see USAGE).

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Configuration -----

# Ordered list of pipeline stages (data -> results -> figures). Comment out any you want to skip
# (e.g. if the chunks are already built and you only need to re-aggregate, run with args "03" "06").
STAGES <- c(
  "01_prepare_data.R",         # build chunk inputs (fixed effects, revenues) from raw data
  "02_run_simulations.R",      # simulate all scenarios over every chunk           (LONG)
  "03_aggregate_results.R",    # aggregate per-pixel PV metrics -> sim_results.csv  (parallel)
  "04_analyze_results.R",      # downstream analysis of aggregated results
  "05_validate_results.R",     # logic / consistency checks
  "06_figures_tables.R",       # Table 2 + paper figures
  "07_leakage_decomposition.R" # leakage decomposition (SI)
)

pipeline_dir <- here::here("r/land_use_model")
rscript_bin  <- file.path(R.home("bin"), "Rscript")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Optional stage selection from command-line args -----
# Each arg may be a stage number ("03"), a full file name, or a unique prefix. Order of execution
# always follows STAGES (not the argument order), so dependencies are never run out of order.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  pick <- function(sel) {
    hit <- STAGES[startsWith(STAGES, sel) | startsWith(sub("_.*", "", STAGES), sel) | STAGES == sel]
    if (length(hit) == 0) stop("No stage matches selector: '", sel, "'")
    hit
  }
  selected <- unique(unlist(lapply(args, pick)))
  STAGES <- STAGES[STAGES %in% selected]   # keep canonical order
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run -----

stamp    <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_dir  <- file.path(pipeline_dir, "logs", paste0("pipeline_", stamp))
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
summary_log <- file.path(log_dir, "summary.log")

say <- function(...) {
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
  cat(line, "\n", sep = "")
  cat(line, "\n", sep = "", file = summary_log, append = TRUE)
}

say("===== PIPELINE START =====")
say("Stages: ", paste(STAGES, collapse = " -> "))
say("Logs:   ", log_dir)

timings    <- list()
overall_ok <- TRUE

for (s in STAGES) {
  script <- file.path(pipeline_dir, s)
  if (!file.exists(script)) { say("MISSING (skipped): ", s); next }

  stage_log <- file.path(log_dir, sub("\\.R$", ".log", s))
  say("----- START ", s, "  (live log: ", stage_log, ") -----")
  t0 <- Sys.time()
  rc <- system2(rscript_bin, args = shQuote(script),
                stdout = stage_log, stderr = stage_log, wait = TRUE)
  secs   <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  status <- if (identical(rc, 0L)) "OK" else paste0("FAIL(rc=", rc, ")")
  timings[[s]] <- data.frame(stage = s, seconds = secs, status = status)
  say("----- END   ", s, "  ", status, "  (", secs, "s) -----")

  if (!identical(rc, 0L)) {
    overall_ok <- FALSE
    say("STOPPING: '", s, "' failed -- see ", stage_log)
    break
  }
}

say("===== PIPELINE ", if (overall_ok) "COMPLETE" else "FAILED", " =====")
summary_tbl <- do.call(rbind, timings)
if (!is.null(summary_tbl)) {
  cat("\nStage timing summary:\n")
  print(summary_tbl, row.names = FALSE)
  suppressWarnings(write.csv(summary_tbl, file.path(log_dir, "timings.csv"), row.names = FALSE))
}

quit(status = if (overall_ok) 0L else 1L)
