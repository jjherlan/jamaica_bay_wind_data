# =============================================================================
# update_dashboard.R
# Jamaica Bay Wildlife Refuge - RM-0002 wind sensor
#
# 1. Rebuilds jamaica_bay_clean.csv from every raw jamaica_bay_YYYYMMDD.csv
# 2. If anything changed, commits the clean file + any NEW raw files and
#    pushes to GitHub, which triggers the Shinylive deploy to GitHub Pages.
#
# Run from VS Code (Ctrl+Shift+S / "Run Source") or from a terminal:
#   Rscript update_dashboard.R            # rebuild + commit + push
#   Rscript update_dashboard.R --no-push  # rebuild only (for testing)
# Base R only - no packages needed. Log: update_dashboard.log
# =============================================================================

# ---- Settings ---------------------------------------------------------------
DEVICE       <- "rm-0002"
# Set to e.g. "2025-09-18 12:00:42" to drop pre-installation (bench) data.
# NULL keeps everything, matching the dashboard's behaviour to date.
DEPLOY_START <- NULL
DIGITS       <- 3          # sensor values are float32; 3 decimals loses nothing real
TEMP_MIN     <- -100       # temperature below this = probe failure (e.g. -144.15)
PRES_MIN     <- 900        # pressure below this = probe failure (~780-824 mb)

# ---- Locate project folder (works for Rscript, Task Scheduler and VS Code) --
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
proj <- if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
setwd(proj)
push <- !("--no-push" %in% commandArgs(TRUE))

log_file <- file.path(proj, "update_dashboard.log")
logm <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(...))
  message(msg)
  cat(msg, "\n", file = log_file, append = TRUE, sep = "")
}

# ---- Helpers ----------------------------------------------------------------
clean_num <- function(v) {
  v <- trimws(v)
  v[is.na(v) | tolower(v) %in% c("", "na", "nan")] <- NA
  v <- gsub("[^0-9.-]", "", v)
  suppressWarnings(as.numeric(v))
}
parse_time <- function(x) {
  # UTC avoids DST gaps turning valid local readings into NA; only used for sorting
  t <- as.POSIXct(x, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
  miss <- is.na(t)
  t[miss] <- as.POSIXct(x[miss], format = "%m/%d/%Y %H:%M", tz = "UTC")
  t
}
fmt <- function(x) ifelse(is.na(x), "", as.character(round(x, DIGITS)))

# ---- Read raw files -----------------------------------------------------------
raw_files <- sort(list.files(proj, pattern = "^jamaica_bay_[0-9]{8}\\.csv$"))
if (!length(raw_files)) stop("No raw jamaica_bay_YYYYMMDD.csv files found in ", proj)

read_raw <- function(f) {
  lines <- sub("\r$", "", readLines(f, warn = FALSE))
  lines <- lines[startsWith(lines, DEVICE)]
  p <- strsplit(lines, ",", fixed = TRUE)
  g <- function(i) vapply(p, function(r) if (length(r) >= i) r[i] else NA_character_, "")
  d <- data.frame(received_at = g(3), local_time = trimws(g(4)), f_cnt = g(5),
                  dir = g(7), humidity = g(8), pressure = g(9), speed = g(10),
                  temperature = g(11), stringsAsFactors = FALSE)
  # Drop non-measurement rows (e.g. the 9/18/2025 field note "SETUP IN LIVING SHORELINE")
  d[grepl("^[0-9]+$", d$f_cnt), ]
}

raw <- do.call(rbind, lapply(raw_files, read_raw))
n_raw <- nrow(raw)

# Deduplicate on received_at (unique per uplink). local_time is NOT unique:
# overlapping downloads write the same reading as "12:00" and "12:00:32".
raw <- raw[!duplicated(raw$received_at), ]

for (v in c("dir", "humidity", "pressure", "speed", "temperature")) raw[[v]] <- clean_num(raw[[v]])
raw$temperature[!is.na(raw$temperature) & raw$temperature < TEMP_MIN] <- NA
raw$pressure[!is.na(raw$pressure) & raw$pressure < PRES_MIN] <- NA

raw$t <- parse_time(raw$local_time)
raw <- raw[!is.na(raw$t), ]
if (!is.null(DEPLOY_START)) raw <- raw[raw$t >= as.POSIXct(DEPLOY_START, tz = "UTC"), ]
raw <- raw[order(raw$t, raw$received_at), ]

out <- c("local_time,dir,humidity,pressure,speed,temperature",
         paste(raw$local_time, fmt(raw$dir), fmt(raw$humidity), fmt(raw$pressure),
               fmt(raw$speed), fmt(raw$temperature), sep = ","))

logm(sprintf("%d raw files, %d sensor rows -> %d unique observations (%s to %s); temp NA %d, pressure NA %d",
             length(raw_files), n_raw, nrow(raw), raw$local_time[1], raw$local_time[nrow(raw)],
             sum(is.na(raw$temperature)), sum(is.na(raw$pressure))))

clean_path <- file.path(proj, "jamaica_bay_clean.csv")
old <- if (file.exists(clean_path)) sub("\r$", "", readLines(clean_path, warn = FALSE)) else character()
changed <- !identical(old, out)
if (changed) {
  con <- file(clean_path, open = "wb")   # "wb" = LF line endings on Windows too
  writeLines(out, con); close(con)
  logm("jamaica_bay_clean.csv updated")
} else {
  logm("jamaica_bay_clean.csv unchanged")
}

# ---- Commit + push ------------------------------------------------------------
if (!push) { logm("--no-push: skipping git"); quit(save = "no") }

git <- function(...) {
  res <- suppressWarnings(system2("git", c("-C", shQuote(proj), ...), stdout = TRUE, stderr = TRUE))
  status <- attr(res, "status"); if (is.null(status)) status <- 0
  list(out = res, ok = status == 0)
}
if (!git("rev-parse", "--is-inside-work-tree")$ok) stop("git not found or folder is not a git repo")

new_raw <- git("ls-files", "--others", "--exclude-standard", "--", "jamaica_bay_2*.csv")$out
to_add <- unique(c("jamaica_bay_clean.csv", new_raw))
git("add", "--", to_add)

if (git("diff", "--cached", "--quiet")$ok) {
  logm("Nothing to commit; dashboard already up to date")
} else {
  msg <- sprintf("Data update %s: %d observations through %s", format(Sys.Date()),
                 nrow(raw), raw$local_time[nrow(raw)])
  cm <- git("commit", "-m", shQuote(msg))
  if (!cm$ok) stop("git commit failed:\n", paste(cm$out, collapse = "\n"))
  logm("Committed: ", msg, if (length(new_raw)) paste0(" (new raw: ", paste(new_raw, collapse = ", "), ")") else "")
  ps <- git("push", "origin", "HEAD")
  if (!ps$ok) { logm("PUSH FAILED: ", paste(ps$out, collapse = " | ")); stop("git push failed - see log") }
  logm("Pushed. GitHub Pages will redeploy in ~5 min: https://jjherlan.github.io/jamaica_bay_wind_data/")
}
