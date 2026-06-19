# 06_snapshot.R
# Run after each matchday to snapshot the current progression probabilities.
# Appends to a running snapshots.rds so you can plot how win probabilities
# evolved across the tournament. Run this TODAY before group stage ends.

library(tidyverse)
library(lubridate)

SNAPSHOT_FILE <- "data/processed/snapshots.rds"

progression <- readRDS("data/processed/progression.rds")

snapshot <- progression %>%
  mutate(snapshot_date  = Sys.Date(),
         snapshot_label = format(Sys.Date(), "%b %d"))

# append to running file, or create it if first run
if (file.exists(SNAPSHOT_FILE)) {
  existing <- readRDS(SNAPSHOT_FILE)
  # avoid duplicate snapshots on same day
  existing <- existing %>% filter(snapshot_date != Sys.Date())
  snapshots <- bind_rows(existing, snapshot)
} else {
  snapshots <- snapshot
}

saveRDS(snapshots, SNAPSHOT_FILE)

dates <- sort(unique(snapshots$snapshot_date))
cat(sprintf("Snapshots saved: %d total across %d dates\n",
            nrow(snapshots), length(dates)))
cat(sprintf("Dates: %s\n", paste(dates, collapse = ", ")))
cat(sprintf("Teams tracked: %d\n", n_distinct(snapshots$team)))