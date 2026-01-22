# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
# dplyr is used for dataframe manipulation
library("dplyr")

# ------------------------------------------------------------
# Input data
# ------------------------------------------------------------
# sys_df: full systematic review dataset built with bibliometrix
# dict_df: curated/custom dataset containing model-related annotations
sys_df  <- read.csv("oriented-ship-detection/science-mapping/full_biblio.csv")
dict_df <- read.csv("oriented-ship-detection/science-mapping/df-full.csv")

# ------------------------------------------------------------
# Text normalization function
# ------------------------------------------------------------
# Purpose:
#   - Standardize text to enable reliable rule-based matching
#   - Remove formatting variability (case, punctuation, separators)
#
# Normalization rules:
#   - lowercase
#   - replace separators (-, _, /) with spaces
#   - remove punctuation
#   - collapse multiple spaces
#   - trim leading/trailing whitespace
normalize_text <- function(x) {
  x <- tolower(x)
  x <- gsub("[\\-_\\/]", " ", x)      # normalize separators
  x <- gsub("[^a-z0-9\\s]", " ", x)   # remove punctuation
  x <- gsub("\\s+", " ", x)           # collapse spaces
  trimws(x)
}

# ------------------------------------------------------------
# Dictionary preparation (alias construction + normalization)
# ------------------------------------------------------------
# Goal:
#   - Prepare a normalized alias space from curated annotations
#   - Support multiple surface forms for the same architecture
#   - Enable version-aware YOLO handling
#
# Steps:
#   1. Copy ModelBase into canonical_arch and alias columns
#   2. Expand YOLO aliases (e.g., "YOLOv5" -> "YOLOv5 | YOLO v5 | YOLO-v5")
#   3. Split expanded aliases into separate rows
#   4. Normalize aliases
#   5. Order by decreasing length to prioritize specific matches
dict_df <- dict_df |>
  mutate(
    canonical_arch = ModelBase,
    alias = ModelBase
  ) |>
  mutate(
    alias = ifelse(
      grepl("^YOLOv[0-9]+$", alias, ignore.case = TRUE),
      paste0(
        alias, "|",
        gsub("v", " v", alias, ignore.case = TRUE), "|",
        gsub("v", "-v", alias, ignore.case = TRUE)
      ),
      alias
    )
  ) |>
  tidyr::separate_rows(alias, sep = "\\|") |>
  mutate(alias_norm = normalize_text(alias)) |>
  arrange(desc(nchar(alias_norm)))

# ------------------------------------------------------------
# Alias vector extraction
# ------------------------------------------------------------
# Purpose:
#   - Provide a clean character vector to induce the architecture dictionary
alias_norm_vec <- dict_df$alias_norm

# ------------------------------------------------------------
# Core-architecture dictionary induction function
# ------------------------------------------------------------
# Purpose:
#   - Build a controlled vocabulary of core detection architectures
#   - Tokenize normalized aliases
#   - Retain only valid architecture tokens using explicit regex rules
#
# Design principles:
#   - Atomic entries (one token = one architecture)
#   - Version-aware YOLO handling (YOLOv3 ≠ YOLOv5)
#   - Explicit inclusion list to avoid paradigm/backbone leakage
build_arch_dictionary_from_alias <- function(alias_norm_vec) {

  # Step 1: Tokenize normalized alias strings by whitespace
  tokens <- strsplit(alias_norm_vec, "\\s+")
  tokens <- unlist(tokens)

  # Step 2: Define valid architecture patterns
  # Note:
  #   - YOLO versions are captured explicitly
  #   - YOLOX is treated as a distinct architecture
  #   - Only architectures (not paradigms or backbones) are included
  arch_regex <- paste(
    c(
      "^yolov[0-9]+[a-z]*$",  # YOLO versions (e.g., yolov5, yolov8n)
      "^yolox$",              # YOLOX
      "^fasterrcnn$",
      "^maskrcnn$",
      "^cascadercnn$",
      "^retinanet$",
      "^centernet$",
      "^centerpoint$",
      "^vfnet$",
      "^ssd$",
      "^fcos$",
      "^rtmdet$",
      "^orientedrcnn$",
      "^s2anet$",
      "^redet$",
      "^rrcnn$",
      "^r3det$",
      "^p2net$",
      "^gwd$",
      "^detr$",
      "^unet$"
    ),
    collapse = "|"
  )

  # Step 3: Filter tokens to keep only valid architectures
  arch_tokens <- tokens[grepl(arch_regex, tokens)]

  # Step 4: Return unique architecture names
  unique(arch_tokens)
}

# ------------------------------------------------------------
# Dictionary construction
# ------------------------------------------------------------
# Build the frozen core-architecture dictionary
arch_atomic <- build_arch_dictionary_from_alias(dict_df$alias_norm)


arch_dict <- tibble(
  pattern   = arch_atomic,
  canonical = arch_atomic
)

arch_cnn <- tibble::tribble(
  ~pattern,        ~canonical,
  "faster r cnn",  "fasterrcnn",
  "fast r cnn",    "fasterrcnn",
  "mask r cnn",    "maskrcnn",
  "cascade r cnn", "cascadercnn",
  "r cnn",         "rcnn"
)

arch_dict <- bind_rows(arch_dict, arch_cnn) |>
  arrange(desc(nchar(pattern)))


# ------------------------------------------------------------
# Dictionary freeze (persistence)
# ------------------------------------------------------------
# Save the finalized architecture dictionary as a standalone artifact
# This file should NOT be overwritten during extraction
saveRDS(arch_dict, "oriented-ship-detection/science-mapping/arch_dict_core_architectures.rds")
