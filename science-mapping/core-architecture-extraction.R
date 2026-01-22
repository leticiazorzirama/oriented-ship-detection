# ------------------------------------------------------------
# Input data
# ------------------------------------------------------------
# sys_df: full systematic review dataset built with bibliometrix
# dict_df: curated/custom dataset containing model-related annotations
sys_df  <- read.csv("oriented-ship-detection/science-mapping/full_biblio.csv")
dict_df <- read.csv("oriented-ship-detection/science-mapping/df-full.csv")

# ------------------------------------------------------------
# Load frozen core-architecture dictionary
# ------------------------------------------------------------
arch_dict <- readRDS("oriented-ship-detection/science-mapping/arch_dict_core_architectures.rds")

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
normalize_text_sys <- function(x) {
  x <- tolower(x)

  # ---- Canonicalize CNN-based detector names ----
  x <- gsub("faster\\s*r\\s*cnn", " fasterrcnn ", x)
  x <- gsub("mask\\s*r\\s*cnn", " maskrcnn ", x)
  x <- gsub("cascade\\s*r\\s*cnn", " cascadercnn ", x)
  x <- gsub("r\\s*cnn", " rcnn ", x)

  # RetinaNet variants
  x <- gsub("retina\\s*net", " retinanet ", x)

  # CenterNet / CenterPoint
  x <- gsub("center\\s*net", " centernet ", x)
  x <- gsub("center\\s*point", " centerpoint ", x)

  # ---- YOLO variants (keep versions) ----
  x <- gsub("yolo\\s*v\\s*([0-9]+[a-z]*)", " yolov\\1 ", x)

  # ---- General cleanup ----
  x <- gsub("[\\-_\\/]", " ", x)
  x <- gsub("[^a-z0-9\\s]", " ", x)
  x <- gsub("\\s+", " ", x)

  trimws(x)
}

# ------------------------------------------------------------
# Normalize text fields used for matching
# ------------------------------------------------------------
sys_df <- sys_df |>
  mutate(
    title_norm    = normalize_text(TI),
    abstract_norm = normalize_text(AB),
    keywords_norm = normalize_text(KW_Merged)
  )

# ------------------------------------------------------------
# Architecture matching function (single document)
# ------------------------------------------------------------
extract_core_arch <- function(title, abstract, keywords, arch_dict) {

  # helper: return first matching architecture
  match_arch <- function(text) {
    hits <- arch_dict[sapply(arch_dict, function(a) grepl(paste0("\\b", a, "\\b"), text))]
    if (length(hits) > 0) hits[1] else NA
  }

  # 1. Title
  arch <- match_arch(title)
  if (!is.na(arch)) {
    return(list(core_arch = arch, evidence = "title"))
  }

  # 2. Abstract
  arch <- match_arch(abstract)
  if (!is.na(arch)) {
    return(list(core_arch = arch, evidence = "abstract"))
  }

  # 3. Keywords
  arch <- match_arch(keywords)
  if (!is.na(arch)) {
    return(list(core_arch = arch, evidence = "keywords"))
  }

  # 4. No match
  list(core_arch = NA, evidence = NA)
}

# ------------------------------------------------------------
# Initialize output columns
# ------------------------------------------------------------
sys_df$core_arch <- NA_character_
sys_df$core_arch_evidence <- NA_character_

# ------------------------------------------------------------
# Main extraction loop
# ------------------------------------------------------------

for (i in seq_len(nrow(sys_df))) {

  fields <- list(
    title    = normalize_text(sys_df$TI[i]),
    abstract = normalize_text(sys_df$AB[i]),
    keywords = normalize_text(sys_df$DE[i])
  )

  found <- FALSE

  for (field_name in names(fields)) {
    txt <- fields[[field_name]]

    if (is.na(txt) || txt == "") next

    for (j in seq_len(nrow(arch_dict))) {
      pat <- paste0("\\b", arch_dict$pattern[j], "\\b")

      if (grepl(pat, txt)) {
        sys_df$core_arch[i] <- arch_dict$canonical[j]
        sys_df$core_arch_evidence[i] <- field_name
        found <- TRUE
        break
      }
    }

    if (found) break
  }
}

# How many papers were classified?
table(is.na(sys_df$core_arch))

# Distribution of architectures
sort(table(sys_df$core_arch), decreasing = TRUE)

# Evidence source distribution
table(sys_df$core_arch_evidence)
