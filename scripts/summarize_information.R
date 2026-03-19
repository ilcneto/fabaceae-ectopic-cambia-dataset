# This script generates summary information for the Fabaceae Ectopic Cambia Dataset
# Output: summary/taxonomic_coverage.csv
# Code developed by Israel L. Cunha-Neto, with assistance from ChatGPT (OpenAI).

library(dplyr)
library(readr)

# Path to CSV 
csv_path <- file.path("data", "Dataset.csv")
if (!file.exists(csv_path)) stop(paste("CSV not found at", csv_path))

# Read + clean 
taxa <- read_csv(csv_path, show_col_types = FALSE)

# Trim whitespace in headers and character cells
names(taxa) <- trimws(names(taxa))
taxa <- taxa %>%
  mutate(across(everything(), ~ if (is.character(.x)) trimws(.x) else .x))

cat("Rows:", nrow(taxa), "Columns:", ncol(taxa), "\n")

# Build Biogeography list (sorted)
biogeo_list <- taxa %>%
  distinct(Biogeography) %>%
  filter(!is.na(Biogeography), Biogeography != "") %>%
  arrange(Biogeography) %>%
  pull(Biogeography)

# Totals (Genus, Species, Biogeography list)
totals_long <- tibble(
  Field   = c("Genus", "Species", "Biogeography"),
  Level   = c(NA, NA, "Unique list (alphabetical)"),
  Value   = c(
    as.character(n_distinct(taxa$Genus,    na.rm = TRUE)),
    as.character(n_distinct(taxa$Species,  na.rm = TRUE)),
    paste(biogeo_list, collapse = ", ")
  )
)

# Frequency tables (Morphotype, Habit, Organ)
morphotype_counts <- taxa %>%
  filter(!is.na(Morphotype), Morphotype != "") %>%
  count(Morphotype, name = "Count") %>%
  arrange(desc(Count), Morphotype) %>%
  transmute(
    Field   = "Morphotype",
    Level   = Morphotype,
    Value   = as.character(Count)
  )

habit_counts <- taxa %>%
  filter(!is.na(Habit), Habit != "") %>%
  count(Habit, name = "Count") %>%
  arrange(desc(Count), Habit) %>%
  transmute(
    Field   = "Habit",
    Level   = Habit,
    Value   = as.character(Count)
  )

organ_counts <- taxa %>%
  filter(!is.na(Organ), Organ != "") %>%
  count(Organ, name = "Count") %>%
  arrange(desc(Count), Organ) %>%
  transmute(
    Field   = "Organ",
    Level   = Organ,
    Value   = as.character(Count)
  )

# Collate everything into one table 
table <- bind_rows(
  totals_long,
  morphotype_counts,
  habit_counts,
  organ_counts
)

cat("\nTABLE OUTPUT (first rows)\n")
print(head(table, 12))

# Save CSV
output_path <- file.path("summary", "summary_information.csv")

write_csv(table, output_path)
cat("\nAll results saved to:", output_path, "\n")
``
