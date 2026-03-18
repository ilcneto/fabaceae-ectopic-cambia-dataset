# This script generates a summary figure for the Fabaceae Ectopic Cambia Dataset
# Output: figures/taxonomic_coverage_Fabaceae.png
# Code developed by Israel L. Cunha-Neto, with assistance from ChatGPT (OpenAI).

library(ggplot2)
library(readr)
library(scales) 

# Path to CSV 
csv_path <- file.path("data", "Dataset.csv")
if (!file.exists(csv_path)) stop(paste("CSV not found at", csv_path))

# Read table
taxa <- read_csv(csv_path, show_col_types = FALSE)
names(taxa) <- trimws(names(taxa))

cat("CSV read successfully. Rows:", nrow(taxa), "Columns:", ncol(taxa), "\n")
cat("Columns:", paste(colnames(taxa), collapse = ", "), "\n")

# Summaries (unique counts)
subfamily_count <- n_distinct(taxa$Subfamily, na.rm = TRUE)
genus_count     <- n_distinct(taxa$Genus,     na.rm = TRUE)
species_count   <- n_distinct(taxa$Species,   na.rm = TRUE)

cat("Subfamilies:", subfamily_count,
    "Genera:", genus_count,
    "Species:", species_count, "\n")

# Build summary data frame for plotting
# Display labels are exactly "Subfamily", "Genera", "Species"
summary_df <- tibble(
  level = factor(c("Subfamily", "Genera", "Species"),
                 levels = c("Species", "Genera", "Subfamily")),  # forces order
  count = c(subfamily_count, genus_count, species_count)
)

# Plot
p <- ggplot(summary_df, aes(y = level, x = count)) +
  geom_col(fill = "grey70") +
  geom_text(aes(label = comma(count)), hjust = -0.15, size = 5) +
  labs(
    x = "Number represented in the database",
    y = NULL,
    title = "Current taxonomic coverage (Ectopic Cambia, Fabaceae)"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.10)), labels = comma) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# Save image
output_path <- file.path("summary", "taxonomic_coverage_Fabaceae.png")

ggsave(filename = out_path, plot = p, width = 7, height = 4, dpi = 300)

cat("Figure saved successfully at:\n", out_path, "\n")
``
