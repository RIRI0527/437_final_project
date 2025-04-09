## ----load-and-clean-data, message=FALSE, warning=FALSE--------------------------------------------------------
# Loading Required Packages
library(here)
library(dplyr)
library(mclust)
library(ggplot2)
library(knitr)
library(kableExtra)
library(psych)
library(tidyr)
library(gridExtra)
library(corrplot)

# Load the RData
load(here("data/CDS_data.RData"))  # assumes your file is stored in /data

# Check NAs
# anyNA(data)

# Aggregate to 1 row per company
cds_avg <- data %>%
  group_by(Company) %>%
  summarise(across(starts_with("PX"), mean, na.rm = TRUE)) %>%
  ungroup()

# Log-transform and scale PX1 to PX10
cds_transformed <- cds_avg %>%
  mutate(across(starts_with("PX"), ~ log1p(.))) %>%
  mutate(across(starts_with("PX"), scale))


## ----tbl-data-summary, warning=FALSE, message=FALSE-----------------------------------------------------------
#| tbl-cap: Summary Statistics of CDS Spread Columns (PX1–PX10)

# Create summary table
px_summary <- describe(data %>% select(starts_with("PX")))

# Show as nicely formatted table
kable(px_summary, digits = 2)


## ----fig-box-data---------------------------------------------------------------------------------------------
#| fig-cap: Distribution of CDS Spreads by Maturity

# Reshape the data to long formats
data_long <- data %>%
  pivot_longer(cols = starts_with("PX"), names_to = "Tenor", values_to = "Spread")

# Boxplot of spreads by tenor
ggplot(data_long, aes(x = Tenor, y = Spread)) +
  geom_boxplot(outlier.alpha = 0.2, fill = "lightblue") +
  scale_y_log10() + 
  labs(y = "CDS Spread (log scale)",
       x = "Tenor (PX1–PX10)") +
  theme_minimal()


## ----fig-cor-data, warning=FALSE, message=FALSE---------------------------------------------------------------
#| fig-cap: Correlation Between CDS Spread Tenors

px_corr <- cor(data %>% select(starts_with("PX")), use = "pairwise.complete.obs")

corrplot(px_corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8, mar = c(0, 0, 1, 0))


## ----fit-model, output=FALSE----------------------------------------------------------------------------------
# Fit Gaussian Mixture Model (GMM)
gmm_model <- Mclust(select(cds_transformed, starts_with("PX")))

# Add cluster labels to the data
cds_transformed$Cluster <- as.factor(gmm_model$classification)
cds_avg$Cluster <- cds_transformed$Cluster  # attach to company names


## ----tbl-model-sum--------------------------------------------------------------------------------------------
#| tbl-cap: GMM Coefficient Summary Table
# 1. Cluster means (mu_k) — PX1 to PX10 per cluster
cluster_means <- as.data.frame(t(gmm_model$parameters$mean))
cluster_means$Cluster <- rownames(cluster_means)

# 2. Mixing proportions
cluster_means$Proportion <- round(gmm_model$parameters$pro, 3)

# 3. Reorder columns
cluster_means <- cluster_means %>%
  relocate(Cluster, Proportion)

# 4. Format as nice table
kable(cluster_means, caption = "GMM Cluster Means and Mixing Proportions",
      digits = 3, format = "html") %>%
  kable_styling(full_width = FALSE, position = "center")


## ----tbl-cluster-summary--------------------------------------------------------------------------------------
#| tbl-cap: Number of Companies per GMM Cluster
# Create a summary table as a data frame
cluster_summary <- as.data.frame(table(cds_avg$Cluster))

# Rename columns for clarity
colnames(cluster_summary) <- c("Cluster", "Number_of_Companies")

# Display as kable
kable(cluster_summary, caption = "Number of Companies per Cluster")


## ----fig-cluster----------------------------------------------------------------------------------------------
#| fig-cap: Clusters of CDS Spread Curves
#|
# Visualize PX1 vs PX10 colored by cluster
ggplot(cds_transformed, aes(x = PX1, y = PX10, color = Cluster)) +
  stat_ellipse(geom = "polygon", alpha = 0.1, aes(fill = Cluster), show.legend = FALSE) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Short-Term Spread (PX1)",
    y = "Long-Term Spread (PX10)"
  ) +
  theme_minimal()


## ----fig-trend------------------------------------------------------------------------------------------------
#| fig-cap: CDS Term Structure by Cluster

# Reshape data for plotting
library(tidyr)
library(dplyr)
library(ggplot2)

# Attach cluster labels to original PX values
cds_plot_data <- cds_transformed %>%
  select(starts_with("PX")) %>%
  mutate(Cluster = cds_transformed$Cluster,
         Company = cds_avg$Company) %>%
  pivot_longer(cols = starts_with("PX"), names_to = "Tenor", values_to = "Spread")

# Plot average PX curve per cluster
cds_plot_data %>%
  group_by(Cluster, Tenor) %>%
  summarise(AvgSpread = mean(Spread), .groups = "drop") %>%
  ggplot(aes(x = Tenor, y = AvgSpread, color = Cluster, group = Cluster)) +
  geom_line(size = 1.2) +
  labs(x = "Tenor (PX1–PX10)",
       y = "Average Transformed Spread") +
  theme_minimal()


## ----fig-boxplot----------------------------------------------------------------------------------------------
#| fig-cap: Boxplot of Average CDS Spread by Cluster
# Compute overall average CDS spread per company (across PX1–PX10)
cds_avg$MeanSpread <- cds_avg %>%
  select(starts_with("PX")) %>%
  rowMeans()

# Boxplot: distribution of risk levels per cluster
ggplot(cds_avg, aes(x = Cluster, y = MeanSpread, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) +
  labs(y = "Average Spread (PX1–PX10)",
       x = "Cluster") +
  theme_minimal()


## ----fig-year-plot,warning=FALSE, message=FALSE---------------------------------------------------------------
#| fig-cap: Short-Term Risk (PX1) by Cluster Over Time

# 1. Combine original CDS `data` with cluster labels
data_with_cluster <- data %>%
  group_by(Company) %>%
  mutate(Cluster = cds_transformed$Cluster[match(Company, cds_avg$Company)]) %>%
  ungroup()

# 2. Convert Date to Date type
data_with_cluster$Date <- as.Date(data_with_cluster$Date)

# 3. Aggregate: get average PX1 (short-term risk) by Date and Cluster
cds_time_series <- data_with_cluster %>%
  group_by(Date, Cluster) %>%
  summarise(AvgPX1 = mean(PX1, na.rm = TRUE)) %>%
  ungroup()

# 4. Plot time series
ggplot(cds_time_series, aes(x = Date, y = AvgPX1, color = Cluster)) +
  geom_line(alpha = 0.8, size = 1) +
  labs(title = "Short-Term Risk (PX1) by Cluster Over Time",
       x = "Date",
       y = "Average PX1 (Short-Term CDS Spread)") +
  theme_minimal()


## ----tbl-top-5-companies, warning=FALSE-----------------------------------------------------------------------
#| tbl-cap: Top 5 Companies from Each Cluster

# View top 5 companies from each cluster as a kable table
cds_avg %>%
  group_by(Cluster) %>%
  slice_head(n = 5) %>%
  select(Company, Cluster, PX1, PX10) %>%
  kable()


## ----fig-BIC--------------------------------------------------------------------------------------------------
#| fig-cap: Visualize the BIC Values For Each K

# Visualize the BIC values for each k
plot(gmm_model, what = "BIC")


## ----appendix-save, message=FALSE, warning=FALSE, echo=FALSE, results='hide'----------------------------------
# Save the code file silently without showing output
dummy <- knitr::purl("437_final_project.qmd", output = "appendix_code.R")


## ----echo=FALSE, results='asis'-------------------------------------------------------------------------------
knitr::asis_output(
  paste(c("```", gsub("#", "\\\\#", readLines("appendix_code.R")), "```"), collapse = "\n")
)

