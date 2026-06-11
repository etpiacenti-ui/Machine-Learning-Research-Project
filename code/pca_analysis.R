
# 1. Load dataset

clean <- read.csv("biomed_clean.csv")
head(clean)
str(clean)

# 2. Select only numeric columns for PCA
pca_data <- clean[, c("year",
                      "n_authors",
                      "title_length",
                      "referencecount",
                      "citationcount",
                      "influentialcitationcount")]
pca_scaled <- scale(pca_data)

# 3. PCA

pca <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)
summary(pca)

# 4. Visualizations

print(plot(pca, type = "l", main = "Scree Plot")) # Scree plot

pc_df <- data.frame(pca$x[,1:2]) # PC1 vs PC2 scatterplot
print(
  plot(pc_df$PC1, pc_df$PC2,
     xlab = "PC1",
     ylab = "PC2",
     main = "PCA: PC1 vs PC2",
     pch = 19,
     col = rgb(0,0,1,0.3))
)

