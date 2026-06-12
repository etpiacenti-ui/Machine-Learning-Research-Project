
library(tidyverse)
library(cluster)
library(factoextra)
library(proxy)

# 1. Loading data

data <- read.csv("biomed_clean.csv")
numeric_data <- data %>%
  select(where(is.numeric)) %>%
  na.omit()

# 2. Transforming skewed variables
numeric_data <- numeric_data %>%
  mutate(
    referencecount          = log1p(referencecount),
    citationcount           = log1p(citationcount),
    influentialcitationcount = log1p(influentialcitationcount)
  )

scaled_data <- scale(numeric_data)

cat("Full dataset dimensions:\n")
print(dim(scaled_data))

# 3. Creating analysis samples

set.seed(123)

# Sample for k-means 
kmeans_idx <- sample(
  1:nrow(scaled_data),
  size = min(10000, nrow(scaled_data))
)
kmeans_data <- scaled_data[kmeans_idx, ]

# Sample for silhouette method
sil_idx <- sample(
  1:nrow(scaled_data),
  size = min(1000, nrow(scaled_data))
)
sil_data <- scaled_data[sil_idx, ]

# Sample for heirarchical clustering
hc_idx <- sample(
  1:nrow(scaled_data),
  size = min(1000, nrow(scaled_data))
)
hc_data <- scaled_data[hc_idx, ]

cat("\nSamples created:\n")
cat("KMeans:", nrow(kmeans_data), "\n")
cat("Silhouette:", nrow(sil_data), "\n")
cat("Hierarchical:", nrow(hc_data), "\n")

# 4. Silhouette method for best k

sil_width <- numeric(10)

for(k in 2:10){
  
  km <- kmeans(
    sil_data,
    centers = k,
    nstart = 25
  )
  
  sil <- silhouette(
    km$cluster,
    dist(sil_data)
  )
  
  sil_width[k] <- mean(sil[,3])
}

plot(
  2:10,
  sil_width[2:10],
  type = "b",
  pch = 19,
  xlab = "Number of Clusters (k)",
  ylab = "Average Silhouette Width",
  main = "Silhouette Method (Sampled Data)"
)

best_k <- which.max(sil_width)

cat("\nOptimal k:", best_k, "\n")

# 5. k-means clustering

for(k in c(2,3,4)){
  
  km <- kmeans(
    kmeans_data,
    centers = k,
    nstart = 25
  )
  
  print(
    fviz_cluster(
      km,
      data = kmeans_data,
      geom = "point"
    ) +
      ggtitle(paste("K-Means Clustering (k =", k, ")"))
  )
}

# Final KMeans model with best k
km_final <- kmeans(
  kmeans_data,
  centers = best_k,
  nstart = 25
)

print(
  fviz_cluster(
    km_final,
    data = kmeans_data,
    geom = "point"
  ) +
    ggtitle(paste("Final K-Means (k =", best_k, ")"))
)

# 6. Heirarchical clustering

cos_dist <- proxy::dist(
  hc_data,
  method = "cosine"
)

hc <- hclust(
  cos_dist,
  method = "average"
)

plot(
  hc,
  main = "Hierarchical Clustering Dendrogram (Sampled)",
  cex = 0.5
)

rect.hclust(
  hc,
  k = best_k,
  border = "red"
)

hc_clusters <- cutree(
  hc,
  k = best_k
)

print(
  fviz_cluster(
    list(data = hc_data, cluster = hc_clusters),
    geom = "point"
  ) +
    ggtitle(paste("Hierarchical Clustering (k =", best_k, ")"))
)

# 7. Comparing k-means vs heriarchical

km_compare <- kmeans(
  hc_data,
  centers = best_k,
  nstart = 25
)

comparison <- table(
  KMeans = km_compare$cluster,
  Hierarchical = hc_clusters
)

cat("\nCluster Comparison:\n")
print(comparison)

# ==========================================
# FINAL OUTPUT ON FULL DATA (KMEANS ONLY)
# ==========================================

km_full <- kmeans(
  scaled_data,
  centers = best_k,
  nstart = 25
)

results <- numeric_data
results$KMeans <- km_full$cluster
