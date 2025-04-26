# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(dplyr)      # For data manipulation
library(RColorBrewer) # For color palettes
library(dendextend) # For manipulating dendrograms
library(gplots)     # For heatmap plotting

# 1: Load the dataset
spellman <- read.csv("/home/ibab/Downloads/spellman-wide.csv")  # Assuming the file is in the current working directory

# 2: Print the dimensions of the dataframe
cat("Dimensions of the spellman dataframe: ", dim(spellman), "\n")

# 3: Print the first 5 rows and the first 8 columns of the dataframe
head(spellman[, 1:8], 5)

# 4: Prepare the dissimilarity matrix using correlation
spellman_cor <- select(spellman, -time, -expt) %>%
  cor(use = "pairwise.complete.obs")  # Pairwise complete observations to handle missing values
#print(spellman_cor)

# Convert the correlation matrix to a distance matrix (1 - correlation)
spellman_dist <- as.dist(1 - spellman_cor)

# 5: Perform hierarchical clustering using 'complete' linkage method
spellman_tree <- hclust(spellman_dist, method = "complete")

# Plot the dendrogram
plot(spellman_tree, main = "Hierarchical Clustering (Complete Linkage)", cex = 0.5)

# 6: Manipulate the dendrogram using dendextend
spellman_dend <- as.dendrogram(spellman_tree)
plot(spellman_dend, leaflab = "none")  # Removing labels to make the plot cleaner

# 7: Cut the tree into 4 clusters
clusters <- cutree(spellman_dend, k = 4)

# Print the frequency table of cluster assignments
cat("Cluster frequency table:\n")
print(table(clusters))

# Print the first 6 cluster assignments
cat("Cluster assignments for the first 6 genes:\n")
print(clusters[1:6])

# 8: Color the branches of the dendrogram and plot with different cluster counts
# Plot the tree with 4 clusters
plotc <- color_branches(spellman_tree, k = 4)
plot(plotc, leaflab = "none")  # Removing labels for better readability

# Plot the tree with 8 clusters
plotc_8 <- color_branches(spellman_tree, k = 8)
plot(plotc_8, leaflab = "none")  # Removing labels for better readability

# Print the frequency table for 8 clusters
cat("Cluster frequency table for 8 clusters:\n")
print(table(cutree(spellman_dend, k = 8)))

# 9: Create a dataframe with genes and their corresponding cluster assignments
clusters_df <- data.frame(gene = names(clusters), cluster = clusters)

# Print the cluster assignment for gene "YALO22C"
cat("The cluster for gene YALO22C is:\n")
print(clusters_df[clusters_df$gene == "YALO22C", ])

# 10: Get the list of genes in the 3rd cluster using the filter() function
cluster3_genes <- filter(clusters_df, cluster == 3) %>%
  select(gene) %>%
  pull()

cat("Genes in the 3rd cluster:\n")
print(cluster3_genes)

# 11: Generate a heatmap for the expression of genes in the 3rd cluster
spellman_long <- gather(spellman, gene, expression, -expt, -time)

# Set up color scheme for the heatmap
color_scheme <- rev(brewer.pal(8, "RdBu"))

# Plot the heatmap for genes in the 3rd cluster during the "alpha" experiment
ggplot(spellman_long %>%
         filter(gene %in% cluster3_genes & expt == "alpha"), 
       aes(x = time, y = gene)) +
  geom_tile(aes(fill = expression)) +
  scale_fill_gradientn(colors = color_scheme, limits = c(-2, 2)) +
  theme(axis.text.y = element_text(size = 6))  # Adjust size of y-axis labels

# 12: Combine the heatmap with the dendrogram using sub-trees

# Create sub-trees by cutting the dendrogram at height 1.48
sub_trees <- cut(spellman_dend, h = 1.48)

# Extract the 3rd cluster sub-tree
cluster3_tree <- sub_trees$lower[[3]]
cat("Sub-tree for Cluster 3:\n")
print(cluster3_tree)

# Plot the 3rd cluster sub-tree
cluster3_tree %>%
  set("labels_cex", 0.45) %>%
  set("labels_col", "red") %>%
  plot(horiz = TRUE)  # Plot the dendrogram horizontally

# Use the heatmap.2 function from the gplots package to combine the heatmap and the dendrogram
alpha_factor <- filter(spellman, expt == "alpha")
alpha_mtx <- as.matrix(select(alpha_factor, -time, -expt))
row.names(alpha_mtx) <- alpha_factor$time
transposed_alpha_mtx <- t(alpha_mtx)

# Generate the heatmap with dendrogram using heatmap.2
heatmap.2(transposed_alpha_mtx,
          Rowv = cluster3_tree,  # Use the previously calculated sub-tree for rows
          Colv = NULL,  # Don't modify the order of columns
          dendrogram = "row",  # Only draw row dendrograms
          breaks = seq(-2, 2, length.out = 9),  # Set break points for color scheme
          col = color_scheme,  # Use the pre-defined color scheme
          trace = "none", density.info = "none",  # Remove unnecessary plot elements
          xlab = "Time (mins)")  # Label the x-axis
