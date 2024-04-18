#PCA
mat<- read.csv("C:/Users/anant/OneDrive/Desktop/Input.csv")
mat1 <- as.matrix(mat[-c(16:19),-1])
mat1_norm <- log2((1000000*mat1/rowSums(mat1))+1)
#mat1_norm_sub <- mat1_norm[,-which(apply(mat1_norm,2,var)==0)] #1 for row, 2 for col
PCA <- prcomp(mat1_norm)
pca_sub <- as.data.frame(PCA$x)
mat <- mat[-c(16:19),]
pca_sub <- cbind(pca_sub,mat[,c(1)])
eigen <- PCA$sdev^2
PC1_var <- round(100*eigen[1]/sum(eigen))
PC2_var <- round(100*eigen[2]/sum(eigen))
ggplot(pca_sub,aes(x=PC1, y=PC2, color=Cell_lines))+geom_point()+labs(x=paste0("PC1 ",PC1_var,"%"),y=paste0("PC2 ",PC2_var,"%"))

# Perform PCA
pca_result <- prcomp(mat1, scale. = TRUE)

# Extract loadings of genes on PC1 and PC2
loadings <- pca_result$rotation

# Find top contributing genes to PC1
top_genes_pc1 <- colnames(mat1)[order(abs(loadings[,1]), decreasing = TRUE)]

# Find top contributing genes to PC2
top_genes_pc2 <- colnames(mat1)[order(abs(loadings[,2]), decreasing = TRUE)]

# Find top contributing genes to PC2
top_genes_pc3 <- colnames(mat1)[order(abs(loadings[,3]), decreasing = TRUE)]

# Print top contributing genes for PC1 and PC2
print("Top contributing genes for PC1:")
print(head(top_genes_pc1, 10)) # Print top 10 genes
print("Top contributing genes for PC2:")
print(head(top_genes_pc2, 10)) # Print top 10 genes


