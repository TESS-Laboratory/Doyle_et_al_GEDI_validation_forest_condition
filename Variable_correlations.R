correlation_matrix <- cor(allGEDI2ABPCA, use = "complete.obs", method = "pearson")  # You can replace 'pearson' with 'spearman' or 'kendall'
print(correlation_matrix)



non_numeric_columns <- sapply(allGEDI2ABPCA, function(col) !is.numeric(col))
non_numeric_columns_names <- names(allGEDI2ABPCA)[non_numeric_columns]
print(non_numeric_columns_names)

library(corrplot)
corrplot(correlation_matrix, method = "color", tl.cex = 0.8, number.cex = 0.7)  # Customize font sizes


plot(allGEDI2ABPCA$cover, allGEDI2ABPCA$pai)
