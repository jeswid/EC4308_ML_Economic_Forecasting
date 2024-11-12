check = readRDS("data/complete_data_df_transformed.RDS")
# standardize the data
df_temp = na.omit(check)
# Convert factor or character columns to numeric (if appropriate)
# df_temp <- df_temp %>% mutate_if(is.factor, as.numeric) %>% mutate_if(is.character, as.numeric)
# Select only numeric columns
numeric_df <- df_temp[, sapply(df_temp, is.numeric)]  # Remove the non-numeric column

# principal component analysis 
pca_result <- prcomp(numeric_df, center = TRUE, scale. = TRUE)
summary(pca_result)  

# because it takes 14 PCs to explain 80% of total variance, we omitted PCA in this analysis