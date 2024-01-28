# Sample data
data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)  # Example data matrix
mean_vector <- colMeans(data)  # Mean vector
cov_matrix <- cov(data)  # Covariance matrix

# Calculate Mahalanobis distances
library(mvnormalTest)
mahalanobis_distances <- mahalanobis(data, mean_vector, cov_matrix)

# Determine critical value for 50% probability contour (chi-square distribution with 2 degrees of freedom)
critical_value <- qchisq(0.5, df = 2)

# Determine observations falling within the 50% probability contour
within_contour <- mahalanobis_distances <= critical_value

# Calculate the proportion of observations falling within the contour
proportion_within_contour <- sum(within_contour) / length(within_contour)

# Print the result
cat("Proportion of observations within the 50% probability contour:", proportion_within_contour)
