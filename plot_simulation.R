# Load necessary libraries
library(gridExtra)
library(ggplot2)
library(grid)

# Create data frame for plotting
plot_data <- data.frame(
  p = factor(rep(p_values, each = 300)),
  lambda = factor(rep(rep(lambdas, each = 100), times = 3)),
  relative_error = unlist(test_errors),
  edf = unlist(effective_dof)
)

# Scale the relative error by the number of features
plot_data$relative_error <- plot_data$relative_error / as.numeric(as.character(plot_data$p))

# Calculate average degrees of freedom for labels and apply ceiling
average_edf <- sapply(lambdas, function(lambda) {
  sapply(p_values, function(p) {
    ceiling(mean(unlist(effective_dof[[paste(p, lambda, sep = "_")]])))
  })
})

# Extract the labels for each level of p
edf_l20 <- average_edf[1, ]
edf_l100 <- average_edf[2, ]
edf_l1000 <- average_edf[3, ]

# Define custom labels for each plot
labels_20 <- as.character(edf_l20)
labels_100 <- as.character(edf_l100)
labels_1000 <- as.character(edf_l1000)

# Plot for p = 20
plot_20 <- ggplot(subset(plot_data, p == "20"), aes(x = factor(lambda, levels = lambdas), y = relative_error)) +
  geom_boxplot(fill = "darkgreen") +
  scale_x_discrete(labels = labels_20) +
  labs(
    title = "20 features"
  ) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# Plot for p = 100
plot_100 <- ggplot(subset(plot_data, p == "100"), aes(x = factor(lambda, levels = lambdas), y = relative_error)) +
  geom_boxplot(fill = "darkgreen") +
  scale_x_discrete(labels = labels_100) +
  labs(
    title = "100 features"
  ) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# Plot for p = 1000
plot_1000 <- ggplot(subset(plot_data, p == "1000"), aes(x = factor(lambda, levels = lambdas), y = relative_error)) +
  geom_boxplot(fill = "darkgreen") +
  scale_x_discrete(labels = labels_1000) +
  labs(
    title = "1000 features"
  ) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# Combine plots with common x and y axis labels
grid.arrange(
  arrangeGrob(plot_20, plot_100, plot_1000, ncol = 3),
  bottom = textGrob("Effective Degrees of Freedom", gp = gpar(fontsize = 15)),
  left = textGrob("Relative Test Error", rot = 90, gp = gpar(fontsize = 15))
)
