library(ggplot2)
library(ggridges)
library(tidyr)
theme_set(theme_minimal())
lincoln_weather <- lincoln_weather
ggplot(
  lincoln_weather, 
  aes(x = `Mean Temperature [F]`, y = `Month`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3,  rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE') 



output_matrix_N_1000 <- readRDS("C:/ChristianThorjussen/BnB-CI-Test/output_matrix_N_1000.rds")

mse_data <- data.frame(matrix(ncol = 0, nrow = 1000))

for (i in 1:100) {
  data_1 <- output_matrix_N_1000[[i]]
  data_1 <- matrix(data_1, ncol = 1, byrow = T)

  data_frame_1 <- as.data.frame(t(sapply(data_1, function(x) unlist(x))))

  colnames(data_frame_1) <- c("mod1_mse", "mod1_r2", "mod2_mse", "mod_r2", "diff_mse", 'diff_r2')
  mse <- data_frame_1[6]
  mse_data <- cbind(mse_data, mse)
}


long_dataset <- pivot_longer(
  data = mse_data, 
  cols = everything(),  # Select all columns to stack
  names_to = "test",  # The name of the new column for the original column names
  values_to = "mse"  # The name of the new column for the values
)
group_number <- ceiling(seq_len(nrow(long_dataset)) / 1000)

# Create the labels by pasting 'test' with the group number
long_dataset$group_label <- paste0("test", group_number)
long_dataset$group_label <- as.factor(long_dataset$group_label)

ggplot(long_dataset, aes(x = mse, y = group_label, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) 

unique_groups <- unique(long_dataset$group_label)

# Randomly select 20 unique group labels
# Make sure you have at least 20 groups, otherwise adjust the number
selected_groups <- sample(unique_groups, min(20, length(unique_groups)))

# Filter the data frame to only include rows from the selected groups
selected_data <- long_dataset[long_dataset$group_label %in% selected_groups, ]

# Verify the result
table(selected_data$group_label)

ggplot(selected_data, aes(x = mse, y = group_label, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) 

