
# ============================================================
# PROJECT: CUSTOMER SEGMENTATION USING R
# ============================================================

# Aim: To perform customer segmentation using K-Means clustering.




# ============================================================
# STEP 1: LOAD AND INSPECT DATASET
# ============================================================

# Load dataset
customers <- read.csv("data/Mall_Customers.csv")

# Display first 6 records
print("First 6 Records:")
print(head(customers))

# Display dataset structure
print("Dataset Structure:")
str(customers)

# Display dimensions
print("Dataset Dimensions:")
print(dim(customers))

# Display column names
print("Column Names:")
print(colnames(customers))

# Check missing values
print("Missing Values:")
print(colSums(is.na(customers)))

# Statistical summary
print("Statistical Summary:")
print(summary(customers))


# ============================================================
# STEP 2: DATA PREPROCESSING AND BASIC ANALYSIS
# ============================================================

# Remove CustomerID
customer_data <- customers[, -1]

# Convert Gender into factor
customer_data$Gender <- as.factor(customer_data$Gender)

# Check missing values
print("Missing Values:")
print(colSums(is.na(customer_data)))

# Check duplicate records
print("Duplicate Records:")
print(sum(duplicated(customer_data)))

# Gender distribution
print("Gender Distribution:")
print(table(customer_data$Gender))

# Average age
print("Average Age:")
print(mean(customer_data$Age))

# Average annual income
print("Average Annual Income:")
print(mean(customer_data$Annual.Income..k..))

# Average spending score
print("Average Spending Score:")
print(mean(customer_data$Spending.Score..1.100.))

# Display processed dataset
print("Processed Dataset:")
print(head(customer_data))


# ============================================================
# STEP 3: DATA VISUALIZATION
# ============================================================

library(ggplot2)

# 1. Gender Distribution
p1 <- ggplot(customer_data, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  labs(title = "Customer Gender Distribution",
       x = "Gender", y = "Number of Customers") +
  theme_minimal()

print(p1)

# 2. Age Distribution
p2 <- ggplot(customer_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue",
                 color = "black") +
  labs(title = "Customer Age Distribution",
       x = "Age", y = "Frequency") +
  theme_minimal()

print(p2)

# 3. Annual Income Distribution
p3 <- ggplot(customer_data, aes(x = Annual.Income..k..)) +
  geom_histogram(binwidth = 10, fill = "orange",
                 color = "black") +
  labs(title = "Annual Income Distribution",
       x = "Annual Income (k$)", y = "Frequency") +
  theme_minimal()

print(p3)

# 4. Spending Score Distribution
p4 <- ggplot(customer_data, aes(x = Spending.Score..1.100.)) +
  geom_histogram(binwidth = 10, fill = "lightgreen",
                 color = "black") +
  labs(title = "Spending Score Distribution",
       x = "Spending Score", y = "Frequency") +
  theme_minimal()

print(p4)


# ============================================================
# STEP 4: SELECT FEATURES AND STANDARDIZE DATA
# ============================================================

# Select Annual Income and Spending Score
clustering_data <- customer_data[, c(
  "Annual.Income..k..",
  "Spending.Score..1.100."
)]

# Standardize the selected features
scaled_data <- scale(clustering_data)

# Display standardized data
print("Standardized Data:")
print(head(scaled_data))

# Check dimensions
print("Clustering Data Dimensions:")
print(dim(scaled_data))

# Display summary
print("Standardized Data Summary:")
print(summary(scaled_data))


# ============================================================
# STEP 5: ELBOW METHOD
# ============================================================

set.seed(123)

wss <- numeric(10)

for (k in 1:10) {
  model <- kmeans(scaled_data, centers = k, nstart = 25)
  wss[k] <- model$tot.withinss
}

# Create Elbow Method dataset
elbow_data <- data.frame(
  Clusters = 1:10,
  WSS = wss
)

# Plot Elbow Method
p5 <- ggplot(elbow_data, aes(x = Clusters, y = WSS)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters",
       y = "Within Cluster Sum of Squares") +
  theme_minimal()

print(p5)


# ============================================================
# STEP 6: K-MEANS CLUSTERING
# ============================================================

# Select 5 clusters based on the Mall Customers
# income and spending segmentation pattern.
set.seed(123)

final_model <- kmeans(
  scaled_data,
  centers = 5,
  nstart = 25
)

# Add cluster labels to customer data
customer_data$Cluster <- as.factor(final_model$cluster)

# Display cluster assignments
print("Customer Cluster Assignments:")
print(head(customer_data))

# Display number of customers in each cluster
print("Customers in Each Cluster:")
print(table(customer_data$Cluster))

# Display cluster centers in standardized units
print("Cluster Centers:")
print(final_model$centers)


# ============================================================
# STEP 7: CUSTOMER SEGMENT VISUALIZATION
# ============================================================

p6 <- ggplot(
  customer_data,
  aes(x = Annual.Income..k..,
      y = Spending.Score..1.100.,
      color = Cluster)
) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Customer Segmentation Using K-Means",
       x = "Annual Income (k$)",
       y = "Spending Score",
       color = "Customer Cluster") +
  theme_minimal()

print(p6)


# ============================================================
# STEP 8: CUSTOMER CLUSTER PROFILING
# ============================================================

# Calculate average characteristics of each cluster
cluster_summary <- aggregate(
  cbind(Age, Annual.Income..k..,
        Spending.Score..1.100.) ~ Cluster,
  data = customer_data,
  FUN = mean
)

print("Customer Cluster Profile:")
print(cluster_summary)

# Display number of customers in each cluster
cluster_count <- as.data.frame(
  table(customer_data$Cluster)
)

colnames(cluster_count) <- c("Cluster", "Customers")

print("Cluster Customer Count:")
print(cluster_count)


# ============================================================
# STEP 9: SAVE RESULTS
# ============================================================

# Save segmented customer data
write.csv(
  customer_data,
  "data/Segmented_Customers.csv",
  row.names = FALSE
)

# Save cluster summary
write.csv(
  cluster_summary,
  "data/Cluster_Summary.csv",
  row.names = FALSE
)

print("Customer segmentation completed successfully!")

print("Results saved in the data folder.")


