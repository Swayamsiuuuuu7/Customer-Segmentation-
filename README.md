# Customer Segmentation Using K-Means Clustering in R

## Project Overview

This project focuses on customer segmentation using the **K-Means Clustering algorithm** in R. It analyzes the Mall Customers dataset to identify different customer groups based on their annual income and spending score.

The project also includes an interactive **Shiny Dashboard** to visualize customer segments and present the analysis results.

## Objectives

- Load and explore the Mall Customers dataset.
- Perform data preprocessing and basic statistical analysis.
- Visualize customer characteristics using ggplot2.
- Standardize numerical features for clustering.
- Determine the optimal number of clusters using the Elbow Method.
- Apply K-Means clustering to segment customers.
- Develop an interactive Shiny dashboard.

## Technologies Used

- **R Programming**
- **RStudio**
- **ggplot2** – Data visualization
- **dplyr** – Data manipulation
- **Shiny** – Interactive dashboard
- **DT** – Interactive data tables

## Dataset

The project uses the Mall Customers dataset containing 200 customer records and the following attributes:

- CustomerID
- Gender
- Age
- Annual Income (k$)
- Spending Score (1–100)

The clustering model uses Annual Income and Spending Score.

## Project Structure

```text
Customer-Segmentation/
│
├── analysis.R
├── app.R
│
├── data/
│   ├── Mall_Customers.csv
│   ├── Segmented_Customers.csv
│   └── Cluster_Summary.csv
│
└── README.md
```

## Methodology

1. **Data Loading:** Import and inspect the Mall Customers dataset.
2. **Preprocessing:** Remove CustomerID and check missing and duplicate values.
3. **Exploratory Data Analysis:** Generate gender distribution and numerical histograms.
4. **Standardization:** Scale annual income and spending score.
5. **Elbow Method:** Identify a suitable number of clusters.
6. **K-Means Clustering:** Apply K-Means with 5 clusters.
7. **Visualization:** Generate customer segmentation plots.
8. **Dashboard:** Present results using an interactive Shiny application.

## Results

The final model identifies **5 customer clusters** based on income and spending behaviour.

The clusters represent different customer profiles:

- High income – High spending
- Average income – Average spending
- Low income – High spending
- Low income – Low spending
- High income – Low spending

The project also generates cluster summaries and segmented customer CSV files.

## How to Run the Project

1. Clone or download this repository.
2. Open the project folder in RStudio.
3. Open `analysis.R`.
4. Run the script to perform customer segmentation.
5. Open `app.R`.
6. Click **Run App** to launch the Shiny Dashboard.

### Install Required Packages

```r
install.packages(c("ggplot2", "dplyr", "shiny", "DT"))
```

**Note:** Make sure the CSV filenames and file paths used in `app.R` match the files inside the `data` folder.

## Conclusion

This project demonstrates the practical application of unsupervised machine learning using K-Means clustering in R. It identifies meaningful customer segments, visualizes spending patterns, and presents the results through an interactive Shiny Dashboard.
