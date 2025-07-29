# Install if not already installed
#install.packages("readxl")
#install.packages("vegan")
#install.packages("pheatmap")
#install.packages("plotly")
#library(pheatmap)

# Load the package
library(readxl)
library(dplyr)
library(ggplot2)
library(vegan)
library(pheatmap)
library(plotly)
library(htmlwidgets)

# Read the Excel file
data <- read_xlsx("FINAL Sleep Content Coding (1).xlsx", col_names = FALSE, sheet = 5)

View(data)

names(data)    # List of column names
str(data)      # Structure and data types
  
# Select Column D specifically (the 4th column)
symptoms <- data[[4]]
symptoms <- symptoms[-1]
symptoms <- as.character(symptoms)
symptoms <- na.omit(symptoms)
print(symptoms)

# Extract binary response matrix from Columns E to O (5th to 15th)
response_matrix <- data[-4, 5:14]

# Remove NAs or blanks in symptom names
symptoms <- symptoms[symptoms != ""]

# Make sure the binary matrix is numeric (e.g., 1s and 0s)
# Step 1: Extract questionnaire response columns (E to O)
response_data <- data[-1, 5:14]  # skip header row if present

# Step 2: Convert all columns to numeric
response_data <- as.data.frame(lapply(response_data, as.numeric))
response_matrix <- as.data.frame(lapply(response_matrix, as.numeric))
symptom_scores <- rowSums(response_data, na.rm = TRUE)

# Combine with symptom names
response_data <- data[-1, 5:14]
symptom_df <- data.frame(Symptom = symptoms, Count = symptom_scores)
y_max <- max(symptom_df$Count)

# Plot Bar Graph Based on Frequency of Symptoms
barplot(symptom_df$Count,
        names.arg = symptom_df$Symptom,
        main = "Frequency of Symptoms",
        xlab = "",
        ylab = "Symptom Frequency",
        las = 2,
        width = 0.8,
        space = 0.2,
        col = "skyblue",
        cex.names = 0.7,
        ylim = c(0, y_max + 2))

# Step 3: Extract questionnaire names from first row (columns F to P)
questionnaire_names <- as.character(unlist(data[1, 5:14]))
print(questionnaire_names)

# Step 5: Extract response data from columns F to P, skipping the first row
response_data <- data[2:nrow(data), 5:14]    # Skip row 1

# Step 4: Convert all columns to numeric (ensures 1s and 0s are treated properly)
response_data <- as.data.frame(lapply(response_data, as.numeric))

# Step 6: Sum 1s down each column
symptom_counts <- colSums(response_data, na.rm = TRUE)
print(symptom_counts)

# Step 7: Create data frame for plotting
summary_df <- data.frame(
  Questionnaire = questionnaire_names,
  Count = symptom_counts
)

y_maxx <- max(summary_df$Count)

#Step 8: Plot the bar chart

barplot(summary_df$Count,
        names.arg = summary_df$Questionnaire,
        main = "Number of Symptoms per Questionnaire",
        xlab = "Questionnaire",
        ylab = "Symptom Count",
        las = 2,
        width = 0.8,
        space = 0.2,
        col = "purple",
        cex.names = 0.7,
        ylim = c(0, 65))

binary_data <- data[-1, 5:14]

# Step 0: Extract questionnaire names
questionnaire_names <- as.character(unlist(data[1, 5:14]))

# Step 1: Convert all to numeric
binary_data <- as.data.frame(lapply(binary_data, as.numeric))

# Step 2: Remove rows AND columns with any NA values
binary_data_clean <- binary_data[complete.cases(binary_data), ]
binary_data_clean <- binary_data_clean[, colSums(is.na(binary_data_clean)) == 0]

# Step 3: Apply questionnaire names
colnames(binary_data_clean) <- questionnaire_names

# Step 4: Transpose to compare columns (e.g., questionnaires)
binary_matrix_t <- t(binary_data_clean)

# ✅ Skip if too few columns left
if (nrow(binary_matrix_t) < 2) {
  stop("Too few questionnaires left after cleaning.")
}

# Step 5: Compute Jaccard similarity
jaccard_dist <- vegdist(binary_matrix_t, method = "jaccard", binary = TRUE)
jaccard_sim <- 1 - as.matrix(jaccard_dist)

# Step 6: Add labels (if missing)
rownames(jaccard_sim) <- questionnaire_names
colnames(jaccard_sim) <- questionnaire_names

# Step 6: Final check before plotting
if (any(is.na(jaccard_sim)) | any(is.nan(jaccard_sim)) | any(is.infinite(jaccard_sim))) {
  stop("Jaccard similarity matrix contains NA, NaN, or Inf.")
}

# Step 7: Plot heatmap
pheatmap(jaccard_sim,
        display_numbers = TRUE,
        main = "Jaccard Similarity Matrix",
        col = colorRampPalette(c("white", "red"))(100),
        fontsize_number = 10,
        margins = c(10, 10),
        labRow = questionnaire_names,
        labCol = questionnaire_names)


# Read the Excel file
data2 <- read_xlsx("FINAL Sleep Content Coding (1).xlsx", sheet = 6)

View(data2)

options(viewer = NULL)
fig <- plot_ly(
  data2,
  labels = ~Labels,
  parents = ~Parents,
  values = ~Values,
  type = 'sunburst',
  branchvalues = 'total',
  textinfo = 'label'
) %>%
  layout(title = "Sunburst Plot", colorway  = ~Colors)

fig

htmlwidgets::saveWidget(as_widget(fig), "SleepSunburstPlot.html")

data3 <- read_xlsx("FINAL Sleep Content Coding (1).xlsx", sheet = 7)

fig2 <- plot_ly(
  data3,
  labels = ~Labels,
  parents = ~Parents,
  values = ~Values,
  type = 'sunburst',
  branchvalues = 'total',
  textinfo = 'label',
  marker = list(colors = ~I(Colors))
) %>%
  layout(title = "Sunburst Plot")

fig2

htmlwidgets::saveWidget(as_widget(fig2), "SleepSunburstPlotICSD.html", knitrOptions = list())
