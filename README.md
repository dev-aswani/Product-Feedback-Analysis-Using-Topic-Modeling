# Product Feedback Analysis Using Topic Modeling

This repository contains the code and report for a topic modeling project that analyzes YouTube comments on a smartphone model using Latent Semantic Analysis (LSA) and Latent Dirichlet Allocation (LDA). The objective is to uncover hidden themes and sentiments in user feedback.

---

## Project Overview

The project focuses on analyzing YouTube comments on a budget smartphone model (e.g., Samsung A14) to derive insights into user sentiments and preferences. The analysis includes data collection, preprocessing, applying topic modeling techniques, and interpreting the results.

---

## Repository Structure

### 1. `code/`

Contains the R script for the analysis:

-   `code.r`: Performs data collection, preprocessing, topic modeling (LSA and LDA), and visualization.

### 2. `report/`

Contains the detailed project report:

-   `Report.docx`: Documents the step-by-step implementation, results, and discussion.

---

## How to Use

### 1. **Prerequisites:**

-   Install R and the required libraries (eg: reticulate, httr, jsonlite, tm etc.)

### 2. **Steps to Run:**

-   Open the R script `code.r`.
-   Run the script to:
    1. Install and load necessary libraries.
    2. Create a virtual Python environment using `reticulate`.
    3. Retrieve comments from YouTube videos using the YouTube Data API.
    4. Perform preprocessing and sentiment tagging.
    5. Apply LSA and LDA for topic modeling.
    6. Visualize the results.

### 3. **Project Report:**

Refer to `Report.docx` for detailed analysis and findings.

---

## Highlights

-   **Data Collection:** Retrieves YouTube comments using Python and integrates with R for analysis.
-   **Sentiment Analysis:** Tags comments into positive, neutral, and negative categories.
-   **Topic Modeling:** Uses LSA and LDA to uncover themes in user comments.
-   **Comprehensive Visualization:** Visualizes topics and sentiment distribution for better insights.

---

## Dataset Information

The dataset consists of comments retrieved from a YouTube video about a budget smartphone. Data collection was performed using the YouTube Data API and saved for replicability.

---

## Contact

For any questions or feedback, please use the repository’s issue tracker.
