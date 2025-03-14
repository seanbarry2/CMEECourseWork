# Miniproject Report and Analysis

This project presents an analysis of microbial growth data using several growth models – logistic, Gompertz, quadratic, and cubic – and compares their performance using metrics such as R² and AICc. The project is organized into three main directories: code, data, and results.

## Directory Structure

- **code/**
  - **Miniproject.pdf**
    The complete report for the Miniproject.
  - **run_Miniproject.sh**
    A bash script that runs the R script.
  - **Model_fitting.R**
    The main R script that performs data management, model fitting, metric computation, and result generation.

- **data/**
  - **LogisticGrowthData.csv**
    The dataset used for analysis. This CSV file contains the following columns: X, Time, PopBio, Temp, Time_units, PopBio_units, Species, Medium, Rep, Citation.
  - **LogisticGrowthMetaData.csv**
    A CSV file providing metadata for the dataset. It describes:
    - Time: Time at which the measurement was taken.
    - PopBio: Population or biomass measurement.
    - Temp: Temperature at which the microbe was grown (degrees Celsius).
    - Time_units: Units in which time is measured.
    - PopBio_units: Units for the population or biomass measurement.
    - Species: Species or strain used.
    - Medium: Medium in which the microbe was grown.
    - Rep: Replicate number within the experiment.
    - Citation: Citation for the study.

- **results/**
  - **model_fitting_results.csv**
    A CSV file containing the results of the model fitting. It includes the following columns: 
    group_id, logistic.R2, logistic.AICc, gompertz.R2, gompertz.AICc, quad.R2, quad.AICc, cubic.R2, cubic.AICc, Akaike_weight_logistic, Akaike_weight_gompertz, Akaike_weight_quad, Akaike_weight_cubic, PopBio_units, Temp.

## How to Run the Analysis

1. **Dependencies:**
   The R script requires the following R packages: minpack.lm, parallel, tidyverse, and AICcmodavg. Install them using:
   
   ```r
   install.packages(c("minpack.lm", "parallel", "tidyverse", "AICcmodavg"))

2. **Execution:**

Navigate to the code directory.

Run the bash script by executing:

cd code
bash run_Miniproject.sh

This command launches the R script (Model_fitting.R), which reads the data from the data directory, performs the model fitting and metric calculations, and outputs the results in the results directory as model_fitting_results.csv.

3. **Report:**
The final report is contained in the file Miniproject.pdf within the code directory.

## Project Overview
**Data Management:**
The R script reads data from LogisticGrowthData.csv, cleans it (removing rows with negative PopBio values and NA values), and assigns a unique group identifier based on experimental conditions.

**Model Definitions:**
Functions for the logistic and four-parameter Gompertz models (fitted in log-space) are defined. Additionally, quadratic and cubic models are fitted using linear regression.

**Model Fitting and Metrics:**
The script fits each model for each group (if there are sufficient data points) using multiple random-start iterations to optimize convergence. Metrics such as R², AICc, and Akaike weights are computed to compare model performance.

**Results:**
The output results, including the performance metrics for each model and group, are saved in model_fitting_results.csv located in the results directory.

**Author**
Sean Barry
Email: sb4524@ic.ac.uk
