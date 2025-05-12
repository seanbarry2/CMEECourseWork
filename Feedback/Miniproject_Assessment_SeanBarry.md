# Miniproject Feedback and Assessment

## Report

**"Guidelines" below refers to the MQB report [MQB Miniproject report guidelines](https://mulquabio.github.io/MQB/notebooks/Appendix-MiniProj.html#the-report) [here](https://mulquabio.github.io/MQB/notebooks/Appendix-MiniProj.html) (which were provided to the students in advance).**


**Title:** “Fitting and Comparing Microbial Population Growth Models”

- **Introduction (15%)**  
  - **Score:** 12/15  
  - Historical context referencing Malthus/Verhulst. Could more explicitly state the precise aim or question.

- **Methods (15%)**  
  - **Score:** 12/15  
  - Data from 10 sources (302 subsets). Four main models tested. Additional detail on parameter initialization or iteration recommended.

- **Results (20%)**  
  - **Score:** 15/20  
  - Gompertz best in R², logistic best in AICc. Could show more numeric distribution or partial fits. 

- **Tables/Figures (10%)**  
  - **Score:** 7/10  
  - The snippet references multiple tables but not deeply integrated with text. [MQB Miniproject report guidelines](https://mulquabio.github.io/MQB/notebooks/Appendix-MiniProj.html#the-report) recommend robust referencing.

- **Discussion (20%)**  
  - **Score:** 15/20  
  - Interprets the difference in model complexity vs. parsimony. Some mention of data constraints would strengthen the reflection.

- **Style/Structure (20%)**  
  - **Score:** 14/20  
  - Coherent flow. Cross-linking results to discussion more explicitly would help.

**Summary:** A solid report that outlines differences among models well, especially Gompertz vs. Logistic. More detail on parameter-fitting and numeric breakdowns would enhance clarity.

**Report Score:** 75  

---

## Computing

### Project Structure & Workflow

**Strengths**

* `run_Miniproject.sh` directly invokes the main R script, making execution straightforward.
* All data handling, model fitting, and result export occur in `Model_fitting.R`, reducing context switching.
* Leveraging `parallel::mclapply()` scales fitting across cores efficiently.

**Suggestions**

1. **Shell Script Robustness:**

   * Use `#!/usr/bin/env bash` (current `#!bin/bash` is missing a slash).
   *  Add `set -euo pipefail` to fail fast on errors.
   * `cd "$(dirname "$0")"` ensures paths resolve relative to script.
   * Pipe output to a log file (`2>&1 | tee results/pipeline.log`) for reproducibility.

2. **Reproducible R Environment:**

   * Use **renv** to snapshot package versions instead of ad-hoc installation. Include `renv.lock` and call `renv::restore()` before running.

---

### README File

**Strengths**

* Describes the purpose, data files, and high-level model comparisons.
* Lists required R packages.

**Suggestions**

1. Show step-by-step commands:

   ```bash
   git clone <repo>
   cd code
   Rscript -e "renv::restore()"
   bash run_Miniproject.sh
   ```
2. Visualize where to place data and where results will appear.
3. Add a LICENSE file and cite your data source in a “Data” section, including metadata references.
4. Ensure markdown code blocks are closed and paths match actual layout (e.g., `code/run\_Miniproject.sh`).

---

## `run_Miniproject.sh`

```bash
#! /usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

# Run the main R fitting script
Rscript Model_fitting.R 2>&1 | tee ../results/pipeline.log
```

* portable shebang, strict mode, directory guard, and logging redirection.

---

## R Script: `Model_fitting.R`

### Data Management

* `rm(list = ls())` is acceptable when the script is standalone. Alternatively, wrap code in a `main()` function and call it at the end.
* Replace `read.csv("../data/LogisticGrowthData.csv")` with `here::here("data", "LogisticGrowthData.csv")` for portability.
* The `cumsum(c(TRUE, ...) )` approach yields `group_id`s but isn’t intuitive. Can instead use:

  ```r
  data <- data %>%
    arrange(Temp, Species, Medium, Rep, Citation, Time) %>%
    mutate(group_id = group_indices(., Temp, Species, Medium, Rep, Citation))
  ```

#### Model Definitions & Fitting

* Logistic and Gompertz definitions are clear. Add explicit parameter bounds that match biological constraints (e.g., `r_max ≥ 0`).
* Good practice. To streamline:
  * Move `set.seed(123)` outside of the loop.
  * Extract start-value generation into a helper function.
  * Flag and log cases where all attempts failed.

#### Metrics Computation

* Pre-allocating a one-row `df` inside `compute.metrics()` is fine; alternatively, return a named vector and bind with `tibble::as_tibble_row()`.
* You use `AICcmodavg::AICc()`. Ensure you pass the correct argument type (`nlsLM` vs `lm`).
* Calculation is correct. Consider using `AICcmodavg::Weights()` to leverage tested implementations.

#### Results Export

* `bind_rows(mclapply(...))` is good. After the join of `PopBio_units` and `Temp`, consider moving joins into the initial data pipeline to maintain a single source of truth.
* Use `here::here("results", "model_fitting_results.csv")` and ensure the `results/` folder exists (create with `dir.create()` if needed).

---

## NLLS Fitting Approach

**Strengths**

* Comprehensive comparison of logistic, Gompertz, quadratic, and cubic models.
* Robust multi-start strategy guards against poor local minima.

**Suggestions**

1. Use `nls.multstart` for cleaner syntax, built-in bounds, and parallel start points.
2. Tighten lower/upper limits to biologically plausible ranges and log any fits hitting those bounds.
3. Collect and summarize convergence codes and residual standard errors to diagnose problematic fits.
4. Compute Akaike weights with `AICcmodavg::Weights()` and plot their distribution (e.g., via boxplots) to interpret model support variability.
5. Implement leave-one-out Cross-Validation on time points to assess predictive performance beyond information criteria.

---

### Summary

Your streamlined pipeline is easy to run and logically structured. By adding directory conventions, portable path handling (`here`), robust shell scripting, environment locking (`renv`), and leveraging advanced optimization utilities (`nls.multstart`), you would have further enhanced reproducibility, clarity, and diagnostic capability.

### **Score: 76**

---

## Overall Score: (75 + 76)/2 = 75.5