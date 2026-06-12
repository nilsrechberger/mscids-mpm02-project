# mscids-mpm02-project

```txt
================================================
          Swiss Road Accident Analysis
================================================

         _______                     _______
        /|_||_\`.__     \ | /     __.'/_||_|\
       (   _    _ _\  --*BAM*--  /_ _    _   )
       =`-(_)--(_)-'    / | \    '-(_)--(_)-'=

================================================
```

This project was developed as part of the MPM02 (Applied Machine Learning and Predictive Modelling 1) module within the Master of Science in Applied Information and Data Science at [HSLU](https://www.hslu.ch/en/). It encompasses a comprehensive end-to-end data analysis, applying various machine learning and statistical modeling techniques covered in the curriculum.

## Project Structure

The repository is organized to ensure a clean separation between raw data, processing logic, and final reporting:

```bash
.
├── data            # Contains train, validation and test data sets
│   ├── processed   # Cleaned and transformed data sets
│   └── raw         # Original, immutable data source
├── docs            # --> FINAL REPORT DIR WITH THE PDF <--
├── .env.example    # Example .env file for global configs and variables
├── .gitignore
├── img             # Static assets and visualizations for Report
├── LICENSE
├── R
│   ├── evaluation  # Performance metrics, validation, and diagnostics
│   └── models      # Script implementations for training (LM, GLM, SVM, etc.)
├── README.md
├── renv.lock       # Snapshot of R package dependencies
├── report.qmd      # Main Quarto document integrating all analyses
├── .Rprofile       # Automatic environment activation via renv
└── utilities       # Helper functions (e.g., data cleaning, custom plotting themes)
```

## Prerequisites

- R >= 4.2
- [Quarto](https://quarto.org/docs/get-started/) >= 1.4
- `renv` R package (installed automatically via `.Rprofile` on first session)

## Setup & Reproducibility

This project uses `renv` to manage R package dependencies, pinning exact versions via `renv.lock` to ensure reproducible execution.

### 1. Clone the repository

```bash
git clone https://github.com/nilsrechberger/mscids-mpm02-project.git
cd mscids-mpm02-project
```

### 2. Configure environment variables

Use this template to create your `.env` file:

```txt
DATA_URL=PATH_TO_DATA_URL
```

### 3. Restore dependencies

Open the project in RStudio or VS Code. `renv` bootstraps itself automatically via `.Rprofile`. To install all required packages:

```r
renv::restore()
```

### 4. Render the report

```bash
quarto render report.qmd
```

## License

This project is licensed under the MIT License.
