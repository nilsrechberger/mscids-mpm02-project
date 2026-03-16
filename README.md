# mscids-mpm02-project

```txt
                    _     _                                      ___ ____                       _           _   
 _ __ ___  ___  ___(_) __| |___       _ __ ___  _ __  _ __ ___  / _ \___ \      _ __  _ __ ___ (_) ___  ___| |_ 
| '_ ` _ \/ __|/ __| |/ _` / __|_____| '_ ` _ \| '_ \| '_ ` _ \| | | |__) |____| '_ \| '__/ _ \| |/ _ \/ __| __|
| | | | | \__ \ (__| | (_| \__ \_____| | | | | | |_) | | | | | | |_| / __/_____| |_) | | | (_) | |  __/ (__| |_ 
|_| |_| |_|___/\___|_|\__,_|___/     |_| |_| |_| .__/|_| |_| |_|\___/_____|    | .__/|_|  \___// |\___|\___|\__|
                                               |_|                             |_|           |__/               
```

This project was developed as part of the MPM02 (Applied Machine Learning and Predictive Modelling 1) module within the Master of Science in Applied Information and Data Science at [HSLU](https://www.hslu.ch/en/). It encompasses a comprehensive end-to-end data analysis, applying various machine learning and statistical modeling techniques covered in the curriculum.

## Project Structure

The repository is organized to ensure a clean separation between raw data, processing logic, and final reporting:

```bash
.
├── data
│   ├── processed   # Cleaned and transformed datasets for modeling
│   └── raw         # Original, immutable data source
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

## Setup & Reproducibility

This project uses `renv` to manage local R dependencies, ensuring that the code runs with the exact same package versions it was built with.

### 1. Clone the repository:

```bash
git clone https://github.com/nilsrechberger/mscids-mpm02-project.git
cd mscids-mpm02-project
```

### 2. Restore dependencies:

Open the R project (e.g., via RStudio or VS Code). `renv` should bootstrap itself automatically. To install the required packages, run:

```bash
renv::restore()
```

### 3. Render the report:

To generate the final analysis report, ensure you have Quarto installed and run:

```bash
quarto render report.qmd
```

## License

This project is licensed under the MIT License.
