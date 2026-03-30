# TODOS:
# - Generate data preview for GitHub
# - Remove coded sting versions
# - String to factors
# - UID to index

library(tidyverse)
library(here)

cat("Start data cleaning...\n\n")

processed_data_dir <- here("data", "processed")

# Creates the data dir if not already exist
if (!dir.exists(processed_data_dir)) {
    cat(paste("Create data dir", processed_data_dir, "\n\n"))
    dir.create(processed_data_dir, recursive = TRUE)
}

raw_data_path <- here("data", "raw", "raw_data.csv")
processed_data_path <- here("data", "processed", "processed_data.csv")
raw_data <- read_csv(raw_data_path)

# Cleaning pipeline
processed_data <- raw_data %>%
    # Check for duplicates in AccidentUID (primary key)
    distinct(
        AccidentUID,
        .keep_all = TRUE
    ) %>%
    # Remove NA's
    drop_na() %>%
    # Remove foreign language columns. We keep the english version.
    select(
        -ends_with(
            c("_de", "_fr", "_it")
        )
    ) %>%
    # Remove coded string versions
    select(
            -c(
                "_de", "_fr", "_it")
        )
    ) %>%
    # String to lowercase
    mutate(
        across(
            where(is.character),
            str_to_lower
        )
    ) %>%
    # Replace string spaces to "_"
    mutate(
        across(
            where(is.character),
            ~ str_replace_all(.x, " ", "_")
        )
    ) %>%
    # Create dt dummy for time series analysis: YYYY-MM-01 HH:00:00
    mutate(
        accident_dt_dummy = make_datetime(
            year = AccidentYear, 
            month = AccidentMonth, 
            day = 1, 
            hour  = AccidentHour
        )
    )

cat("Done cleaning data.\n\n")

write.csv(processed_data, file = processed_data_path, append = FALSE, quote = TRUE, sep = ",")