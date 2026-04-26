library(dotenv)
library(here)

load_dot_env(".env")

data_url <- Sys.getenv("DATA_URL")
if (data_url == "") {
    stop("Error: DATA_URL is not defined in .env!")
}

raw_dir <- here("data", "raw")
dest_file <- file.path(raw_dir, "raw_data.csv")

cat("Start data download... \n\n")

# Creates the data dir if not already exist
if (!dir.exists(raw_dir)) {
    cat(paste("Create data dir", raw_dir, "\n\n"))
    dir.create(raw_dir, recursive = TRUE)
}

tryCatch(
    {
        download.file(
            url = data_url,
            destfile = dest_file,
            mode = "wb"
        )
        cat("Finish data download \n")
    },
    error = function(e) {
        stop(conditionMessage(e))
    }
)

# Data preview for GitHub
preview_data <- read.csv(dest_file, nrows = 10)
data_preview <- file.path(raw_dir, "preview.csv")

write.csv(
    preview_data,
    file = data_preview,
    row.names = FALSE
)