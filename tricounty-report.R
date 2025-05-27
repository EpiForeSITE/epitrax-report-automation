library(lubridate)

# Set up file system
internal_report_folder <- "internal_reports/"
if (!dir.exists(internal_report_folder)) {
  dir.create(internal_report_folder)
}
input_data_folder <- "input_epitrax_data/"
if (!dir.exists(input_data_folder)) {
  dir.create(input_data_folder)
}
processed_data_folder <- "processed_epitrax_data/"
if (!dir.exists(processed_data_folder)) {
  dir.create(processed_data_folder)
}

# Function to read in input EpiTrax file
get_input_file <- function() {
  fname <- list.files(input_data_folder)
  
  if (length(fname) > 1) {
    stop(paste0("Please include only 1 file in the '", input_data_folder, "' folder."))
  }
  
  fpath <- paste0(input_data_folder, fname)
  if (!file.exists(fpath)) {
    stop(paste0("Please add an EpiTrax data file to the '", input_epitrax_data, "' folder"))
  }
  
  if (!grepl("\\.csv$", fpath)) {
    stop(paste0("Please add an EpiTrax data file (.csv) to the '", input_epitrax_data, "' folder"))
  }
  
  fname
}

# - Read in EpiTrax data
input_file <- get_input_file()
disease_data <- read.csv(paste0(input_data_folder, input_file), header = TRUE)
# - Move file to processed_data_folder
file.rename(paste0(input_data_folder, input_file), paste0(processed_data_folder, input_file))

# - Add columns for month number and month name
disease_data$patient_mmwr_month <- month(ymd(disease_data$patient_mmwr_year * 10000 + 0101) + disease_data$patient_mmwr_week * 7)
disease_data$month_name <- month(disease_data$patient_mmwr_month, label=TRUE)
# - Sort data by year, week, and disease
disease_data <- disease_data[order(disease_data$patient_mmwr_year, disease_data$patient_mmwr_week, disease_data$patient_disease),]

# Extract list of unique diseases
disease_list <- sort(unique(disease_data$patient_disease))
# Extract list of unique years
year_list <- sort(unique(disease_data$patient_mmwr_year), decreasing = TRUE)

# General annual summary data (annualized counts for each disease)
annual_summary_data <- data.frame(
  Disease = disease_list
)

for (y in year_list) {
  counts_y <- c()

  for (d in disease_list) {
    counts_y <- c(counts_y, sum(disease_data$patient_disease == d & disease_data$patient_mmwr_year == y))
  }

  annual_summary_data[[as.character(y)]] <- counts_y
}
# - Save to CSV file
write.csv(annual_summary_data, paste0(internal_report_folder, "annualized_counts.csv"))

# General monthly summary data (monthly counts for each disease)
monthly_data_combined <- list()
for (y in year_list) {
  monthly_summary_data <- data.frame(
    Disease = disease_list
  )

  for (m in 1:12) {
    counts_m <- c()

    for (d in disease_list) {
      counts_m <- c(counts_m, sum(disease_data$patient_disease == d & disease_data$patient_mmwr_year == y & disease_data$patient_mmwr_month == m))
    }

    m_name <- month.abb[m]
    monthly_summary_data[[m_name]] <- counts_m
  }

  # - Save each year's monthly data as a separate CSV
  write.csv(monthly_summary_data, file = paste0(internal_report_folder, "monthly_counts_", y, ".csv"))

  # - Convert to more programmatically friendly version before adding to combined data
  monthly_summary_data$Disease <- NULL
  colnames(monthly_summary_data) <- c(1:12)
  rownames(monthly_summary_data) <- disease_list
  monthly_data_combined[[as.character(y)]] <- monthly_summary_data
}


