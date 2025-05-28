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
  if (!file.exists(fpath) || !grepl("\\.csv$", fpath)) {
    stop(paste0("Please add an EpiTrax data file (.csv) to the '", input_data_folder, "' folder"))
  }
  
  fname
}

# - Read in EpiTrax data
input_file <- get_input_file()
disease_data <- read.csv(paste0(input_data_folder, input_file), header = TRUE)

# TODO: Process that input file has the correct fields

# - Move file to processed_data_folder
# file.rename(paste0(input_data_folder, input_file), paste0(processed_data_folder, input_file))

# - Add columns for month number and month name
disease_data$patient_mmwr_month <- month(ymd(disease_data$patient_mmwr_year * 10000 + 0101) + disease_data$patient_mmwr_week * 7)
disease_data$month_name <- month(disease_data$patient_mmwr_month, label = TRUE)
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

# Compute 5yr averages for each month
five_yr_avgs <- data.frame(matrix(0, ncol = 12, nrow = length(disease_list)), row.names = disease_list)
m_names <- month.abb[1:12]
colnames(five_yr_avgs) <- m_names
# print(five_yr_avgs)

for (m in 1:12) {
  m_avgs <- c()
  
  for (d in disease_list) {
    d_counts <- 0
    for (y in year_list[2:length(year_list)]) {
      d_counts <- d_counts + monthly_data_combined[[as.character(y)]][d,m]
    }
    m_avgs <- c(m_avgs, d_counts/5)
  }

  five_yr_avgs[[m_names[m]]] <- m_avgs
}

# - Save to CSV
# write.csv(five_yr_avgs, paste0(internal_report_folder, "five_yr_avgs.csv"))

report_month_num <- as.numeric(format(today(),"%m")) - 1
report_month <- month.abb[report_month_num]

# Create Report table
five_yr_avgs_report_month <- five_yr_avgs[, report_month]
report_month_avgs <- monthly_data_combined[[as.character(year_list[1])]][, report_month_num]
report_trends <- mapply(function(x,y) {
  if (x > y) {
    "HIGHER"
  } else if (x < y) {
    "LOWER"
  } else {
    "SAME"
  }
}, report_month_avgs, five_yr_avgs_report_month)

final_report <- data.frame(Disease = disease_list, Current = report_month_avgs, Historical = five_yr_avgs_report_month, Trend = report_trends)

write.csv(final_report, paste0(internal_report_folder, "final_report.csv"))

print(final_report)

