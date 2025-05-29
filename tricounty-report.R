library(lubridate)

# Set up file system
input_data_folder <- "input_epitrax_data/"
if (!dir.exists(input_data_folder)) {
  dir.create(input_data_folder)
}
processed_data_folder <- "processed_epitrax_data/"
if (!dir.exists(processed_data_folder)) {
  dir.create(processed_data_folder)
}
internal_report_folder <- "internal_reports/"
if (!dir.exists(internal_report_folder)) {
  dir.create(internal_report_folder)
}
public_reports_folder <- "public_reports/"
if (!dir.exists(public_reports_folder)) {
  dir.create(public_reports_folder)
}

# Function to read in input EpiTrax file
read_epitrax_data <- function() {
  # Get file name from input data folder
  fname <- list.files(input_data_folder)
  
  # Check for only 1 file
  if (length(fname) > 1) {
    stop(paste0("Please include only 1 file in the '", input_data_folder, "' folder."))
  }
  
  # Check file has correct extension
  fpath <- paste0(input_data_folder, fname)
  if (!file.exists(fpath) || !grepl("\\.csv$", fpath)) {
    stop(paste0("Please add an EpiTrax data file (.csv) to the '", input_data_folder, "' folder"))
  }
  
  # Read data from file
  input_data <- read.csv(fpath, header = TRUE)
  
  # TODO: Verify data file has correct fields
  
  # Move processed input file to processed_data_folder
  # - Comment to disable this behavior
  # file.rename(fpath, paste0(processed_data_folder, fname))
  
  # Return data from file
  input_data
}

# - Read in EpiTrax data
disease_data <- read.csv(paste0(input_data_folder, "export_7404_051525123227.csv"), header = TRUE)
# epitrax_data <- read.csv(paste0(input_data_folder, "EpitraxExport_5yrs_Onset.csv"), header = TRUE)
epitrax_data <- read.csv(paste0(input_data_folder, "export_7404_051525123227.csv"), header = TRUE)

# Format EpiTrax Data
# - Format with onset_date
# epitrax_data$year <- as.integer(format(as.Date(epitrax_data$patient_disease_onset_date,"%m/%d/%Y"), "%Y"))
# epitrax_data$month <- as.integer(format(as.Date(epitrax_data$patient_disease_onset_date,"%m/%d/%Y"), "%m"))
# epitrax_data$patient_disease_onset_date <- NULL
# epitrax_data$patient_mmwr_year <- NULL
# colnames(epitrax_data) <- c("disease", "year", "month")
# - Format with patient_mmwr_week
epitrax_data$month <- month(ymd(epitrax_data$patient_mmwr_year * 10000 + 0101) + epitrax_data$patient_mmwr_week * 7)
epitrax_data$patient_mmwr_week <- NULL
epitrax_data$counts <- 1 # Makes easier to use aggregate()
colnames(epitrax_data) <- c("year", "disease", "month", "counts")

# Rearrange columns (for DEBUGGING)
epitrax_data <- epitrax_data[,c("disease", "month", "year", "counts")]

# Compute annual counts for each disease ---------------------------------------
annual_counts <- aggregate(counts ~ disease + year, data = epitrax_data, FUN = sum)
# - Reshape data to use years as columns and diseases as rows
annual_counts <- reshape(
  merge(
    annual_counts,
    expand.grid(
      disease = unique(annual_counts$disease),
      year = unique(annual_counts$year)
    ),
    all = TRUE
  ),
  direction = "wide",
  idvar = "disease",
  timevar = "year"
)
# - Set NA values to 0
annual_counts[is.na(annual_counts)] <- 0
# - Update column names to more human-readable format
colnames(annual_counts) <- c("disease", sort(unique(epitrax_data$year)))
# - Write to CSV
write.csv(annual_counts, paste0(internal_report_folder,"annual_counts.csv"), row.names = FALSE)

# Compute monthly counts for each year -----------------------------------------
monthly_counts <- aggregate(counts ~ disease + year + month, data = epitrax_data, FUN = sum)

for (y in sort(unique(epitrax_data$year))) {
  # - Extract counts for given year
  m_df <- monthly_counts[monthly_counts$year == y, ]
  # - Remove year column (don't want to save it to CSV)
  m_df$year <- NULL
  # - Reshape data to use months as columns and disease as rows
  m_df <- reshape(
    merge(
      m_df,
      expand.grid(
        disease = unique(m_df$disease),
        month = unique(m_df$month)
      ),
      all = TRUE
    ),
    direction = "wide",
    idvar = "disease",
    timevar = "month"
  )
  # - Set NA values to 0
  m_df[is.na(m_df)] <- 0
  # - Update column names to more human-readable format
  colnames(m_df) <- c("disease", month.abb[1:(ncol(m_df)-1)])
  
  # TODO: include full disease list, not just those in the dataset (use 0s)
  
  write.csv(m_df, paste0(internal_report_folder, "monthly_counts_", y, ".csv"), row.names = FALSE)
  
  # print(head(m_df))
}

# Compute monthly averages for all years except current year -------------------
# - Extract all previous years
epitrax_data_5yr <- epitrax_data[epitrax_data$year != max(epitrax_data$year),]
# - Compute average counts for each month
monthly_5yr_avgs <- aggregate(counts ~ disease + month, data = epitrax_data_5yr, FUN = sum)
monthly_5yr_avgs$counts <- monthly_5yr_avgs$counts / length(unique(epitrax_data_5yr$year))
# - Reshape data to use months as columns and disease as rows
monthly_5yr_avgs <- reshape(
  merge(
    monthly_5yr_avgs,
    expand.grid(
      disease = unique(monthly_5yr_avgs$disease),
      month = unique(monthly_5yr_avgs$month)
    ),
    all = TRUE
  ),
  direction = "wide",
  idvar = "disease",
  timevar = "month"
)
# - Set NA values to 0
monthly_5yr_avgs[is.na(monthly_5yr_avgs)] <- 0
# - Update column names to more human-readable format
colnames(monthly_5yr_avgs) <- c("disease", month.abb[1:(ncol(monthly_5yr_avgs)-1)])
# - Write to CSV
write.csv(monthly_5yr_avgs, paste0(internal_report_folder, "monthly_5yr_avgs.csv"), row.names = FALSE)

# Prepare Public Report
public_disease_list <- read.csv("other_data/public_report_disease_names.csv", header = TRUE)
# TODO: if this fails (doc doesn't exist, just skip this step, don't fail completely)

# - Remove rows from monthly_5yr_avgs that aren't going into the public report
monthly_5yr_avgs <- subset(monthly_5yr_avgs, monthly_5yr_avgs$disease %in% public_disease_list$EpiTrax_name)
# - Get diseases from public report list that weren't in the EpiTrax data
missing_diseases <- subset(public_disease_list, !(public_disease_list$EpiTrax_name %in% monthly_5yr_avgs$disease))

# - Fill the missing diseases in with avg = 0
missing_5yr_avgs <- data.frame(
  disease = missing_diseases$EpiTrax_name
)
missing_5yr_avgs[, colnames(monthly_5yr_avgs)[2:length(colnames(monthly_5yr_avgs))]] <- 0.0

# - Combine monthly_5yr_avgs with missing_5yr_avgs
monthly_5yr_avgs <- rbind(monthly_5yr_avgs, missing_5yr_avgs)
monthly_5yr_avgs <- monthly_5yr_avgs[order(monthly_5yr_avgs$disease),]


# - Most recent month is the maximum month included in the data minus 1
current_report_year <- max(monthly_counts$year)
current_report_month <- max(monthly_counts[monthly_counts$year == current_report_year,]$month) - 1

# create_public_report() creates a public report for the given month
# - TODO: Make this roll back a year if current report is Jan or Feb
create_public_report <- function(month_num) {
  current_month_counts <- monthly_counts[monthly_counts$year == current_report_year & monthly_counts$month == month_num,c("disease","counts")]
  current_month_counts <- subset(current_month_counts, current_month_counts$disease %in% monthly_5yr_avgs$disease)
  
  # - Create the report data frame initializing the Current_Rate column to 0
  current_report <- data.frame(
    Disease = monthly_5yr_avgs$disease,
    Current_Rate = 0, 
    Historical_Rate = monthly_5yr_avgs[month_num + 1]
  )
  
  # - Update the Current_Rate column with values from current_month_counts
  for (i in 1:length(current_month_counts$disease)) {
    current_report[current_report$Disease == current_month_counts$disease[i], ]$Current_Rate <- current_month_counts$counts[i]
  }
  
  # - Add Trends column
  current_report$Trend <- mapply(function(x,y) {
    if (x > y) {
      "HIGHER"
    } else if (x < y) {
      "LOWER"
    } else {
      "SAME"
    }
  }, current_report$Current_Rate, current_report[[3]])
  
  write.csv(current_report, paste0(public_reports_folder, "public_report_",   colnames(current_report)[3], current_report_year,".csv"), row.names = FALSE)
}

# - Create report table for most recent month
create_public_report(current_report_month)

# - Create report table for one month prior
create_public_report(current_report_month - 1)

# - Create report table for two months prior
create_public_report(current_report_month - 2)



