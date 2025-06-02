library(lubridate)
library(writexl)

# ------------------------------------------------------------------------------
# Set up file system -----------------------------------------------------------
# - Create folders if needed
input_data_folder <- "input_epitrax_data"
if (!dir.exists(input_data_folder)) {
  dir.create(input_data_folder)
}
processed_data_folder <- "processed_epitrax_data"
if (!dir.exists(processed_data_folder)) {
  dir.create(processed_data_folder)
}
internal_reports_folder <- "internal_reports"
if (!dir.exists(internal_reports_folder)) {
  dir.create(internal_reports_folder)
}
public_reports_folder <- "public_reports"
if (!dir.exists(public_reports_folder)) {
  dir.create(public_reports_folder)
}
# - Define helper functions for writing report CSV files
write_internal_report <- function(data, filename) {
  write.csv(data,
            file.path(internal_reports_folder, filename),
            row.names = FALSE)
}
write_public_report <- function(data, filename) {
  write.csv(data,
            file.path(public_reports_folder, filename),
            row.names = FALSE)
}
# - List of internal reports that will be combined into single .xlsx file
xl_files <- list()

# ------------------------------------------------------------------------------
# Define Helper Functions ------------------------------------------------------

# Function to validate data
validate_data <- function(data) {
  # Check column names
  expected_colnames <- c("patient_mmwr_year", "patient_mmwr_week", "patient_disease")
  actual_colnames <- colnames(data)
  
  if (!all(expected_colnames %in% actual_colnames)) {
    stop("The EpiTrax data is missing one of the following fields:\n\n\t",
         paste(expected_colnames, collapse=", "), 
         "\n\nPlease add the missing fields to the file and try again.")
  }
  # Check column data types
  if (class(data$patient_mmwr_week) != "integer" ||
      class(data$patient_mmwr_year) != "integer" ||
      class(data$patient_disease) != "character") {
    stop("One or more columns in the EpiTrax dataset has an incorrect 
         data type:\n\n",
         "\t'patient_mmwr_week' should be of type 'integer'\n",
         "\t'patient_mmwr_year' should be of type 'integer'\n",
         "\t'patient_disease' should be of type 'character'\n",
         "\nPlease try again with a valid dataset."
    )
  }
  # Remove all columns we're not using
  # - Note this also rearranges the columns into the order we expect below
  data <- data[expected_colnames]
  
  data
}

# Function to format EpiTrax data using 'patient_mmwr_week' column
format_week_num <- function(data) {
  # Validate data
  data <- validate_data(data)
  
  # Format data
  data$month <- with(data, month(
    ymd(patient_mmwr_year * 10000 + 0101) + 
      patient_mmwr_week * 7
  ))
  data$patient_mmwr_week <- NULL
  data$counts <- 1 # Makes easier to use aggregate()
  colnames(data) <- c("year", "disease", "month", "counts")

  data
}

# Function to read in input EpiTrax file
read_epitrax_data <- function() {
  # Get file name from input data folder
  fname <- list.files(input_data_folder)
  
  # Check for only 1 file
  if (length(fname) > 1) {
    stop("Please include only 1 file in the '", input_data_folder, "' folder.")
  }
  
  # Check file has correct extension
  fpath <- file.path(input_data_folder, fname)
  if (!file.exists(fpath) || !grepl("\\.csv$", fpath)) {
    stop("Please add an EpiTrax data file (.csv) to the '", input_data_folder, 
         "' folder")
  }
  
  # Read data from file
  input_data <- read.csv(fpath, header = TRUE)
  
  # Validate and format data
  input_data <- format_week_num(input_data)
  
  # Move processed input file to processed_data_folder
  # - Comment to disable this behavior
  # file.rename(fpath, file.path(processed_data_folder, fname))
  
  # Return data from file
  input_data
}

# Function to read in the public disease list (CSV)
get_public_disease_list <- function(epitrax_data) {
  fname <- "disease_list_for_public_report.csv"
  fpath <- file.path(public_reports_folder, fname)
  
  # If the file doesn't exist, the function will return the list of diseases
  # - from the input EpiTrax dataset instead and give the user a warning
  if (!file.exists(fpath) || !grepl("\\.csv$", fpath)) {
    warning("File '", fname, "' not found in '", public_reports_folder,
            "' folder. Using list from EpiTrax input dataset instead.")
    
    diseases <- sort(unique(epitrax_data$disease))
    disease_list <- data.frame(
      EpiTrax_name = diseases,
      Public_name = diseases
    )
    
    disease_list
  } else {
    disease_list <- read.csv(fpath, header = TRUE)
    
    # Validate file
    if (is.null(disease_list$EpiTrax_name) || is.null(disease_list$Public_name)) {
      stop("File '", fpath, "' is incorrectly formatted. Please use the column 
           names: 'EpiTrax_name' and 'Public_name'.")
    }
    
    disease_list
  }
}

# ------------------------------------------------------------------------------
# Read in EpiTrax data ---------------------------------------------------------
epitrax_data <- read_epitrax_data()

# Rearrange columns (for DEBUGGING)
epitrax_data <- epitrax_data[,c("disease", "month", "year", "counts")]

# ------------------------------------------------------------------------------
# Compute annual counts for each disease ---------------------------------------
annual_counts <- aggregate(counts ~ disease + year, 
                           data = epitrax_data, 
                           FUN = sum)
# - Reshape data to use years as columns and diseases as rows
annual_counts <- with(annual_counts, reshape(
  merge(
    annual_counts,
    expand.grid(
      disease = unique(disease),
      year = unique(year)
    ),
    all = TRUE
  ),
  direction = "wide",
  idvar = "disease",
  timevar = "year"
))
# - Set NA values to 0
annual_counts[is.na(annual_counts)] <- 0
# - Update column names to more human-readable format
colnames(annual_counts) <- c("disease", sort(unique(epitrax_data$year)))
# - Write to CSV
write_internal_report(annual_counts, "annual_counts.csv")
# - Add to Excel List
xl_files[["annual_counts"]] <- annual_counts

# ------------------------------------------------------------------------------
# Compute monthly counts for each year -----------------------------------------
monthly_counts <- aggregate(counts ~ disease + year + month, 
                            data = epitrax_data, 
                            FUN = sum)

for (y in sort(unique(epitrax_data$year))) {
  # - Extract counts for given year
  m_df <- monthly_counts[monthly_counts$year == y, ]
  # - Remove year column (don't want to save it to CSV)
  m_df$year <- NULL
  # - Reshape data to use months as columns and disease as rows
  m_df <- with(m_df, reshape(
    merge(
      m_df,
      expand.grid(
        disease = unique(disease),
        month = unique(month)
      ),
      all = TRUE
    ),
    direction = "wide",
    idvar = "disease",
    timevar = "month"
  ))
  # - Set NA values to 0
  m_df[is.na(m_df)] <- 0
  # - Update column names to more human-readable format
  colnames(m_df) <- c("disease", month.abb[1:(ncol(m_df)-1)])
  
  # - Write to CSV
  fname <- paste0("monthly_counts_", y)
  write_internal_report(m_df, paste0(fname, ".csv"))
  
  # - Add to Excel List
  xl_files[[fname]] = m_df
}

# ------------------------------------------------------------------------------
# Compute monthly averages for all years except current year -------------------
# - Extract all previous years
epitrax_data_5yr <- with(epitrax_data, epitrax_data[year != max(year),])

# - Compute average counts for each month
monthly_5yr_avgs <- aggregate(counts ~ disease + month, 
                              data = epitrax_data_5yr, 
                              FUN = sum)
num_yrs <- length(unique(epitrax_data_5yr$year))
monthly_5yr_avgs$counts <- monthly_5yr_avgs$counts / num_yrs
# - Reshape data to use months as columns and disease as rows
monthly_5yr_avgs <- with(monthly_5yr_avgs, reshape(
  merge(
    monthly_5yr_avgs,
    expand.grid(
      disease = unique(disease),
      month = unique(month)
    ),
    all = TRUE
  ),
  direction = "wide",
  idvar = "disease",
  timevar = "month"
))
# - Set NA values to 0
monthly_5yr_avgs[is.na(monthly_5yr_avgs)] <- 0
# - Update column names to more human-readable format
colnames(monthly_5yr_avgs) <- c("disease", 
                                month.abb[1:(ncol(monthly_5yr_avgs) - 1)])
# - Write to CSV
avgs_fname <- with(epitrax_data_5yr,
                   paste0("monthly_avgs_", min(year), "-", max(year), ".csv"))
write_internal_report(monthly_5yr_avgs, avgs_fname)

# - Add to Excel List
xl_files[["monthly_5yr_avgs"]] <- monthly_5yr_avgs

# - Combine internal reports into single .xlsx file
write_xlsx(xl_files, file.path(internal_reports_folder, "internal_reports.xlsx"))

# ------------------------------------------------------------------------------
# Prepare Public Report --------------------------------------------------------
public_disease_list <- get_public_disease_list(epitrax_data)

# - Remove rows from monthly_5yr_avgs that aren't going into the public report
monthly_5yr_avgs <- subset(monthly_5yr_avgs, 
                           disease %in% public_disease_list$EpiTrax_name)
# - Get diseases from public report list that weren't in the EpiTrax data
missing_diseases <- subset(public_disease_list, 
                           !(EpiTrax_name %in% monthly_5yr_avgs$disease))

# If there are any missing diseases, add them
if (length(missing_diseases$EpiTrax_name) > 0) {
  # - Fill the missing diseases in with avg = 0
  missing_5yr_avgs <- data.frame(
    disease = missing_diseases$EpiTrax_name
  )
  present_colnames <- colnames(monthly_5yr_avgs)
  missing_colnames <- present_colnames[2:length(present_colnames)]
  missing_5yr_avgs[, missing_colnames] <- 0.0
  
  # - Combine monthly_5yr_avgs with missing_5yr_avgs
  monthly_5yr_avgs <- rbind(monthly_5yr_avgs, missing_5yr_avgs)
  # - Sort alphabetically so missing diseases are correctly placed
  monthly_5yr_avgs <- monthly_5yr_avgs[order(monthly_5yr_avgs$disease),]
}

# - Most recent month is the maximum month included in the data minus 1
current_report_year <- max(monthly_counts$year)
current_report_month <- max(
  monthly_counts[monthly_counts$year == current_report_year
                 ,]$month) - 1

# create_public_report() creates a public report for the given month
create_public_report <- function(month_num) {
  current_month_counts <- with(monthly_counts, 
                               monthly_counts[
                                 year == current_report_year & month == month_num,
                                 c("disease","counts")])
  current_month_counts <- subset(current_month_counts, 
                                 disease %in% monthly_5yr_avgs$disease)
  
  # - Create the report data frame initializing the Current_Rate column to 0
  current_report <- data.frame(
    Disease = monthly_5yr_avgs$disease,
    Current_Rate = 0, 
    Historical_Rate = monthly_5yr_avgs[month_num + 1]
  )
  
  # - Update the Current_Rate column with values from current_month_counts
  for (i in 1:length(current_month_counts$disease)) {
    current_report[current_report$Disease == current_month_counts$disease[i]
                   , ]$Current_Rate <- current_month_counts$counts[i]
  }
  
  # - Add Trends column
  current_report$Trend <- mapply(function(x, y) {
    ifelse(x > y, "↑", ifelse(x < y, "↓", "-"))
  }, current_report$Current_Rate, current_report[[3]])
  
  # - Wait until final step to convert disease names to public-facing versions
  current_report$Disease <- public_disease_list$Public_name
  
  # - Write to CSV file
  write_public_report(
    current_report, 
    paste0("public_report_",
           colnames(current_report)[3],
           current_report_year,
           ".csv")
    )
}

# - Create report table for most recent month
create_public_report(current_report_month)

# - Create report table for one month prior
create_public_report(current_report_month - 1)

# - Create report table for two months prior
create_public_report(current_report_month - 2)



