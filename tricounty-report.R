library(lubridate)
library(writexl)

# ------------------------------------------------------------------------------
# Set up file system -----------------------------------------------------------
input_data_folder <- "input_epitrax_data"
processed_data_folder <- "processed_epitrax_data"
internal_reports_folder <- "internal_reports"
public_reports_folder <- "public_reports"

xl_files <- list() # Internal reports to combine into single .xlsx file

# - Create folders if needed
for (f in c(input_data_folder, processed_data_folder, 
            internal_reports_folder, public_reports_folder)) {
  if (!dir.exists(f)) {
    dir.create(f)
  }
}

# - Remove old internal reports
do.call(file.remove, list(list.files(internal_reports_folder, full.names = TRUE)))
# - Remove old public reports
do.call(file.remove, list(list.files(public_reports_folder, full.names = TRUE)[grepl("public_report_", list.files(public_reports_folder, full.names = TRUE))]))

# ------------------------------------------------------------------------------
# Define Helper Functions ------------------------------------------------------

# Writes report CSV files
write_report_csv <- function(data, filename, is.public = FALSE) {
  write.csv(data,
            file.path(ifelse(is.public, 
                             public_reports_folder, 
                             internal_reports_folder),
                      filename),
            row.names = FALSE)
}

# Validates input EpiTrax data
validate_data <- function(data) {
  # Check column names
  expected_cols <- c("patient_mmwr_year", "patient_mmwr_week", "patient_disease")
  actual_cols <- colnames(data)
  
  if (!all(expected_cols %in% actual_cols)) {
    stop("The EpiTrax data is missing one of the following fields:\n\n\t",
         paste(expected_cols, collapse=", "), 
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
  # - Note this also rearranges the columns into the order of expected_cols
  data <- data[expected_cols]
  
  data
}

# Formats input EpiTrax data using 'patient_mmwr_week' column
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
  # - Rearrange columns for easier debugging
  data <- data[c("disease", "month", "year", "counts")]

  # - Extract last years of data
  data <- with(data, data[year >= (max(year) - 5), ])

  data
}

# Reads in input EpiTrax data
read_epitrax_data <- function(input_folder, processed_folder = NULL) {
  # Get file name from input data folder
  fname <- list.files(input_folder)
  
  if (length(fname) == 0) {
    stop("No input file provided.")
  }
  
  # Check for only 1 file
  if (length(fname) > 1) {
    stop("Please include only 1 file in the '", input_folder, "' folder.")
  }
  
  # Check file has correct extension
  fpath <- file.path(input_folder, fname)
  if (!file.exists(fpath) || !grepl("\\.csv$", fpath)) {
    stop("Please add an EpiTrax data file (.csv) to the '", input_folder, 
         "' folder")
  }
  
  # Read data from file
  input_data <- read.csv(fpath, header = TRUE)
  
  # Validate and format data
  input_data <- format_week_num(input_data)
  
  # Move processed input file to processed_folder (if provided)
  if (!is.null(processed_folder)) {
    file.rename(fpath, file.path(processed_folder, fname))
  }
  
  # Return data from file
  input_data
}

# Reads in the public disease list (CSV)
# - This file contains two columns that map the EpiTrax disease name to
# - a public-facing name for the public report
get_public_disease_list <- function(default_diseases) {
  fname <- "disease_list_for_public_report.csv"
  fpath <- file.path(public_reports_folder, fname)
  
  if (file.exists(fpath)) {
    
    d_list <- read.csv(fpath, header = TRUE)
    
    # Validate file
    if (is.null(d_list$EpiTrax_name) || is.null(d_list$Public_name)) {
      stop("File '", fpath, "' is incorrectly formatted. Please use the column 
           names: 'EpiTrax_name' and 'Public_name'.")
    }
    
    d_list
    
  } else {
    # If the file doesn't exist, use the list of diseases in the input data
    warning("File '", fname, "' not found in '", public_reports_folder,
            "' folder. Using list from EpiTrax input dataset instead.")
    
    default_diseases <- sort(default_diseases)
    
    d_list <- data.frame(
      EpiTrax_name = default_diseases,
      Public_name = default_diseases
    )
    
    d_list
  }
}

# Reshape data frame with each month as a column
reshape_monthly_wide <- function(df) {
  m_df <- with(df, reshape(
    merge(
      df,
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
  colnames(m_df) <- c("disease", month.abb[1:(ncol(m_df) - 1)])
  
  m_df
}

# ------------------------------------------------------------------------------
# Read in EpiTrax data ---------------------------------------------------------
epitrax_data <- read_epitrax_data(input_data_folder)
epitrax_data_yrs <- sort(unique(epitrax_data$year))
epitrax_data_diseases <- unique(epitrax_data$disease)
report_year <- max(epitrax_data_yrs)

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
colnames(annual_counts) <- c("disease", epitrax_data_yrs)
# - Write to CSV
write_report_csv(annual_counts, "annual_counts.csv")
# - Add to Excel List
xl_files[["annual_counts"]] <- annual_counts

# ------------------------------------------------------------------------------
# Compute monthly counts for each year -----------------------------------------
month_counts <- aggregate(counts ~ disease + year + month, 
                          data = epitrax_data, 
                          FUN = sum)

for (y in epitrax_data_yrs) {
  # - Extract counts for given year
  m_df <- month_counts[month_counts$year == y, ]
  
  # - Remove year column (don't want to save it to CSV)
  m_df$year <- NULL
  
  # - Reshape data to use months as columns and disease as rows
  m_df <- reshape_monthly_wide(m_df)
  
  # - Write to CSV
  fname <- paste0("monthly_counts_", y)
  write_report_csv(m_df, paste0(fname, ".csv"))
  
  # - Add to Excel List
  xl_files[[fname]] = m_df
}

# ------------------------------------------------------------------------------
# Compute monthly averages for all years except current year -------------------
# - Extract all previous years
epitrax_data_prev_yrs <- epitrax_data[epitrax_data$year != report_year,]
num_yrs <- length(unique(epitrax_data_prev_yrs$year))

# - Compute average counts for each month
monthly_avgs <- aggregate(counts ~ disease + month, 
                          data = epitrax_data_prev_yrs, 
                          FUN = sum)

monthly_avgs$counts <- monthly_avgs$counts / num_yrs

# - Reshape data to use months as columns and disease as rows
monthly_avgs <- reshape_monthly_wide(monthly_avgs)

# - Write to CSV
avgs_fname <- with(epitrax_data_prev_yrs,
                   paste0("monthly_avgs_", min(year), "-", max(year), ".csv"))
write_report_csv(monthly_avgs, avgs_fname)

# - Add to Excel List
xl_files[["monthly_avgs"]] <- monthly_avgs

# - Combine internal reports into single .xlsx file
write_xlsx(xl_files, 
           file.path(internal_reports_folder, "internal_reports.xlsx"))

# ------------------------------------------------------------------------------
# Prepare Public Report --------------------------------------------------------
diseases <- get_public_disease_list(epitrax_data_diseases)

# - Remove rows from monthly_avgs that aren't going into the public report
monthly_avgs <- subset(monthly_avgs, disease %in% diseases$EpiTrax_name)

# - Get diseases from public report list that weren't in the EpiTrax data
missing_diseases <- subset(diseases, !(EpiTrax_name %in% monthly_avgs$disease))

# If there are any missing diseases, add them
if (length(missing_diseases$EpiTrax_name) > 0) {
  # - Fill the missing diseases in with avg = 0
  missing_avgs <- data.frame(
    disease = missing_diseases$EpiTrax_name
  )
  
  missing_cols <- colnames(monthly_avgs)[2:length(colnames(monthly_avgs))]
  missing_avgs[, missing_cols] <- 0.0
  
  # - Combine monthly_avgs with missing_avgs
  monthly_avgs <- rbind(monthly_avgs, missing_avgs)
  
  # - Sort alphabetically so missing diseases are correctly placed
  monthly_avgs <- monthly_avgs[order(monthly_avgs$disease),]
}

# - Find the previous month of the report year
report_month <- max(month_counts[month_counts$year == report_year, ]$month) - 1

# create_public_report() creates a public report for the given month
create_public_report <- function(month_num) {
  
  m_counts <- with(month_counts, 
                   month_counts[year == report_year & month == month_num,
                                c("disease","counts")])
  # - Only take the rows with data in the final report
  m_counts <- subset(m_counts, disease %in% monthly_avgs$disease)
  
  # - Create the report data frame initializing the Current_Rate column to 0
  m_report <- data.frame(
    Disease = monthly_avgs$disease,
    Current_Rate = 0, 
    Historical_Rate = round(monthly_avgs[month_num + 1], digits = 2)
  )
  
  # - Update the Current_Rate column with values from m_counts
  for (i in 1:length(m_counts$disease)) {
    d <- m_counts$disease[i]
    m_report[m_report$Disease == d, ]$Current_Rate <- m_counts$counts[i]
  }
  
  # - Add Trends column
  m_report$Trend <- mapply(function(x, y) {
    ifelse(x > y, "↑", ifelse(x < y, "↓", "→"))
  }, m_report$Current_Rate, m_report[[3]])
  
  # - Wait until final step to convert disease names to public-facing versions
  m_report$Disease <- diseases$Public_name
  
  # - Write to CSV file
  write_report_csv(
    m_report, 
    paste0("public_report_", colnames(m_report)[3], report_year, ".csv"),
    is.public = TRUE)
}

# - Create report table for most recent month and for 1 and 2 months prior
create_public_report(report_month)
create_public_report(report_month - 1)
create_public_report(report_month - 2)