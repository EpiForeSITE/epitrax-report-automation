# epitrax-report-automation
Automating Disease Surveillance Report Creation for Local Health Departments in Utah based on EpiTrax data.

This tool is a work in progress. Many of the functions used here will ultimately end up in an R package which will be wrapped in an R Shiny application for better usability. In the meantime, we hope this tool will enable local health officials to automate parts of their report generation process.

You shouldn't have to write any code to run the program! The tool has been designed for use by local health officials with little to no coding experience. Once you have the R script loaded on your machine and the correct libraries installed, you'll simply run the code from within RStudio and it will generate your reports.

## Installation
1. Download and Install R: https://cran.rstudio.com/
2. Download and Install RStudio: https://posit.co/download/rstudio-desktop/
3. Create a folder on your desktop to hold the R code and reports.
4. Copy/Move the R script [`epitrax-report-generator.R`](epitrax-report-generator.R) into the project folder.
5. Open the folder as a project in RStudio: "File" > "Open Project" > "Project Folder Name"
6. Open the [`epitrax-report-generator.R`](epitrax-report-generator.R) script: "File" > "Open File" > "epitrax-report-generator.R"
7. Install the required packages.
  - When you open the R script, RStudio may display a message that says "Packages lubridate, writexl, and yaml are required but not installed." If you see this message, click "Install" to install the packages.
  - Alternatively, copy the following line into RStudio's "Console" tab (bottom left pane) and press "Enter":
```
install.packages(c("lubridate", "writexl", "yaml"))
```

## Usage

The program requires certain folders and files to execute properly:
- Folders:
  - `input_epitrax_data/`
  - `report_settings/`
  - `internal_reports/`
  - `public_reports/`
  - `processed_epitrax_data/`
- Files:
  - Input EpiTrax CSV data file (required)
  - `internal_report_diseases.csv` (optional)
  - `public_report_diseases.csv` (optional)
  - `report_config.yaml` (optional)

These files and folders are described below.

The very first time you run the program, it will create these folders (if they don't already exist) in the same directory where you've stored the R script (e.g., the Project Folder from Installation Step 3). It will then fail with an error message because there is no input file to use for report generation. After you provide an input file (as described below), you will be able to execute the program properly.

Thus, the very first time you run the program, you will need to run it twice---once to setup the folders and a second time to generate your first batch of reports. After that, you'll only run it once with new input data.

### Executing the Script
In RStudio, "Run" is used to execute a *portion* of code from an R file. To execute the *entire* script, we use "Source" in one of two ways:
1. Click "Source" in the corner of the top left pane (where the `epitrax-report-generator.R` code is displayed)
2. Go to "Code" > "Source".

Again, the first time you source `epitrax-report-generator.R`, you will see an error in the "Console" tab describing a missing input file. The script has created the right folders, but you need to populate them with your files. Then source `epitrax-report-generator.R` again.

### Add Input File
Place the EpiTrax data file from which you would like to generate reports in the `input_epitrax_data/` folder.
- **NOTE:** There can only be 1 input file in this folder when the program runs or it will return an error. 

After executing the program, the input data file will be moved to the `processed_epitrax_data/` folder. This is to give you confirmation that the data was processed. It also ensures that the next time you run the program, you don't have first to move the old input file out of the input folder.

### Add Report Settings Files
In the `report_settings/` folder, you may add three **OPTIONAL** files:
1. `internal_report_diseases.csv`: Lists the diseases to include in internal reports as they are represented in EpiTrax.
    - If not provided, the program will default to using whatever diseases are found in the input dataset.
    - This file should have one column: 
       - `EpiTrax_name`: A list of all diseases to include in the report **AS THEY ARE NAMED IN EPITRAX**.
2. `public_report_diseases.csv`: Lists the diseases to include in public reports as they are represented in EpiTrax.
    - If not provided, the program will default to using whatever diseases are found in the input dataset.
    - This file should have two columns:
      - `EpiTrax_name`: A list of all diseases to include in the report **AS THEY ARE NAMED IN EPITRAX**.
      - `Public_name`: How each disease should be named in the report. For example, converting "Chlamydia trachomatis infection" to "Chlamydia".
      - **NOTE:** If multiple diseases have the same `Public_name` their report entries will be combined. For example, to combine "Syphilis, primary" and "Syphilis, secondary" into "Syphilis", simply set the `Public_name` of both diseases to "Syphilis".
3. `report_config.yaml`: Provides additional configuration information, such as:
    - `county_population`: Local population to use for converting case counts to Rates per 100k (defaults to 100k, if not provided)
    - `rounding_decimals`: How many digits to round the decimals to (defaults to 2, if not provided)

### Important Things to Keep in Mind

- Newly generated reports are placed in the `internal_reports` and `public_reports` folders.
    - **DO NOT** place any of your own files in these folders as **EVERYTHING** in these folders is **DELETED** each time the program runs.
    - That includes old reports (i.e., reports from previous program runs). If you want to save old reports, you will need to move them out of the `internal_reports` and `public_reports` folders before running the program.
- Rows with missing or NA values in the input dataset will be ignored and a warning will be printed to the Console. The warning is intended to inform the user that their input had missing/NA values, in case the user wants to correct that (and re-run the report generation). It does not mean the program was unable to generate reports from the rest of the data.
- Reports (both internal and public) might include diseases for which there are no case data in the input dataset. In this case, their values will simply be 0s.
