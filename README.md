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

### Input Data
The program expects an input file in CSV format that contains EpiTrax data with the following columns:
- `patient_mmwr_week` (integer)
- `patient_mmwr_year` (integer)
- `patient_disease` (character)

For example:
```csv
"patient_mmwr_week","patient_mmwr_year","patient_disease"
"26","2020","Chlamydia trachomatis infection"
```

### Executing the Script
In RStudio, "Run" is used to execute a *portion* of code from an R file. To execute the *entire* script, we use "Source" in one of two ways:
1. Click "Source" in the corner of the top left pane (where the `epitrax-report-generator.R` code is displayed)
2. Go to "Code" > "Source".

When you run the program, it will ask you for an input file to use for report generation. Find your EpiTrax CSV data file and select it. The program will then generate reports based on the data in that file.

**Note:** The very first time you run the program, it will create three folders in the same directory where you're running the R script (e.g., the Project Folder from Installation Step 3). These folders are:
- `report_settings/`: Holds settings files for report generation.
- `internal_reports/`: Holds reports intended for internal use by the health department.
- `public_reports/`: Holds reports intended for public use.


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
    - `current_population`: Population to use for converting case counts for the **current year** to Rates per 100k
      - If not provided, defaults to 100k.
    - `avg_5yr_population`: Population to use for converting case counts for the **5yr historical average** to Rates per 100k
      - If not provided, defaults to `current_population`.
    - `rounding_decimals`: How many digits to round decimals
      - If not provided, defaults to 2.
    - `generate_csvs`: Whether to generate CSV files for internal reports. When FALSE, only Excel files will be generated.
      - If not provided, defaults to `TRUE`.

### Important Things to Keep in Mind

- Newly generated reports are placed in the `internal_reports` and `public_reports` folders.
    - **DO NOT** place any of your own files in these folders as **EVERYTHING** in these folders is **DELETED** each time the program runs.
    - That includes old reports (i.e., reports from previous program runs). If you want to save old reports, you will need to move them out of the `internal_reports` and `public_reports` folders before running the program.
- Rows with missing or NA values in the input dataset will be ignored and a warning will be printed to the Console. The warning is intended to inform the user that their input had missing/NA values, in case the user wants to correct that (and re-run the report generation). It does not mean the program was unable to generate reports from the rest of the data.
- Reports (both internal and public) might include diseases for which there are no case data in the input dataset. In this case, their values will simply be 0s.
