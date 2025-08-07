## **Sample Type Optimization Classifier**

An R Shiny application to automate duplicate biosample detection, enable bulk or manual sample selection for retention/discarding (prioritizing quality flags), and generate interactive summaries with downloadable Excel reports to streamline biobank storage management.

**An R Shiny app to automate duplicate‐detection and “keep vs. discard” decisions on large biosample datasets to 500,000 rows of data of long term storage samples.**

## **Features**
- **Upload Excel Files**: Accepts .xlsx biospecimen records with columns like sample type, screen ID, visit name, etc.
- **Automatic Duplicate Detection**: Identifies and allows removal of replicate (duplicate) rows.
- **Flexible Sample Classification**: Bulk classify samples by type as "keep", "discard", or "manual select" for per-visit/sample selection.
- **Interactive Summaries**: View classified data, grouped summaries, and visualizations of samples kept/discarded per sample type.
- **Downloadable Excel Reports**: Export updated data, grouped summaries, and instructions with embedded summary plots.
- **Local Data Processing**: All processing is local—your data is never stored outside your session.

## **Getting Started**
**Prerequisites** 
- R (>= 4.0.0 recommended)
- R Shiny
- Required packages:
- shiny
- readxl
- dplyr
- DT
- openxlsx
- tidyr
- ggplot2
- janitor
  
**Install any missing packages using:**

- R
- install.packages(c("shiny", "readxl", "dplyr", "DT", "openxlsx", "tidyr", "ggplot2", "janitor"))
- Running the App
  
**Installation Methods**
Method A: Run Directly from GitHub (Quick Start)
r
shiny::runGitHub(
  repo = "Biospecimen-Discard-Optimization-Classifier", 
  username = "BetsiRosiely",
  subdir = "app"  # Required because your app.R is inside /app
  
**Clone the repository:**  
bash
git clone https://github.com/BetsiRosiely/Biospecimen-Discard-Optimization-Classifier.git
Open the app/app.R file in RStudio or your preferred R environment.
Run the app:
R
shiny::runApp("app")

## **How to Use**

**Upload Data**: Upload an Excel file containing biospecimen records. Required columns (case-insensitive, snake_case preferred):
- sample_type
- screen_id
- visit_name
- (sample_comment is optional)
  
**Duplicate Detection**: The app automatically detects replicate rows. Optionally, remove duplicates.

**Sample Classification**:
- Use dropdowns to classify each sample type as "keep", "discard", or "manual select".
- For "manual select", define how many samples per visit per screen ID should be kept.

**Apply Classification**: Click "Apply Classification" to label rows with "keep" or "discard".

## **Review Summaries**:
- View grouped summary tables by sample type, screen ID, and visit name.
- Visualize totals kept vs discarded by sample type.

**Download Results**: Export updated Excel files with full data, grouped summaries, and instructions (including embedded summary plots).

**Output Files**
**Full_Data**: All rows with original and classification labels.
**Summary_by_Group**: Pivot table grouped by sample type, screen ID, and visit name, with counts of "keep" and "discard".
**Summary_Instructions**: Quick reference and usage instructions, plus summary totals and plot.

## Notes

Data is processed locally—no information is transmitted or stored externally.
The app supports column name variants for flexibility in input files.
For optimal results, ensure your Excel data is well-formatted and includes the required columns.
License
© 2025 Betsi Santos Rodriguez.
This project is licensed under the MIT License.

**Contact** 
For questions or feature requests, please open an issue or contact the repository owner.
Contact



