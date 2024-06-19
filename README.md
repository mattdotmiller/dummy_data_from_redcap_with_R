# Simulated Data from a REDCap Data Dictionary

This project aims to create a simulated dataset for any research project using a REDCap data dictionary as a standardised template for the data-fields. Research team members could then use this simulated data to practice their analysis.

  

**Flow of data**

Generally, it would be expected that the data dictionary is created as part of the study design of the research project before data collection. If a data file exists already, for example, from a pilot study, then a draft data dictionary can be created using the draft\_redcap\_dd function. This can then be updated with the new study variables.

  

**The REDCap data dictionary**

REDCap data dictionaries are .csv files that can be uploaded to a REDCap project to create the data collection instruments. They need to be formatted correctly for this to work. An example data dictionary is provided in the files (example REDCap DD PONV), as is a template that can be downloaded and edited for new projects (blank Redcap DD.csv).

  

The columns that must be completed for the create\_dummy\_data function to work are:

| **Column** | **Notes** |
| ---| --- |
| Variable / Field Name | lower case with "\_" instead of a space between words |
| Field Type | chose from text, dropdown, checkbox |
| Text Validation Type OR Show Slider Number | for numbers either:integer, number<br>for date/time the available formats are: date\_dmy, date\_mdy, date\_ymd, datetime\_seconds\_dmy, datetime\_seconds\_ymd, datetime\_seconds\_mdy, time\_hh\_mm\_ss, time, time\_mm\_ss, datetime\_mdy, datetime\_dmy, datetime\_ymd<br>for categorical<br> |
| Text Validation Min | needed for numerical variables |
| Text Validation Max | needed for numerical variables |

  

Note: Calculated fields are not used in this project as they can be produced using standard R scripts using mutate from dplyr package

  

Summarised description of all the columns used in a REDCap are in a table at the bottom of this file

  

  

| **Column cell** | **Column name** | **Required (R) or optional (O)** | **Explanation** |
| ---| ---| ---| --- |
| A | Variable / Field Name | R | lowercase and "\_" not " " eg first\_name not first name<br>Used for reporting, export, and analysis, not displayed in entry forms.<br>Allowed characters: letters, numbers, underscores (no spaces or special characters).<br>Cannot start with a number; must be unique in the database.<br>Brief names are preferred; detailed descriptions in Variable Labels.<br>Changing a name requires updates across all usage instances (e.g., calculations, logic).<br> |
| B | Form Name | R | Forms group database variables for efficient data entry and storage.<br>Use short forms for easier data entry and frequent data saving.<br>Form names: all lowercase in Excel, displayed in REDCap with initial capitals.<br>For multi-word names, connect with an underscore; it appears as a space in REDCap.<br>Variables within a form must be in consecutive rows in the data dictionary for proper display order.<br> |
| C | Section Header | O | Section Headers visually separate items within a form, aiding data entry.<br>Useful for displaying interview scripts or introducing new topics during data entry in REDCap.<br> |
| D | Field Type | R | Types include dropdown, radio button, checkboxes, text box, note box, calculated field, file upload, and section header.<br>Categorical types (dropdown, radio buttons, checkboxes) require defined response options in Column G.<br>Text field types (text box, note box) benefit from validation (Column I), possibly with specified minimum and maximum values (Columns J & K). These cannot have choices in Column G.<br>Calculated fields display calculated results and don't allow data entry.<br>File upload permits document attachment (max 50MB).<br> |
| E | Field Label | R | Field Label provides descriptive information displayed on the form.<br>Offers more detail than the variable/field name for reader comprehension.<br>Can be the same as Column A if needed for efficiency<br> |
| F | Choices, Calculations, OR Slider Labels | R | Categorical field types need numerical value association with labels in the format number, text | number, text . For example: 1, yes | 2, no |  or: 1, general | 2, orthopedics | 3, obs and gyne<br>Calculated fields require the specific calculation syntax.<br>Slider fields necessitate labeling for three anchor points (left, middle, right).<br> |
| G | Field Note | O | Field notes offer guidance for data entry, such as expected formats or units (e.g., phone number format, kg vs. lb). |
| H | Text Validation Type OR Show Slider Number | O | Text field validation types include date, time, integer, number, zipcode, phone, and email.<br>Slider fields can specify showing or hiding the selected value (1-100).<br>For date/time the available formats are: date\_dmy, date\_mdy, date\_ymd, datetime\_seconds\_dmy, datetime\_seconds\_ymd, datetime\_seconds\_mdy, time\_hh\_mm\_ss, time, time\_mm\_ss, datetime\_mdy, datetime\_dmy, datetime\_ymd<br> |
| I | Text Validation Min | O | minimum and maximum acceptable values |
| J | Text Validation Max | O | minimum and maximum acceptable values |
| K | Identifier? | O | For HIPAA compliance, certain information should be marked as 'identifiers' in REDCap. These include:<br>Name<br>Fax number<br>Phone number<br>E-mail address<br>Account numbers<br>Social Security number<br>Medical Record number<br>Health Plan number<br>Certificate/license numbers<br>URL<br>IP address<br>Vehicle identifiers<br>Device ID<br>Biometric ID<br>Full face/identifying photo<br>Other unique identifying number, characteristic, or code<br>Postal address (geographic subdivisions smaller than state)<br>Date precision beyond year."<br> |
| L | Branching Logic (Show field only if...) | O | Controls field display based on values in previous fields.<br>Syntax details are available in the REDCap Help Section<br> |
| M | Required Field? | O | Designate as "required" if an error message is needed to be displayed if the field is left blank.<br> |
| N | Custom Alignment | O | Specifies the location of text boxes or categorical responses.<br>Options include Right/Vertical, Left/Vertical, Right/Horizontal, Left/Horizontal.<br>Default setting: Right/Vertical if not specified<br> |
| O | Question Number (surveys only) | O | Auto-numbering available in REDCap for survey questions.<br>Custom numbering scheme can be specified in this field if preferred<br> |
| P | Matrix Group Name | O | See REDCap help for this |
| Q | Matrix Ranking? | O | See REDCap help for this |
| R | Field Annotation | O | Annotations help clarify field usage, reduce errors, and document projects for future reference.<br>Not displayed except on project metadata<br> |

  

### Summary of the steps

This script processes a REDCap Data Dictionary and generates a simulated dataset based on the defined variables. It includes the following main steps:

1. **Load Necessary Packages**: The script uses several R packages to handle data manipulation, cleaning, and simulation. If not already installed, you may need to install the `pacman` package to manage these dependencies.
2. **Load and Pre-process Data Dictionary**:
    *   The script reads in a REDCap Data Dictionary CSV file.
    *   It cleans the column names and updates certain fields based on conditions (e.g., setting choices for yes/no fields).
3. **Add Required Columns**: Extra columns needed for simulation (e.g., distribution type, mean, standard deviation) are added to the Data Dictionary.
4. **Classify Variables**: Variables are classified into types such as numeric, categorical, yes/no, and date/time based on their attributes.
5. **Manual Corrections**: The script identifies variables that need manual corrections, which are then saved to a CSV file for manual updating.
6. **Update Data Dictionary**: The manually updated data is reloaded and integrated back into the main Data Dictionary.
7. **Simulate Data**:
    *   **Categorical Variables**: Generates simulated data for categorical variables based on defined probabilities.
    *   **Numeric Variables**: Generates simulated data for numeric variables using normal or uniform distributions.
    *   **Date/Time Variables**: Generates simulated data for date and time variables within specified ranges.
8. **Combine Simulated Data**: All simulated data (categorical, numeric, date/time) is combined into a single dataset.
9. **Post-processing**:
    *   Ensures all variables from the original Data Dictionary are included in the simulated dataset.
    *   Adds a unique record identifier.
    *   Converts categorical variables to the appropriate format required by REDCap (e.g., checkboxes).
10. **Save the Simulated Dummy Dataset**: The final simulated dataset is saved as a CSV file.

### Usage Instructions

  

1. Ensure all necessary R packages are installed. The script uses `pacman` to load the required packages.
2. Place your REDCap Data Dictionary CSV file in the working directory.
3. Run through the script step by step, there are comments to guide you
4. you will need to review two CSV files generated for manual corrections of probability distributions of the categorical data and mean, SD, max and min values for the numeric data. Update them as needed.
5. You can also use existing data to obtain some of the probability distribution and summary statistics data.
6. Reload the manually corrected files and run the remaining part of the script to complete the data simulation process.