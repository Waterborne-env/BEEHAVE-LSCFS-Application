# BEEHAVE Model Validation: How To
Author: Colleen Roy

Last Edited: 2020-07-20


## Disclaimer
The software and associated files uploaded in this repository were used to generate the results to be published in:

Schmolke A, Abi-Akar F, Roy C, Galic N, Hinarejos S. Simulating honey bee large-scale colony feeding studies using the BEEHAVE model. Part I: model validation. Submitted.

Abi-Akar F, Schmolke A, Roy C, Galic N, Hinarejos S. Simulating honey bee large-scale colony feeding studies using the BEEHAVE model. Part II: analysis of overwintering outcomes. Submitted.

Both the manuscripts have been submitted for review to a scientific journal.

*This software and associated files are provided "as is" with the sole purpose to allow the reproduction of the published results without any warranties of performance or fitness for any other purpose.*

## Publication: Schmolke et al. (submitted)
### BEEHAVE by Study
*In order to run BEEHAVE you must:* Have NetLogo 5 installed (for runs in this study, NetLogo 5.3.1 was used). The link to download NetLogo 5.3.1 is here: https://ccl.northwestern.edu/netlogo/5.3.1/

#### Run Simulations
Files needed (put in the same folder):
- BEEHAVE_by_Study\Beehave_BeeMapp2015_LSCFSproject.nlogo (a modified version of the BEEHAVE program)
- BEEHAVE_by_Study\scenarios.xml
- Extracted zip file for study to be simulated (located in BEEHAVE_by_Study folder)

Open up the command prompt

Change directory to NetLogo's Location (file path will have to be updated to reflect user's NetLogo installation)

	cd C:\Program Files\NetLogo 5.3.1\app

Run bat file for the specific study (file path will have to be updated to reflect location of input files listed above)

	C:\BEEHAVEModelValidation\LSCFS_2013_1.bat

Each apiary from the selected study's result file will appear in the same directory

#### CompileOutput.R
**_Required R Packages:_** plyr

**_Required Files from Repository:_**

	read_beehave.R
	measured_data_range.csv

**_Initial Folder Locations/Names:_**

	Line 7: File path to folder with output from BEEHAVE runs (sim_fp)
	Line 8: File path to read_beehave.R (fun_fp)
	Line 9: File path to measured_data_range.csv (rmeas_fp)

### Goodness of Fit
#### 1. Data file
The calculation of the goodness-of-fit indicators uses the file “Validation_BEEHAVE_LSCFS_data.csv.” The file combines data of colony condition assessments (CCAs) conducted during the LSCFS and output data from apiary-specific simulations.

#### 2. Calculation of goodness-of-fit indicators not corrected for bias in BEEHAVE outputs
The script “Calc_GoodnessOfFitIndicators.R” reads in the data file, and calculates the goodness-of-fit indicators (NMAE, NRMSE, RSE) across the whole data set and defined subsets of data. Subsets by season (whole study period, first-year only, last assessment in fall only and spring after overwintering only, respectively) and subsets of studies, including all studies, validation studies, and study-specific calculations.
The output of the script is a table with the name “GoodnessOfFitIndicators_LSCFS.csv” which includes all calculated indicators for all studies and the data subsets.
The script uses “GoodnessOfFitIndicators_functions.R” in which the equations for the goodness-of -fit indicators are defined.

#### 3. Calculation of goodness-of-fit indicators with the correction for bias in BEEHAVE outputs
The script “Calc_GoodnessOfFitIndicators_biascorr.R” reads in the data file, and calculates the goodness-of-fit indicators (NMAE, NRMSE, RSE) across the whole data set and defined subsets of data. Subsets by season (whole study period, first-year only, last assessment in fall only and spring after overwintering only, respectively) and subsets of studies, including all studies, validation studies, and study-specific calculations.
The output of the script is a table with the name “GoodnessOfFitIndicators_LSCFS_biascorr.csv” which includes all calculated indicators for all studies and the data subsets.
The script uses “GoodnessOfFitIndicators_functions_biascorr.R” in which the equations for the goodness-of -fit indicators are defined.

#### 4. Plotting of goodness-of-fit indicators
The script “Plot_GoodnessOfFitIndicators.R” produces the plots that are presented in the supplemental material. The plots include goodness-of-fit indicators from both, BEEHAVE output corrected for bias and not corrected for bias. The scripts reads the data tables “GoodnessOfFitIndicators_LSCFS.csv” and “GoodnessOfFitIndicators_LSCFS_biascorr.csv” produced by the scripts described in point 2. and 3.

### Publication Figures
#### Figure1.R
**_Required R Packages:_** plyr, scales

**_Required Files from Repository:_**

	read_beehave.R
	measured_data.csv
	measured_data_range.csv

**_Initial Folder Locations/Names:_**

	Line 9: File path to folder where image will be written (outp_fp)
	Line 10: File path to folder with output from BEEHAVE runs (sim_fp)
	Line 11: File path to read_beehave.R (fun_fp)
	Line 12: File path to measured_data.csv (meas_fp)
	Line 13: File path to measured_data_range.csv (rmeas_fp)

#### Figure2.R & Figure3.R
**_Required R Packages:_** plyr

**_Required Files from Repository:_**

	measured_data.csv

**_Required Outputs from Repository:_**

	Validation_BEEHAVE_LSCFS_data.csv (from BEEHAVE_by_Study\CompileOutput.R)

**_Initial Folder Locations/Names:_**

	Line 7: File path to folder where image will be written (outp_fp)
	Line 8: File path to measured_data.csv (meas_fp)
	Line 9: File path to Validation_BEEHAVE_LSCFS_data.csv (val_fp)

#### Figure4.R & Figure5.R
**_Required R Packages:_** plyr

**_Required Outputs from Repository:_**

	GoodnessOfFitIndicators_LSCFS.csv (from Goodness_of_Fit\Calc_GoodnessOfFitIndicators.R)
	GoodnessOfFitIndicators_LSCFS_biascorr.csv (from Goodness_of_Fit\Calc_GoodnessOfFitIndicators_biascorr.R)

**_Initial Folder Locations/Names:_**

	Line 7: File path to folder where image will be written (outp_fp)
	Line 8: File path to output from Calc_GoodnessOfFitIndicators.R, GoodnessOfFitIndicators_LSCFS.csv (gofi_fp)
	Line 9: File path to output from Calc_GoodnessOfFitIndicators_biascorr.R, GoodnessOfFitIndicators_LSCFS_biascorr.csv (bias_fp)

## Publication: Abi-Akar et al. (submitted)
### Overwinter Analysis
#### Part2_OverwinterAnalysis.R

The folder "Overwinter_Analysis" contains the files needed to run the following:

1.	Using data from all LSCFSs, Part2_OverwinterAnalysis.R creates the time line LOESS graphs (e.g., Figure 1), applies and graphs the fall colony overwintering thresholds (Figure 2), and statistically tests them.
2.	Using data output by BEEHAVE from the targeted simulations, it runs the Linear Mixed Effects models (Tables 2 and 3) and creates the corresponding figures (Figures 4 and 5).

Copy all files in the folder to a folder on your computer, change the file location at the top of the R file in line 7, ensure the listed packages are installed, and run the file.
