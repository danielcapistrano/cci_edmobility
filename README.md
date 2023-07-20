This directory contains all the code necessary to reproduce the analysis described in the paper "'I guess we are from very different backgrounds':
Attitudes towards Social Justice and Intergenerational Educational Mobility in European Societies"

# Requirements

The analysis was developed using [R](https://www.r-project.org/). Once the software is installed, it is necessary to install some packages before running the code. To do that, you can run the following command

`install.packages(c("tidyverse", "haven", "missRanger", "Hmisc", "MCM", "purrr", "broom", "broom.helpers", "gt", "ggpubr"))`

# Data
The analysis uses data from the 9th Round of the [European Social Survey (ESS)](www.europeansocialsurvey.org). The original full dataset can be downloaded for free on the ESS website. The subdirectory "code/data" contains the working datasets, which only includes the cases and variables required to reproduce the analysis (the processing steps are described in the code file 0_Processing). 

# Code

The analysis can be reproduced by running the following files included in the "code" sub-directory.

0) **0_Processing.R**: Contains all steps to produce the working datasets from the original raw ESS9 data file. You can skip this step if you do no not want to include a new variable or recode the variables in the working datasets. 

1) **1_Descriptives**: This is the code required to reproduce the descriptive analysis and generate Table 2 of the paper. 

2) **2_Estimates**: This is the code required to generate Table 3 of the paper as well as Table 1A (Appendix). 

3) **3_EstimatesByCntry**: This is the code required to generate Figure 1 of the paper as well as the remaining tables from the Appendix. 

