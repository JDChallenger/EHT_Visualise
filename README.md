# EHT_Visualise

The files in this folder can be used as a tutorial on how to visualise and analyse data from experimental hut trials. The PDF file 'EHT_tutorial.pdf' will talk you through the necessary steps. The R script 'script_for_tutorial.R' will allow you to perform the analysis for yourself. 

It is important that you install both R and Rstudio before starting the tutorial. Once you have done so, click on the green button 'Code' above, and select 'Download ZIP'. After you've extracted the files, double click on the R project file 'EHT_Visualise.Rproj'. Then open the tutorial script 'script_for_tutorial.R'. This script will allow you to load and view the data, and perform the data analysis.

There are some R packages you will need to install in order to carry out the analyses. For convenience we list them here, along with the instruction to install them:

```
install.packages(c('lme4','devtools','MASS','RColorBrewer','cowplot','ggplot2','dplyr','reshape2'))
```

There is one remaining package to install. After the `devtools` package is installed, you should be able to run the following command to install the `GLMMmisc` package:
```
devtools::install_github("pcdjohnson/GLMMmisc")
```

