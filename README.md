# EHT_Visualise

The files in this folder can be used as a tutorial on how to visualise and analyse data from experimental hut trials. The PDF file 'EHT_tutorial.pdf' will talk you through the necessary steps. The R script 'script_for_tutorial.R' will allow you to perform the analysis for yourself. 

It is important that you install both R and Rstudio before starting the tutorial. If you have a Windows computer, R can be installed [here](https://cran.r-project.org/bin/windows/base/). For other operating systems (Mac, Linux etc.), visit this [page](https://cran.r-project.org). Double-clicking the downloaded file should start the installation process. Rstudio can be downloaded [here](https://www.rstudio.com/products/rstudio/download/), choose the free option 'Rstudio Desktop'. Again, double-clicking the downloaded file should start the installation process. There are videos on Youtube (such as [this one](https://www.youtube.com/watch?v=NZxSA80lF1I), for a Windows installation), which go through the installation process for both R and Rstudio, step-by-step. 

Once you have installed R and Rstudio, click on the green button 'Code' near the top of this page, and select 'Download ZIP'. After you've extracted the files, open the R project file 'EHT_Visualise.Rproj' in Rstudio. Then open the tutorial script 'script_for_tutorial.R'. This script will allow you to load and view the data, and perform the data analysis.

There are some R packages you will need to install in order to carry out the analyses. For convenience we list them here, along with the command to install them:

```
install.packages(c('lme4','devtools','MASS','RColorBrewer','cowplot','ggplot2','dplyr','reshape2'))
```

There is one remaining package to install. After the `devtools` package is installed, you should be able to run the following command to install the `GLMMmisc` package:
```
devtools::install_github("pcdjohnson/GLMMmisc")
```

