##************************************************************************
## Script Name: Automate data update
## Purpose:         
## 
##
## Created: 2020-10-01                               
## Authors: Mariana Fernández 
##          
## GitHub: marianafdz465  
##
##
##************************************************************************

source("analysis/01-wrangle-data-covid-ssa-mx-state.R")
source("automatization/compare_data_vs_officialfigures.R")
rmarkdown::render("automatization/generate_daily_report.Rmd")


