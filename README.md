# fLAMP Analysis Software Tool

## fAST V1.0
A software tool written in R using the shiny package for quickly analyzing fLAMP data

### Installing:  
1. Download fAST as zip and extract contents to a folder on your desktop (for speed purposes it is recommended to save files on your local disk, not on Egnyte) 

2. From an R program (e.g. *Rstudio*, R command line), navigate working directory to fAST installation with either: 

(Session > Set Working Directory in *Rstudio*) or
```
setwd(<fAST directory>)
```
3. Load shiny package. 
If (shiny) is not a part of your installation, first install it with: 
```
install.packages("shiny") 
```
If shiny is already installed: 
```
 library(shiny) 
 shiny:: runApp()
```
4. The user interface should appear, and/or a url that will work from any browser



### Usage
1. Load thermal cycler data into a folder (on Enygte or personal computer)
2. Make sure all files from folder are closed on your computer. 
3. Select folder using fAST UI. 
4. fAST allows you to change analysis settings before or after selecting a folder. Preferences can be saved to be 
loaded from startup using the "Save preferences" button 
 
#### Some details regarding TTR algorithms
Midpoint:
TTR is defined as Cycle/Time when RFU signal reaches the midpoint of the baseline (defined between baselineStart and baselineEnd) and the peak value

Regression:
Logistic regression is performed to a fit a logistic curve to the data. TTR is defined as the time by which the fit curve reaches halfway to the asymptote value  
__Note__: If the algorithm can not fit a logistic regression curve, the last cycle is shaved until at most 50 data points are dropped. This functionality is required for curves where the peak steady-state value is decreasing over time. 
