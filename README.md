# provis = Property Value Impact Simulator


There are 3 main modules:
1) simulation of impact:   R version without Shiny interface
2) estimation of model:    R code
3) simulation of impact:   Shiny version


# Simulation of investment:   R version

     currentversion/dafni/app_dafni.R

Application takes user input and simulates impact of infrastructure investment.
Model must already have been estimated.
user inputs are stored in list "input"
outputs are saved to directory www and also output in list "output".

Model simulator is same as "currentversion/shiny/app.R" without the Shiny interface.


# Estimation of model:

    setwd("/code/currentversion/model")
    source("A0Setup.R")
    source("A1Model1.R")
    source("A2Model2_data.R")
    source("A3Model2.R")

# Shiny Web Application to simulate investment:   Shiny version

    currentversion/shiny/app.R

Web application displays user interface and asks user to input model parameters.
Then calls R to simulate model, creates latex and pdf file to display outputs.
Displays outputs in web browser.

Model simulator is same as "currentversion/dafni/app_dafni.R"






