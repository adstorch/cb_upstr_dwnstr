# call packages -----------------------------------------------------------
packages <- c("openxlsx",
              "ggplot2",
              "R2jags",
              "ggmcmc",
              "scales",
              "postpack",
              "MCMCvis",
              "car",
              "extrafont",
              "remotes")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

# reconcile fonts ---------------------------------------------------------
remotes::install_version("Rttf2pt1", version = "1.3.8")
font_import(prompt = FALSE, pattern = "calibri")
fonts()
loadfonts(device = "win")
windowsFonts()

## read data 
cb_upstr_dwnstr.dat <- read.xlsx("Data\\Input Data\\cb_upstr_dwnstr_inp_data.xlsx",
                         sheet = 1,
                         colNames = TRUE)

## data manipulation
cb_upstr_dwnstr.dat <- subset(cb_upstr_dwnstr.dat, brd_yr>=2000|brd_yr>=2014)