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
cb_upstr_dwnstr.dat <- subset(cb_upstr_dwnstr.dat, brd_yr>=2000 & brd_yr<=2015)
cb_upstr_dwnstr.dat$rs <- cb_upstr_dwnstr.dat$nat_recruits/cb_upstr_dwnstr.dat$tot_spnrs
cb_upstr_dwnstr.dat$basin_index <- ifelse(cb_upstr_dwnstr.dat$basin=="sr",1,2)

### covariate placeholders (for framework development only)
cb_upstr_dwnstr.dat$cov1 <- rnorm(nrow(cb_upstr_dwnstr.dat),20,3)
cb_upstr_dwnstr.dat$cov2 <- rnorm(nrow(cb_upstr_dwnstr.dat),20,4)
cb_upstr_dwnstr.dat$cov3 <- rnorm(nrow(cb_upstr_dwnstr.dat),20,10)
