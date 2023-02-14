# call packages -----------------------------------------------------------
packages <- c("openxlsx",
              "ggplot2",
              "tidymv",
              "car",
              "ggfortify",
              "gridExtra",
              "grid",
              "ggpubr",
              "openair",
              "cvTools",
              "extrafont",
              "remotes",
              "R2jags",
              "dplyr",
              "broom.mixed",
              "boot",
              "ggmcmc",
              "scales",
              "postpack",
              "MCMCvis",
              "data.table",
              "flextable",
              "officer",
              "webshot2")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

# reconcile fonts ---------------------------------------------------------
## for plotting purposes only--the font package takes some time to load (skip this bit as appropriate)
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
cb_upstr_dwnstr.dat <- subset(cb_upstr_dwnstr.dat, brd_yr>=2000 & brd_yr<=2015 & popn=="grcat")
cb_upstr_dwnstr.dat$rs <- cb_upstr_dwnstr.dat$nat_recruits/cb_upstr_dwnstr.dat$tot_spnrs
cb_upstr_dwnstr.dat$basin_index <- ifelse(cb_upstr_dwnstr.dat$basin=="sr",1,2)
cb_upstr_dwnstr.dat$lnrs <- log(cb_upstr_dwnstr.dat$rs)

### covariate placeholders (for framework development only--can be deleted after we develop covariates)
cb_upstr_dwnstr.dat$cov1 <- rnorm(nrow(cb_upstr_dwnstr.dat),20,3)
cb_upstr_dwnstr.dat$cov2 <- rnorm(nrow(cb_upstr_dwnstr.dat),20,4)
cb_upstr_dwnstr.dat$cov3 <- rnorm(nrow(cb_upstr_dwnstr.dat),20,10)
