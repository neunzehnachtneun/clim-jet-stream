#' ==========================================================================================================
#' HAUPTPROGRAMM DER ABSCHLUSSARBEIT ZUR DETEKTION, UNTERSCHEIDUNG UND ANALYSE VON                         ==
#' JETSTREAMS DER OBEREN TROPOSPHAERE AUF DER NOERDLICHEN HEMISPHAERE.                                     ==
#' ICH HABE EINE LIBRARY SELBST ERSTELLT UND AUF VERSCHIEDENEN PACKAGES UND ROUTINEN ZURÜCKGEGRIFFEN.      ==
#' DIE ARBEITSSCHRITTE WERDEN HIER NACH BESTEM WISSEN UND GEWISSEN DOKUMENTIERT.                           ==
#' ==========================================================================================================
#' GERECHNET WURDE AUF WORKSTATIONS (OPENSUSE 42.1-42.3 & FEDORA 24-26) SOWIE EINEM RECHENSERVER           ==
#' DES METEOROLOGISCHEN INSTITUTS DER UNIVERSITÄT BONN MIT 32 KERNEN UND 128 GB ARBEITSSPEICHER.           ==
#' ==========================================================================================================
#' 


#' Setzen des Working Directories
setwd("~/01-Master-Thesis/02-code-git/01-code/")
#' Setzen des Verzeichnisses zum Speichern von Abbildungen
save_dir <- "../03-visu/"


#' Laden wichtiger Pakete -----------------------------------------------------------------------------------
library(ncdf4)
library(ChebyshevMax)
library(rootSolve)
library(igraph)
library(parallel)
library(foreach)
library(doParallel)
library(lubridate)
library(magrittr)
library(dplyr)
library(reshape2)
library(zoo)
library(broom)
library(ggplot2)
library(tikzDevice)
# library(styler)
# library(lintr)

#' Sourcen von Hilfs- und Unterroutinen ---------------------------------------------------------------------
#' 
source("1_02_jetstream_detection_schemes.R")
source("1_03_help_functions.R")
# source("1_04_get_orography.R")

#' Festlegen von Konstanten für Routinen ---------------------------------------
geopotential <- 250
bound_lower_chebyshev <- 20
bound_upper_chebyshev <- 80
order_polynomial_fit <- 24
threshold_single_jet <- 10
n_cluster <- 16

  
#' Eigenständige (!) Vor-Routinen ---------------------------------------------------------------------------
#' Nicht aus diesem Skript ausrufen!
# source("2_optimize_fit_order_chebyshev.R")
# source("3_download_format_seaice_data.R")


#' Sourcen von Haupt-Routinen im Workflow -------------------------------------------------------------------
#'
source("1_05_read_era_data.R")
source("1_06_apply_jet_detection_on_data.R")
save.image("stp-a.RData"); load("stp-a.RData")
source("1_07_format_tidy_data.R")
save.image("stp-b.RData"); load("stp-b.RData")

#' Visualisierung der Ergebnisse ----------------------------------------------------------------------------
#' 
source("1_08_visualize_timeseries_anomalies_eradata.R")
source("1_09_visualize_individual_cases.R")
source("1_10_visualize_comparison_methods.R")
source("1_11_visualize_mean_jetstreams.R")
source("1_12_visualize_climate_trends.R")
source("1_13_visualize_hovmoller_diagramms.R")
source("1_14_visualize_seaice_dev_relation_jetstream.R")
save.image("stp-c.RData"); load("stp-c.RData")


#' Zitationen der genutzten Packages im Bibtex-Format zum Einbinden in Abschlussarbeit ----------------------
#'
citation(package = "base")
citation(package = "ncdf4")
citation(package = "rootSolve")
citation(package = "igraph")
citation(package = "parallel")
citation(package = "foreach")
citation(package = "doParallel")
citation(package = "lubridate")
citation(package = "magrittr")
citation(package = "dplyr")
citation(package = "reshape2")
citation(package = "zoo")
citation(package = "broom")
citation(package = "ggplot2")
citation(package = "tikzDevice")
citation(package = "rstudio")
