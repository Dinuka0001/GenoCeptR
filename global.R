# ==================================================================
# GenoCeptR: Global settings and libraries
# Version 3.0 Enhanced - Modular version
# By Dinuka Adasooriya, Yonsei University College of Dentistry, Seoul, Korea
# ==================================================================

# List of required packages
library(shiny)
library(bslib)
library(shinyjs)
library(colourpicker)
library(VennDiagram)
library(ggvenn)
library(dplyr)
library(DT)
library(shinyWidgets)
library(readxl)
library(openxlsx)
library(tools)
library(grid)
library(UpSetR)
library(eulerr)
library(ggplot2)
library(showtext)
library(ggVennDiagram)
library(plotly)

# Pathway analysis packages are used lazily in pathway_utils.R so the core
# overlap app can still launch when optional pathway dependencies are missing.
optional_pathway_packages <- c("gprofiler2", "ggdendro", "visNetwork", "igraph", "scales")
invisible(lapply(optional_pathway_packages, requireNamespace, quietly = TRUE))
library(htmlwidgets)

# Set maximum file upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)
options(encoding = "UTF-8")

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

# Source utility functions
source("utils/file_utils.R", local = TRUE, encoding = "UTF-8")
source("utils/palette_utils.R", local = TRUE, encoding = "UTF-8")
source("utils/overlap_utils.R", local = TRUE, encoding = "UTF-8")
source("utils/plot_utils.R", local = TRUE, encoding = "UTF-8")
source("utils/pathway_utils.R", local = TRUE, encoding = "UTF-8")

# Source UI modules
source("modules/ui_styles.R", local = TRUE, encoding = "UTF-8")
source("modules/ui_sidebar.R", local = TRUE, encoding = "UTF-8")
source("modules/ui_tabs.R", local = TRUE, encoding = "UTF-8")
source("modules/ui_pathway.R", local = TRUE, encoding = "UTF-8")

# Source server modules
source("modules/server_data_input.R", local = TRUE, encoding = "UTF-8")
source("modules/server_plotting.R", local = TRUE, encoding = "UTF-8")
source("modules/server_downloads.R", local = TRUE, encoding = "UTF-8")
source("modules/server_outputs.R", local = TRUE, encoding = "UTF-8")
source("modules/server_pathway.R", local = TRUE, encoding = "UTF-8")
