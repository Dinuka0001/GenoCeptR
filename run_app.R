# Launch GenoCeptR v3.0 as a standalone Shiny app.

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE), error = function(e) NULL)
app_dir <- if (!is.null(script_path)) dirname(script_path) else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
setwd(app_dir)

shiny::runApp(appDir = app_dir, host = "127.0.0.1", port = 3838, launch.browser = TRUE)
