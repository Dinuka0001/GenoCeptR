# ==================================================================
# GenoCeptR: Main Application File
# Version 3.0 Enhanced - Modular version
# By Dinuka Adasooriya, Yonsei University College of Dentistry, Seoul, Korea
# ==================================================================

# Load global settings and all modules
source("global.R", local = TRUE, encoding = "UTF-8")

# ---- UI ----
app_theme <- bs_theme(
    version = 5,
    primary = "#1f6feb",
    secondary = "#8ec1f6",
    success = "#2ca25f",
    info = "#1f6feb",
    warning = "#e8a33d",
    danger = "#d9534f",
    bg = "#f5faff",
    fg = "#0a1f33",
    `card-border-radius` = "8px",
    `card-box-shadow` = "0 6px 18px rgba(11, 61, 143, 0.07)"
)

ui <- page_sidebar(
    window_title = "GenoCeptR v3.0",
    title = tags$div(),
    theme = app_theme,
    sidebar = ui_sidebar(),
    tags$head(tags$meta(charset = "utf-8")),
    ui_styles(),
    useShinyjs(),
    tags$main(
        class = "app-workspace",
        ui_mainpanel()
    )
)

# ---- Server ----
server <- function(input, output, session) {
    showtext_auto()

    # Reactive values to store data across modules
    gene_lists <- reactiveVal(NULL)
    gene_names_data <- reactiveVal(NULL)
    all_overlaps <- reactiveVal(NULL)
    exact_regions <- reactiveVal(NULL)
    de_data <- reactiveVal(NULL)
    de_data_unfiltered <- reactiveVal(NULL)
    analysis_status <- reactiveVal("awaiting")

    diagram_settings <- reactiveValues(
        labels = NULL,
        colors = NULL
    )

    # Call server modules
    server_data_input(
        input, output, session, gene_lists, gene_names_data,
        all_overlaps, exact_regions, de_data, de_data_unfiltered, diagram_settings,
        analysis_status
    )

    server_plotting(
        input, output, session, gene_lists, gene_names_data,
        all_overlaps, de_data, diagram_settings
    )

    server_downloads(
        input, output, session, gene_lists, gene_names_data,
        all_overlaps, exact_regions, de_data, diagram_settings
    )

    server_outputs(
        input, output, session, gene_lists, gene_names_data,
        all_overlaps, exact_regions, de_data
    )

    server_pathway(
        input, output, session, gene_lists, de_data, de_data_unfiltered, all_overlaps
    )

    render_status_pill <- function() {
        status <- analysis_status()
        if (identical(status, "processing")) {
            return(div(
                class = "navbar-status status-pills",
                span(class = "status-pill warning", icon("spinner", class = "fa-spin"), " Processing...")
            ))
        }
        if (identical(status, "ready") || !is.null(gene_lists())) {
            return(div(
                class = "navbar-status status-pills",
                span(class = "status-pill ready", icon("check-circle"), " Data Loaded")
            ))
        }
        div(
            class = "navbar-status status-pills",
            span(class = "status-pill", icon("circle-notch"), " Awaiting data")
        )
    }

    status_output_ids <- c(
        "status_data_input", "status_visualization", "status_overlap_summary",
        "status_gene_lists", "status_pathway_analysis", "status_data_input_info",
        "status_about"
    )

    lapply(status_output_ids, function(output_id) {
        output[[output_id]] <- renderUI(render_status_pill())
    })

    observeEvent(input$nav_rail_select, {
        updateTabsetPanel(session, "main_tabs", selected = input$nav_rail_select)
    }, ignoreInit = TRUE)

    observe({
        selected_tab <- input$main_tabs %||% "welcome"
        session$sendCustomMessage("setActiveNav", list(value = selected_tab))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
