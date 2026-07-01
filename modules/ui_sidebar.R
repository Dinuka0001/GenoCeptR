# ==================================================================
# UI Sidebar Module
# Pure navigation rail plus the Data Input workspace controls
# ==================================================================

nav_button <- function(value, label, icon_name, class = "nav-rail-link") {
    tags$button(
        type = "button",
        class = class,
        `data-nav-value` = value,
        onclick = sprintf(
            "Shiny.setInputValue('nav_rail_select', '%s', {priority:'event'});",
            value
        ),
        tags$span(class = "nav-rail-icon", icon(icon_name)),
        tags$span(class = "nav-rail-label", label)
    )
}

ui_sidebar <- function() {
    bslib::sidebar(
        width = 260,
        bg = "#0d47a1",
        class = "app-sidebar",
        open = "always",

        div(
            class = "nav-rail-brand",
            tags$img(src = "logo.png", class = "brand-logo"),
            div(class = "brand-tagline", "Gene Expression Overlap Analysis")
        ),

        div(
            class = "nav-rail-shell",
            div(
                class = "nav-rail nav-rail-main",
                div(class = "nav-rail-section", "Workspace"),
                nav_button("welcome", "Welcome", "home"),
                nav_button("data_input", "Data Input", "database"),
                nav_button("visualization", "Visualization", "chart-area"),
                nav_button("overlap_summary", "Overlap Summary", "table"),
                nav_button("gene_lists", "Gene Lists", "list"),
                nav_button("pathway_analysis", "Pathway Analysis", "project-diagram"),
                nav_button("data_input_info", "Data Input Info", "file-alt")
            ),
            div(
                class = "nav-rail-bottom",
                nav_button("about", "About", "info-circle"),
                div(
                    class = "sidebar-footer",
                    tags$div(class = "sidebar-credit-line", HTML("&copy;"), " Dinuka Adasooriya"),
                    tags$div(class = "sidebar-credit-line", "Yonsei University College of Dentistry"),
                    tags$div(class = "sidebar-credit-line", "2026")
                )
            )
        )
    )
}

ui_data_input_workspace <- function() {
    div(
        class = "data-input-grid",
        div(
            class = "data-input-left-column",
            div(
                class = "data-input-card data-input-settings-card",
                div(
                    class = "data-input-card-header",
                    icon("sliders-h"),
                    h3("Data Input Settings & Method")
                ),
                numericInput("num_datasets", "Number of datasets (2-8):",
                    value = 2, min = 2, max = 8, step = 1
                ),
                div(
                    id = "dataset_count_warning", style = "display: none;",
                    class = "alert alert-warning",
                    icon("exclamation-triangle"),
                    " Please enter a valid number between 2 and 8"
                ),
                radioButtons("input_method",
                    "Choose input method:",
                    choices = c(
                        "Upload DE result files (with statistics)" = "file",
                        "Upload pre-filtered gene lists (text/csv)" = "genelist",
                        "Single file with multiple gene lists" = "single",
                        "Paste gene IDs/names" = "paste"
                    ),
                    selected = "file"
                ),
                div(
                    class = "input-method-help",
                    tags$p(icon("info-circle"), strong(" DE result files:"), " Upload full differential expression result files with statistics."),
                    tags$p(strong("Pre-filtered lists:"), " Upload simple gene list files, one gene per line."),
                    tags$p(strong("Single file:"), " Upload one file containing all gene lists in different columns or sheets."),
                    tags$p(strong("Paste genes:"), " Directly paste lists of gene IDs or names.")
                )
            ),
            conditionalPanel(
                condition = "input.input_method == 'file'",
                div(
                    class = "data-input-card data-input-config-card",
                    div(
                        class = "data-input-card-header",
                        icon("sliders-h"),
                        h3("Column Mapping & Filters")
                    ),
                    div(class = "subhead", icon("columns"), "Column Name Mapping"),
                    div(
                        class = "input-grid-two",
                        selectizeInput("gene_col", "Gene ID Column Name:",
                            choices = NULL,
                            options = list(create = TRUE, placeholder = "Type or select column name")
                        ),
                        selectizeInput("gene_name_col", "Gene Name Column (optional):",
                            choices = NULL,
                            options = list(create = TRUE, placeholder = "Type or select column name")
                        ),
                        selectizeInput("padj_col", "Adjusted P-value Column Name:",
                            choices = NULL,
                            options = list(create = TRUE, placeholder = "Type or select column name")
                        ),
                        selectizeInput("lfc_col", "Log2 Fold Change Column Name:",
                            choices = NULL,
                            options = list(create = TRUE, placeholder = "Type or select column name")
                        )
                    ),
                    hr(),
                    div(class = "subhead", icon("filter"), "Filter Settings"),
                    div(
                        class = "input-grid-two",
                        numericInput("padj_cutoff", "Adjusted P-value Cutoff:",
                            value = 0.05, min = 0, max = 1, step = 0.01
                        ),
                        numericInput("lfc_cutoff", "Absolute Log2FC Cutoff:",
                            value = 1, min = 0, step = 0.1
                        )
                    ),
                    checkboxInput("use_lfc", "Apply Log2FC filter", value = TRUE),
                    hr(),
                    div(class = "subhead", icon("arrows-alt-v"), "Gene Direction Filter"),
                    radioButtons("gene_direction",
                        "Include genes:",
                        choices = c(
                            "All significant genes" = "all",
                            "Upregulated only (Log2FC > 0)" = "up",
                            "Downregulated only (Log2FC < 0)" = "down"
                        ),
                        selected = "all"
                    )
                )
            ),
            div(
                class = "data-input-card data-input-action-card",
                div(
                    class = "data-input-card-header",
                    icon("play-circle"),
                    h3("Action Area")
                ),
                p("Generate the overlap analysis after the input settings and files are ready."),
                actionButton("generate", "Generate Diagram",
                    class = "btn-primary btn-lg generate-action",
                    icon = icon("chart-area")
                )
            )
        ),

        div(
            class = "data-input-card data-input-dynamic-card",
            div(
                class = "data-input-card-header",
                icon("folder-open"),
                h3("Dynamic Input Area")
            ),
            conditionalPanel(
                condition = "input.input_method == 'file'",
                div(class = "subhead", icon("file-upload"), "Upload DE Result Files"),
                helpText("Recommended: Use .xlsx or .csv formats."),
                uiOutput("file_upload_ui")
            ),
            conditionalPanel(
                condition = "input.input_method == 'genelist'",
                div(class = "subhead", icon("list-ul"), "Upload Pre-filtered Gene List Files"),
                helpText("Upload text/csv/tsv files with one gene per line. Headers are not required."),
                uiOutput("genelist_upload_ui")
            ),
            conditionalPanel(
                condition = "input.input_method == 'single'",
                div(class = "subhead", icon("file-archive"), "Upload Single File with Multiple Gene Lists"),
                fileInput("single_file",
                    "Upload CSV/TSV/Excel file:",
                    accept = c("text/csv", ".csv", ".tsv", ".txt", ".xlsx", ".xls")
                ),
                uiOutput("single_file_options_ui")
            ),
            conditionalPanel(
                condition = "input.input_method == 'paste'",
                div(class = "subhead", icon("keyboard"), "Paste Gene Lists"),
                helpText("Enter one gene ID or name per line."),
                uiOutput("paste_genes_ui"),
                actionButton("clear_paste", "Clear All Pasted Data",
                    icon = icon("eraser"), class = "btn-warning btn-sm btn-sm-block"
                )
            )
        )
    )
}
