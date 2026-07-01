# ==================================================================
# UI Main Workspace Tabs Module
# Hidden tabs controlled by the left navigation rail
# ==================================================================

workspace_header <- function(title, subtitle = NULL, status_id = NULL, actions = NULL) {
    div(
        class = "app-workspace-header",
        div(
            class = "app-workspace-title-block",
            h2(class = "app-workspace-title", title),
            if (!is.null(subtitle)) p(class = "app-workspace-subtitle", subtitle)
        ),
        div(
            class = "app-workspace-actions",
            if (!is.null(status_id)) uiOutput(status_id),
            actions
        )
    )
}

download_group <- function(...) {
    div(class = "download-group", ...)
}

diagram_downloads_ui <- function() {
    tagList(
        conditionalPanel(
            condition = "input.diagram_type == 'venn'",
            download_group(
                downloadButton("download_venn_png", "PNG", class = "btn-primary"),
                downloadButton("download_venn_svg", "SVG", class = "btn-primary"),
                downloadButton("download_venn_pdf", "PDF", class = "btn-primary")
            )
        ),
        conditionalPanel(
            condition = "input.diagram_type == 'interactive_venn'",
            downloadButton("download_interactive_venn_html", "Interactive HTML", class = "btn-success")
        ),
        conditionalPanel(
            condition = "input.diagram_type == 'upset'",
            download_group(
                downloadButton("download_upset_png", "PNG", class = "btn-primary"),
                downloadButton("download_upset_svg", "SVG", class = "btn-primary"),
                downloadButton("download_upset_pdf", "PDF", class = "btn-primary")
            )
        ),
        conditionalPanel(
            condition = "input.diagram_type == 'euler'",
            download_group(
                downloadButton("download_euler_png", "PNG", class = "btn-primary"),
                downloadButton("download_euler_svg", "SVG", class = "btn-primary"),
                downloadButton("download_euler_pdf", "PDF", class = "btn-primary")
            )
        ),
        conditionalPanel(
            condition = "input.diagram_type == 'edwards'",
            download_group(
                downloadButton("download_edwards_png", "PNG", class = "btn-primary"),
                downloadButton("download_edwards_svg", "SVG", class = "btn-primary"),
                downloadButton("download_edwards_pdf", "PDF", class = "btn-primary")
            )
        )
    )
}

diagram_settings_panel <- function() {
    div(
        class = "settings-panel",
        div(
            class = "settings-panel-header",
            icon("sliders-h", class = "settings-header-icon"),
            h3("Diagram Settings")
        ),
        div(
            class = "settings-collapse-controls",
            selectInput("diagram_type", "Select Diagram Type:",
                choices = c(
                    "Venn Diagram" = "venn",
                    "Interactive Venn (hover)" = "interactive_venn",
                    "UpSet Plot" = "upset",
                    "Euler Diagram" = "euler",
                    "Edwards' Venn" = "edwards"
                ),
                selected = "venn"
            ),
            div(
                class = "settings-section",
                div(class = "settings-section-title", icon("text-height"), span("Font Sizes")),
                numericInput("label_font_size", "Label Font Size:",
                    value = 1.2, min = 0.1, max = 5, step = 0.1
                ),
                numericInput("number_font_size", "Number Font Size:",
                    value = 1.5, min = 0.1, max = 5, step = 0.1
                )
            ),
            div(
                class = "settings-section",
                div(class = "settings-section-title", icon("tags"), span("Set Labels")),
                uiOutput("label_inputs")
            ),
            conditionalPanel(
                condition = "input.diagram_type == 'venn'",
                div(
                    class = "settings-section",
                    div(class = "settings-section-title", icon("project-diagram"), span("Venn Diagram Type")),
                    selectInput("venn_type", "Select Venn diagram package:",
                        choices = c(
                            "ggVennDiagram (gradient scale)" = "ggvenndiagram",
                            "ggvenn (custom colors)" = "ggvenn",
                            "VennDiagram (classic)" = "venndiagram"
                        ),
                        selected = "ggvenndiagram"
                    )
                )
            ),
            div(
                class = "settings-section",
                div(class = "settings-section-title", icon("palette"), span("Color Palettes")),
                selectInput("venn_color_palette", "Set color palette:",
                    choices = gc_palette_choices("discrete"),
                    selected = "custom"
                ),
                conditionalPanel(
                    condition = "input.venn_color_palette == 'custom'",
                    div(class = "settings-note", "Custom colors applied per set below.")
                ),
                selectInput("venn_gradient_palette", "Count gradient palette:",
                    choices = gc_palette_choices("gradient"),
                    selected = "custom"
                ),
                div(class = "settings-note", "Custom colors stay as default unless a palette is chosen.")
            ),
            conditionalPanel(
                condition = "input.diagram_type == 'venn' && input.venn_type == 'ggvenndiagram'",
                div(
                    class = "settings-section",
                    div(class = "settings-section-title", icon("droplet"), span("ggVennDiagram Colors")),
                    conditionalPanel(
                        condition = "input.venn_gradient_palette == 'custom'",
                        colourInput("ggvenndiagram_low_color", "Low value color:", value = "#F4FAFE"),
                        colourInput("ggvenndiagram_high_color", "High value color:", value = "#4981BF")
                    ),
                    checkboxInput("ggvenndiagram_show_scale", "Show color scale bar", value = FALSE)
                )
            ),
            conditionalPanel(
                condition = "input.diagram_type == 'venn' && input.venn_type == 'ggvenn' && input.venn_color_palette == 'custom'",
                div(
                    class = "settings-section",
                    div(class = "settings-section-title", icon("fill-drip"), span("ggvenn Colors")),
                    uiOutput("color_inputs_ggvenn"),
                    sliderInput("ggvenn_alpha", "Fill transparency:",
                        min = 0, max = 1, value = 0.5, step = 0.05
                    ),
                    checkboxInput("ggvenn_stroke_size", "Show borders", value = TRUE),
                    checkboxInput("ggvenn_show_percentage", "Show percentages", value = FALSE)
                )
            ),
            conditionalPanel(
                condition = "input.diagram_type == 'venn' && input.venn_type == 'venndiagram' && input.venn_color_palette == 'custom'",
                div(
                    class = "settings-section",
                    div(class = "settings-section-title", icon("circle"), span("VennDiagram Colors")),
                    uiOutput("color_inputs_venndiagram"),
                    sliderInput("venndiagram_alpha", "Fill transparency:",
                        min = 0, max = 1, value = 0.3, step = 0.05
                    )
                )
            ),
            conditionalPanel(
                condition = "(input.diagram_type == 'euler' || input.diagram_type == 'edwards') && input.venn_color_palette == 'custom'",
                div(
                    class = "settings-section",
                    div(class = "settings-section-title", icon("palette"), span("Set Colors")),
                    uiOutput("color_inputs")
                )
            ),
            conditionalPanel(
                condition = "input.diagram_type == 'upset'",
                div(
                    class = "settings-section",
                    div(class = "settings-section-title", icon("bar-chart"), span("UpSet Plot Colors")),
                    colourInput("upset_main_bar_color", "Main Bar Color:", value = "#0073C2FF"),
                    colourInput("upset_sets_bar_color", "Sets Bar Color:", value = "#EFC000FF"),
                    colourInput("upset_matrix_color", "Matrix Dot Color:", value = "#404040")
                )
            )
        ),
        div(
            class = "settings-collapse-downloads",
            div(class = "section-header", icon("download"), span("Download")),
            diagram_downloads_ui()
        )
    )
}

ui_mainpanel <- function() {
    div(
        class = "app-main-tabs",
        tabsetPanel(
            id = "main_tabs",
            type = "hidden",
            tabPanel(
                title = "Welcome",
                value = "welcome",
                div(
                    class = "welcome-page",
                    div(
                        class = "welcome-hero",
                        tags$img(src = "logo2.png", class = "welcome-logo"),
                        div(
                            h1("GenoCeptR v3.0: Gene Expression Overlap Analysis"),
                            p("An interactive tool for visualizing and comparing differential gene expression overlaps.")
                        )
                    ),
                    div(
                        class = "content-card",
                        h3(icon("info-circle"), " What is GenoCeptR?"),
                        p("GenoCeptR is a Shiny application for rapid analysis and visualization of overlapping gene lists derived from differential expression studies. It supports up to 8 datasets and provides multiple visualization options for shared and unique genes across experimental conditions."),
                        tags$ul(
                            tags$li(strong("Flexible input:"), " full DE result files, pre-filtered lists, single multi-list files, or pasted gene IDs."),
                            tags$li(strong("Multiple diagrams:"), " Venn, interactive Venn, UpSet, Euler, and Edwards' Venn diagrams."),
                            tags$li(strong("Detailed output:"), " downloadable summary tables and lists of overlapping genes with associated DE statistics.")
                        )
                    ),
                    div(
                        id = "imageCarousel",
                        class = "carousel-card",
                        div(
                            class = "carousel-images",
                            tags$img(src = "image1.png", class = "carousel-slide active"),
                            tags$img(src = "image2.png", class = "carousel-slide"),
                            tags$img(src = "image3.png", class = "carousel-slide"),
                            tags$img(src = "image4.png", class = "carousel-slide"),
                            tags$img(src = "image5.png", class = "carousel-slide")
                        ),
                        div(
                            id = "slideIndicators",
                            class = "slide-indicators",
                            lapply(0:4, function(i) {
                                tags$button(
                                    type = "button",
                                    class = if (i == 0) "indicator active" else "indicator",
                                    onclick = sprintf("goToSlide(%d)", i),
                                    `aria-label` = paste("Go to slide", i + 1)
                                )
                            })
                        )
                    ),
                    div(
                        class = "content-card",
                        h3(icon("rocket"), " Quick Start Guide"),
                        tags$ol(
                            tags$li(strong("Open Data Input:"), " set the number of datasets and choose an input method."),
                            tags$li(strong("Upload or enter data:"), " provide DE files, gene-list files, a multi-list file, or pasted genes."),
                            tags$li(strong("Configure filters:"), " map columns and set p-value, log2FC, and direction filters when using DE files."),
                            tags$li(strong("Generate Diagram:"), " run the analysis from the action card."),
                            tags$li(strong("Review outputs:"), " use Visualization, Overlap Summary, and Gene Lists for plots and exports.")
                        )
                    ),
                    tags$script(HTML("
                        let currentSlide = 0;
                        function showSlide(n) {
                          const slides = document.querySelectorAll('.carousel-slide');
                          const indicators = document.querySelectorAll('.indicator');
                          slides.forEach((slide, index) => slide.classList.toggle('active', index === n));
                          indicators.forEach((indicator, index) => indicator.classList.toggle('active', index === n));
                          currentSlide = n;
                        }
                        function goToSlide(n) { showSlide(n); }
                        setInterval(() => {
                          const slides = document.querySelectorAll('.carousel-slide');
                          if (slides.length > 0) showSlide((currentSlide + 1) % slides.length);
                        }, 5000);
                    "))
                )
            ),
            tabPanel(
                title = "Data Input",
                value = "data_input",
                workspace_header(
                    "Data Input",
                    "Configure datasets, upload gene lists or DE result files, and start the overlap analysis.",
                    "status_data_input"
                ),
                ui_data_input_workspace()
            ),
            tabPanel(
                title = "Visualization",
                value = "visualization",
                workspace_header(
                    "Visualization",
                    "Inspect overlap diagrams and tune diagram rendering options.",
                    "status_visualization"
                ),
                fluidRow(
                    column(
                        width = 9,
                        div(
                            class = "plot-container",
                            conditionalPanel(
                                condition = "input.diagram_type != 'interactive_venn'",
                                plotOutput("main_plot", height = "640px")
                            ),
                            conditionalPanel(
                                condition = "input.diagram_type == 'interactive_venn'",
                                plotlyOutput("interactive_venn_plot", height = "640px"),
                                helpText("Hover over a region or count to inspect the genes in that overlap.")
                            )
                        )
                    ),
                    column(width = 3, diagram_settings_panel())
                )
            ),
            tabPanel(
                title = "Overlap Summary",
                value = "overlap_summary",
                workspace_header(
                    "Overlap Summary",
                    "Review set sizes, exclusive Venn regions, and inclusive intersections.",
                    "status_overlap_summary",
                    downloadButton("download_summary_txt", "Download Summary (TXT)", class = "btn-primary")
                ),
                div(
                    class = "content-card",
                    h3("Number of Genes per Set"),
                    tableOutput("summary_table"),
                    hr(),
                    h3("Venn Region Counts (exclusive)"),
                    helpText("These mutually exclusive regions match the numbers displayed in the Venn diagram."),
                    tableOutput("intersection_table"),
                    hr(),
                    h3("Inclusive Intersection Counts"),
                    helpText("Inclusive intersections also contain genes shared with additional datasets."),
                    tableOutput("inclusive_intersection_table")
                )
            ),
            tabPanel(
                title = "Gene Lists",
                value = "gene_lists",
                workspace_header(
                    "Gene Lists",
                    "Browse and export genes from individual regions or combined overlap sets.",
                    "status_gene_lists"
                ),
                div(
                    class = "content-card",
                    fluidRow(
                        column(
                            width = 4,
                            selectInput("overlap_select", "Select gene set / region:", choices = NULL),
                            helpText("Choose an individual region or a Show all option. Inclusive export reproduces the older app's all-samples/all-combinations CSV.")
                        ),
                        column(
                            width = 8,
                            class = "gene-list-actions",
                            downloadButton("download_overlaps", "Download Selected / All (CSV)", class = "btn-primary")
                        )
                    ),
                    DTOutput("gene_table")
                )
            ),
            tabPanel(
                title = "Pathway Analysis",
                value = "pathway_analysis",
                workspace_header(
                    "Pathway Analysis",
                    "Run over-representation analysis and pathway-level visual summaries.",
                    "status_pathway_analysis"
                ),
                fluidRow(
                    column(width = 3, ui_pathway_sidebar()),
                    column(width = 9, div(class = "content-card pathway-results-card", ui_pathway_results()))
                )
            ),
            tabPanel(
                title = "Data Input Info",
                value = "data_input_info",
                workspace_header(
                    "Data Input Info",
                    "Inspect the parsed input metadata and file details.",
                    "status_data_input_info",
                    downloadButton("download_file_info", "Download Info (TXT)", class = "btn-primary")
                ),
                div(
                    class = "content-card",
                    verbatimTextOutput("file_info")
                )
            ),
            tabPanel(
                title = "About",
                value = "about",
                workspace_header("About", "Version, author, availability, and package references.", "status_about"),
                div(
                    class = "content-card",
                    tags$div(
                        class = "text-center mb-4 p-4",
                        style = paste0(
                            "background: linear-gradient(135deg, var(--gc-primary-soft) 0%, var(--gc-panel) 100%);",
                            "border: 1px solid var(--gc-border); border-radius: var(--radius-lg);",
                            "box-shadow: var(--shadow-md);"
                        ),
                        tags$img(
                            src = "logo2.png",
                            style = "height: 7rem; width: auto; margin-bottom: 1rem; filter: drop-shadow(0 4px 12px rgba(11, 61, 143, 0.15));"
                        ),
                        tags$h3("GenoCeptR", style = "font-weight: 700; color: var(--gc-text-dark); margin-bottom: 0.25rem;"),
                        tags$p(class = "mb-0", style = "color: var(--gc-primary); font-weight: 600;", "v3.0"),
                        tags$p(class = "small mb-0", style = "color: var(--gc-muted);", "Gene Expression Overlap & Pathway Enrichment Analysis")
                    ),
                    p("An interactive tool for visualizing and comparing differential gene expression overlaps."),
                    p(strong("Developer:"), " Dinuka Adasooriya"),
                    p("Division of Anatomy and Developmental Biology, Department of Oral Biology, BK21 FOUR Project, Yonsei University College of Dentistry, Seoul, Korea"),
                    p(strong("Email:"), " dinuka90@yuhs.ac"),
                    h3("Version 3.0 Updates"),
                    tags$ul(
                        tags$li("Multiple input methods for gene lists"),
                        tags$li("Single file upload with multiple columns or sheets"),
                        tags$li("Support for up to 8 datasets"),
                        tags$li("ggVennDiagram for enhanced Venn diagram visualization"),
                        tags$li("Edwards' Venn diagrams"),
                        tags$li("Enhanced plot customization options"),
                        tags$li("Improved data input configuration"),
                        tags$li("Pathway Analysis with Over-Representation Analysis"),
                        tags$li("Multiple pathway databases including GO, KEGG, Reactome, and WikiPathways"),
                        tags$li("Interactive pathway visualizations")
                    ),
                    h3("Availability"),
                    p(
                        "Source code: ",
                        tags$a(
                            href = "https://github.com/Dinuka0001/GenoCeptR.git",
                            "GitHub repository", target = "_blank"
                        )
                    ),
                    h3("R packages used"),
                    verbatimTextOutput("references")
                )
            )
        )
    )
}
