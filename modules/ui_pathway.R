# ==================================================================
# UI Pathway Analysis Module
# Pathway analysis tab UI with all controls
# ==================================================================

ui_pathway_sidebar <- function() {
    div(
        class = "panel-pad-sm",
        div(
            class = "settings-panel pathway-settings-panel",
            div(
                class = "settings-panel-header",
                icon("cog", class = "settings-header-icon"),
                h3("Pathway Settings")
            ),
            div(
                class = "settings-collapse-controls",
                # Gene input method selection
                radioButtons("pathway_gene_input_method",
                    "Gene Input Method:",
                    choices = c(
                        "Select from dataset" = "dataset",
                        "Paste gene list manually" = "manual"
                    ),
                    selected = "dataset"
                ),

                # Dataset selection (conditional)
                conditionalPanel(
                    condition = "input.pathway_gene_input_method == 'dataset'",
                    selectInput("pathway_overlap_select", "Select Dataset/Overlap:",
                        choices = NULL,
                        selected = NULL
                    )
                ),

                # Manual gene paste (conditional)
                conditionalPanel(
                    condition = "input.pathway_gene_input_method == 'manual'",
                    textAreaInput("pathway_manual_genes",
                        "Paste Gene List (one per line):",
                        rows = 8,
                        placeholder = "Gene1\nGene2\nGene3..."
                    )
                ),
                hr(),

                # Gene ID Type selection
                radioButtons("pathway_gene_type",
                    "Gene Input Type:",
                    choices = c(
                        "Gene IDs (Ensembl/Entrez)" = "gene_id",
                        "Gene Names/Symbols" = "gene_name"
                    ),
                    selected = "gene_name"
                ),
                hr(),

                # Species/Genome assembly
                div(class = "subhead", icon("dna"), "Species"),
                selectInput("pathway_species", "Species/Genome Assembly:",
                    choices = c(
                        "Human (Homo sapiens)" = "hsapiens",
                        "Mouse (Mus musculus)" = "mmusculus",
                        "Rat (Rattus norvegicus)" = "rnorvegicus",
                        "Zebrafish (Danio rerio)" = "drerio",
                        "Fruit fly (Drosophila melanogaster)" = "dmelanogaster",
                        "C. elegans (Caenorhabditis elegans)" = "celegans",
                        "Yeast (Saccharomyces cerevisiae)" = "scerevisiae",
                        "Arabidopsis (Arabidopsis thaliana)" = "athaliana"
                    ),
                    selected = "hsapiens"
                ),
                hr(),

                # Background options
                div(class = "subhead", icon("layer-group"), "Background Gene Set"),
                radioButtons("pathway_background_type",
                    "Background Source:",
                    choices = c(
                        "Paste background genes" = "paste",
                        "Use automatic background for species" = "auto"
                    ),
                    selected = "auto"
                ),
                conditionalPanel(
                    condition = "input.pathway_background_type == 'paste'",
                    textAreaInput("pathway_bg_list",
                        "Background Genes (one per line):",
                        rows = 8,
                        placeholder = "BgGene1\nBgGene2\nBgGene3..."
                    )
                ),
                hr(),

                # Pathway database selection
                div(class = "subhead", icon("database"), "Pathway Database"),
                selectInput("pathway_database", "Select Database:",
                    choices = c(
                        "GO: Biological Process" = "GO:BP",
                        "GO: Cellular Component" = "GO:CC",
                        "GO: Molecular Function" = "GO:MF",
                        "KEGG" = "KEGG",
                        "Reactome" = "REAC",
                        "WikiPathways" = "WP",
                        "TRANSFAC" = "TF",
                        "Human Phenotype Ontology" = "HP",
                        "CORUM" = "CORUM"
                    ),
                    selected = "GO:BP"
                ),
                hr(),

                # Analysis parameters
                div(class = "subhead", icon("sliders-h"), "Analysis Parameters"),
                numericInput("pathway_fdr_cutoff", "FDR Cutoff:",
                    value = 0.05, min = 0.001, max = 1, step = 0.01
                ),
                numericInput("pathway_num_show", "Pathways to Show:",
                    value = 20, min = 1, max = 100, step = 1
                ),
                numericInput("pathway_min_size", "Pathway Size (Min):",
                    value = 2, min = 2, max = 30, step = 1
                ),
                numericInput("pathway_max_size", "Pathway Size (Max):",
                    value = 5000, min = 1000, max = 20000, step = 100
                ),
                checkboxInput("pathway_remove_redundancy", "Remove Redundancy", value = TRUE),
                checkboxInput("pathway_abbreviate", "Abbreviate Pathways", value = TRUE),
                checkboxInput("pathway_use_db_counts", "Use Pathway DB for Gene Counts", value = FALSE),
                checkboxInput("pathway_show_ids", "Show Pathway IDs", value = FALSE),
                hr(),

                # Gene selection criteria (only shown when DE data is available)
                conditionalPanel(
                    condition = "output.pathway_has_de_data",
                    div(class = "subhead", icon("filter"), "Gene Selection Criteria"),
                    helpText(
                        "Significance filtering (P-adj/Log2FC cutoffs) was already applied on the ",
                        "Data Input tab. Use this to further restrict the selected dataset/overlap ",
                        "to only up- or down-regulated genes, based on the mapped Log2FC column."
                    ),
                    radioButtons("pathway_gene_direction", "Gene Direction:",
                        choices = c(
                            "Upregulated" = "up",
                            "Downregulated" = "down",
                            "Both (Up + Down)" = "both"
                        ),
                        selected = "both"
                    ),
                    hr()
                )
            ),
            # Action region - kept visible when the panel is collapsed
            div(
                class = "settings-collapse-downloads",
                div(class = "section-header", icon("play-circle"), span("Run Analysis")),
                actionButton("run_pathway_analysis", "Run Pathway Analysis",
                    class = "btn-success btn-lg btn-block run-action",
                    icon = icon("play-circle")
                )
            )
        )
    )
}

ui_pathway_results <- function() {
    div(
        class = "panel-pad",
        fluidRow(
            column(
                width = 4,
                selectInput("pathway_result_type", "Result Display:",
                    choices = c(
                        "Enrichment Plot" = "enrichment",
                        "Table" = "table",
                        "Tree" = "tree",
                        "Network" = "network"
                    ),
                    selected = "enrichment"
                )
            ),
            column(
                width = 8,
                class = "pathway-result-toolbar",
                # Download buttons
                conditionalPanel(
                    condition = "input.pathway_result_type == 'enrichment'",
                    div(class = "download-group",
                        downloadButton("download_pathway_plot_png", "Download (PNG)", class = "btn-primary"),
                        downloadButton("download_pathway_plot_svg", "Download (SVG)", class = "btn-primary"),
                        downloadButton("download_pathway_plot_pdf", "Download (PDF)", class = "btn-primary")
                    )
                ),
                conditionalPanel(
                    condition = "input.pathway_result_type == 'table'",
                    downloadButton("download_pathway_table", "Download Table (CSV)", class = "btn-primary")
                ),
                conditionalPanel(
                    condition = "input.pathway_result_type == 'tree'",
                    div(class = "download-group",
                        downloadButton("download_pathway_tree_png", "Download (PNG)", class = "btn-primary"),
                        downloadButton("download_pathway_tree_svg", "Download (SVG)", class = "btn-primary"),
                        downloadButton("download_pathway_tree_pdf", "Download (PDF)", class = "btn-primary")
                    )
                ),
                conditionalPanel(
                    condition = "input.pathway_result_type == 'network'",
                    div(class = "download-group",
                        downloadButton("download_pathway_network_img", "Export Image", class = "btn-primary"),
                        downloadButton("download_pathway_network_html", "Download HTML", class = "btn-primary"),
                        downloadButton("download_pathway_edges", "Edges", class = "btn-primary"),
                        downloadButton("download_pathway_nodes", "Nodes", class = "btn-primary")
                    )
                )
            )
        ),
        fluidRow(
            column(
                width = 9,
                # Main results area
                conditionalPanel(
                    condition = "input.pathway_result_type == 'enrichment'",
                    plotOutput("pathway_enrichment_plot", height = "700px")
                ),
                conditionalPanel(
                    condition = "input.pathway_result_type == 'table'",
                    DTOutput("pathway_table")
                ),
                conditionalPanel(
                    condition = "input.pathway_result_type == 'tree'",
                    plotOutput("pathway_tree_plot", height = "700px")
                ),
                conditionalPanel(
                    condition = "input.pathway_result_type == 'network'",
                    uiOutput("pathway_network_plot")
                )
            ),
            column(
                width = 3,
                div(
                    class = "settings-panel pathway-options-panel",
                    div(
                        class = "settings-panel-header",
                        icon("sliders-h", class = "settings-header-icon"),
                        h3("Result Options")
                    ),
                    div(
                        class = "settings-collapse-controls",
                    # Plot options panel
                    conditionalPanel(
                        condition = "input.pathway_result_type == 'enrichment'",
                        div(
                            class = "settings-section",
                            div(class = "settings-section-title", icon("chart-area"), span("Plot Options")),
                            selectInput("pathway_sort_by", "Sort Pathway by:",
                                choices = c(
                                    "Fold Enrichment" = "fold_enrichment",
                                    "FDR" = "fdr",
                                    "Gene Count" = "gene_count",
                                    "Pathway Name" = "name"
                                ),
                                selected = "fold_enrichment"
                            ),
                            selectInput("pathway_xaxis", "X-axis:",
                                choices = c(
                                    "Fold Enrichment" = "fold_enrichment",
                                    "Gene Count" = "gene_count",
                                    "-log10(FDR)" = "log_fdr"
                                ),
                                selected = "fold_enrichment"
                            ),
                            selectInput("pathway_color_by", "Color:",
                                choices = c(
                                    "-log10(FDR)" = "log_fdr",
                                    "FDR" = "fdr",
                                    "Fold Enrichment" = "fold_enrichment"
                                ),
                                selected = "log_fdr"
                            ),
                            selectInput("pathway_size_by", "Size:",
                                choices = c(
                                    "Genes" = "gene_count",
                                    "Fold Enrichment" = "fold_enrichment"
                                ),
                                selected = "gene_count"
                            ),
                            numericInput("pathway_font_size", "Font Size:",
                                value = 12, min = 3, max = 18, step = 1
                            ),
                            numericInput("pathway_circle_size", "Circle Size:",
                                value = 4, min = 1, max = 10, step = 0.5
                            )
                        ),
                        div(
                            class = "settings-section",
                            div(class = "settings-section-title", icon("palette"), span("Color Palette")),
                            selectInput("pathway_color_palette", "Pathway gradient palette:",
                                choices = gc_palette_choices("gradient"),
                                selected = "custom"
                            ),
                            conditionalPanel(
                                condition = "input.pathway_color_palette == 'custom'",
                                colourInput("pathway_color_high", "Color: High", value = "#FF0000"),
                                colourInput("pathway_color_low", "Color: Low", value = "#0000FF")
                            ),
                            div(class = "settings-note", "Custom colors stay as default unless a palette is chosen.")
                        ),
                        div(
                            class = "settings-section",
                            div(class = "settings-section-title", icon("sliders-h"), span("Display")),
                            selectInput("pathway_chart_type", "Chart Type:",
                                choices = c(
                                    "Dot Plot" = "dotplot",
                                    "Bar Plot" = "barplot",
                                    "Lollipop" = "lollipop"
                                ),
                                selected = "barplot"
                            ),
                            numericInput("pathway_aspect_ratio", "Aspect Ratio:",
                                value = 1, min = 0.5, max = 4, step = 0.1
                            ),
                            selectInput("pathway_theme", "Plot Theme:",
                                choices = c(
                                    "Default" = "default",
                                    "Gray" = "gray",
                                    "Black & White" = "bw",
                                    "Light" = "light",
                                    "Dark" = "dark",
                                    "Classic" = "classic",
                                    "Minimal" = "minimal",
                                    "Line Draw" = "linedraw",
                                    "Add Grid" = "grid"
                                ),
                                selected = "default"
                            )
                        )
                    ),

                    # Table options panel
                    conditionalPanel(
                        condition = "input.pathway_result_type == 'table'",
                        div(
                            class = "settings-section",
                            div(class = "settings-section-title", icon("table"), span("Table Options")),
                            checkboxInput("pathway_show_genes", "Show Gene IDs/Names", value = FALSE),
                            helpText("Click column headers to sort the table.")
                        )
                    ),

                    # Tree options panel
                    conditionalPanel(
                        condition = "input.pathway_result_type == 'tree'",
                        div(
                            class = "settings-section",
                            div(class = "settings-section-title", icon("sitemap"), span("Tree Options")),
                            numericInput("pathway_tree_aspect", "Aspect Ratio:",
                                value = 1, min = 0.5, max = 3, step = 0.1
                            ),
                            helpText("Hierarchical clustering tree showing pathway correlation.")
                        )
                    ),

                    # Network options panel
                    conditionalPanel(
                        condition = "input.pathway_result_type == 'network'",
                        div(
                            class = "settings-section",
                            div(class = "settings-section-title", icon("project-diagram"), span("Network Options")),
                            actionButton("pathway_change_layout", "Change Layout",
                                class = "btn-info btn-sm btn-sm-block",
                                icon = icon("random")
                            ),
                            actionButton("pathway_static_plot", "Static Plot",
                                class = "btn-info btn-sm btn-sm-block",
                                icon = icon("image")
                            ),
                            numericInput("pathway_edge_cutoff", "Edge Cutoff:",
                                value = 0.3, min = 0, max = 1, step = 0.05
                            ),
                            checkboxInput("pathway_wrap_text", "Wrap Text", value = FALSE),
                            helpText("Nodes are pathways; edges show similarity (>=20% shared genes by default).")
                        )
                    )
                    )
                )
            )
        )
    )
}
