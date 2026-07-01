# ==================================================================
# Server Outputs Module
# Handles table outputs, text outputs, and file info
# ==================================================================

server_outputs <- function(input, output, session, gene_lists, gene_names_data,
                           all_overlaps, exact_regions, de_data) {
    # ---- Summary table ----
    output$summary_table <- renderTable(
        {
            req(gene_lists())
            lists <- gene_lists()
            data.frame(
                Dataset = names(lists),
                `Number of Genes` = sapply(lists, length),
                check.names = FALSE
            )
        },
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE
    )

    # ---- Exact Venn region table ----
    output$intersection_table <- renderTable(
        {
            req(exact_regions())
            regions <- exact_regions()

            if (length(regions) > 0) {
                data.frame(
                    `Exact Venn Region` = names(regions),
                    Count = vapply(regions, length, integer(1)),
                    check.names = FALSE
                )
            } else {
                data.frame(`Exact Venn Region` = "No regions found", Count = NA, check.names = FALSE)
            }
        },
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE
    )

    # ---- Inclusive intersection table ----
    output$inclusive_intersection_table <- renderTable(
        {
            req(all_overlaps())
            overlaps <- all_overlaps()
            intersection_names <- if (!is.null(overlaps)) {
                names(overlaps)[grepl(intersection_symbol(), names(overlaps), fixed = TRUE)]
            } else {
                character(0)
            }

            if (length(intersection_names) > 0) {
                data.frame(
                    `Inclusive Intersection` = intersection_names,
                    Count = vapply(overlaps[intersection_names], length, integer(1)),
                    check.names = FALSE
                )
            } else {
                data.frame(`Inclusive Intersection` = "No intersections found", Count = NA, check.names = FALSE)
            }
        },
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE
    )

    # ---- Gene table ----
    output$gene_table <- renderDT({
        req(all_overlaps(), exact_regions(), input$overlap_select)

        gene_df <- selected_gene_rows(input$overlap_select, all_overlaps(), exact_regions())
        if (nrow(gene_df) == 0) {
            return(datatable(data.frame(Message = "No genes in this overlap"),
                options = list(dom = "t")
            ))
        }
        genes <- gene_df$Gene_ID

        # Add gene names if available
        gene_names_map <- gene_names_data()
        if (!is.null(gene_names_map) && length(gene_names_map) > 0) {
            tryCatch(
                {
                    all_names <- c()
                    for (dataset in names(gene_names_map)) {
                        if (!is.null(gene_names_map[[dataset]])) {
                            all_names <- c(all_names, gene_names_map[[dataset]])
                        }
                    }

                    if (length(all_names) > 0) {
                        gene_df$Gene_Name <- vapply(genes, function(g) {
                            tryCatch(
                                {
                                    if (g %in% names(all_names)) {
                                        name <- all_names[[g]]
                                        if (!is.null(name) && !is.na(name) && name != "" && name != "NA") {
                                            return(as.character(name))
                                        }
                                    }
                                    "N/A"
                                },
                                error = function(e) "N/A"
                            )
                        }, character(1))
                    }
                },
                error = function(e) {
                    message("Error adding gene names: ", e$message)
                }
            )
        }

        # Add DE statistics if available
        de_data_list <- de_data()
        if (!is.null(de_data_list) && length(de_data_list) > 0) {
            tryCatch(
                {
                    all_de_data <- list()
                    for (dataset_name in names(de_data_list)) {
                        dataset_df <- de_data_list[[dataset_name]]
                        if (!is.null(dataset_df) && nrow(dataset_df) > 0) {
                            all_de_data[[dataset_name]] <- dataset_df
                        }
                    }

                    if (length(all_de_data) > 0) {
                        gene_df$Padj <- vapply(genes, function(g) {
                            padj_vals <- c()
                            for (dataset_df in all_de_data) {
                                if (input$gene_col %in% colnames(dataset_df) &&
                                    input$padj_col %in% colnames(dataset_df)) {
                                    match_idx <- which(as.character(dataset_df[[input$gene_col]]) == g)
                                    if (length(match_idx) > 0) {
                                        padj_vals <- c(padj_vals, dataset_df[[input$padj_col]][match_idx])
                                    }
                                }
                            }
                            if (length(padj_vals) > 0) {
                                return(formatC(min(as.numeric(padj_vals), na.rm = TRUE), format = "e", digits = 2))
                            }
                            return("N/A")
                        }, character(1))

                        gene_df$Log2FC <- vapply(genes, function(g) {
                            lfc_vals <- c()
                            for (dataset_df in all_de_data) {
                                if (input$gene_col %in% colnames(dataset_df) &&
                                    !is.null(input$lfc_col) && input$lfc_col != "" &&
                                    input$lfc_col %in% colnames(dataset_df)) {
                                    match_idx <- which(as.character(dataset_df[[input$gene_col]]) == g)
                                    if (length(match_idx) > 0) {
                                        lfc_vals <- c(lfc_vals, dataset_df[[input$lfc_col]][match_idx])
                                    }
                                }
                            }
                            if (length(lfc_vals) > 0) {
                                return(formatC(mean(as.numeric(lfc_vals), na.rm = TRUE), format = "f", digits = 3))
                            }
                            return("N/A")
                        }, character(1))
                    }
                },
                error = function(e) {
                    message("Error adding DE statistics: ", e$message)
                }
            )
        }

        datatable(gene_df,
            options = list(
                pageLength = 25, scrollX = TRUE,
                dom = "Bfrtip", buttons = c("copy", "csv", "excel")
            ),
            rownames = FALSE, class = "cell-border stripe"
        )
    })

    # ---- File info ----
    output$file_info <- renderPrint({
        if (is.na(input$num_datasets) || input$num_datasets < 2 || input$num_datasets > 8) {
            cat("Please enter a valid number of datasets (2-8)\n")
            return(NULL)
        }

        n <- input$num_datasets

        cat("============================================\n")
        cat("  DATA INPUT INFORMATION\n")
        cat("============================================\n\n")

        cat("Number of datasets specified:", n, "\n")
        cat(
            "Input method:", switch(input$input_method,
                "file" = "Upload DE result files",
                "genelist" = "Upload pre-filtered gene lists",
                "single" = "Single file with multiple lists",
                "paste" = "Paste gene IDs/names"
            ), "\n\n"
        )

        if (input$input_method == "file") {
            cat("Uploaded Files:\n")
            cat("--------------------------------------------\n")
            for (i in 1:n) {
                file <- input[[paste0("file", i)]]
                if (!is.null(file)) {
                    cat(sprintf("  Dataset %d: %s\n", i, file$name))
                    cat(sprintf("    Size: %.2f KB\n", file$size / 1024))
                } else {
                    cat(sprintf("  Dataset %d: (not uploaded)\n", i))
                }
            }

            cat("\n")
            cat("COLUMN MAPPINGS\n")
            cat("--------------------------------------------\n")
            cat("  Gene ID column:    ", input$gene_col %||% "Not set", "\n")
            cat(
                "  Gene Name column:  ",
                ifelse(is.null(input$gene_name_col) || input$gene_name_col == "",
                    "Not specified", input$gene_name_col
                ), "\n"
            )
            cat("  P-adj column:      ", input$padj_col %||% "Not set", "\n")
            cat("  Log2FC column:     ", input$lfc_col %||% "Not set", "\n")

            cat("\n")
            cat("FILTER SETTINGS\n")
            cat("--------------------------------------------\n")
            cat("  P-adj cutoff:       ", input$padj_cutoff, "\n")
            cat("  Log2FC cutoff:      ", input$lfc_cutoff, "\n")
            cat("  Apply Log2FC filter:", input$use_lfc, "\n")
            cat(
                "  Gene direction:     ",
                switch(input$gene_direction,
                    "all" = "All significant genes",
                    "up" = "Upregulated only",
                    "down" = "Downregulated only"
                ), "\n"
            )
        } else if (input$input_method == "genelist") {
            cat("Gene List Files:\n")
            cat("--------------------------------------------\n")
            for (i in 1:n) {
                file <- input[[paste0("genelist_file", i)]]
                name <- input[[paste0("genelist_name", i)]]
                if (!is.null(file)) {
                    cat(sprintf("  %s: %s\n", name, file$name))
                } else {
                    cat(sprintf("  %s: (not uploaded)\n", name))
                }
            }
        } else if (input$input_method == "single") {
            cat("Single File:\n")
            cat("--------------------------------------------\n")
            if (!is.null(input$single_file)) {
                cat("  File:", input$single_file$name, "\n")
                cat("  Type:", input$single_file_type, "\n")
                if (input$single_file_type == "sheets" && !is.null(input$selected_sheets)) {
                    cat("  Selected sheets:", paste(input$selected_sheets, collapse = ", "), "\n")
                } else if (input$single_file_type == "columns" && !is.null(input$selected_columns)) {
                    cat("  Selected columns:", paste(input$selected_columns, collapse = ", "), "\n")
                }
            }
        } else {
            cat("Pasted Gene Lists:\n")
            cat("--------------------------------------------\n")
            for (i in 1:n) {
                dataset_name <- input[[paste0("dataset_name", i)]]
                genes_text <- input[[paste0("genes", i)]]

                if (is.null(dataset_name) || dataset_name == "") {
                    dataset_name <- paste("Dataset", i)
                }

                gene_count <- 0
                if (!is.null(genes_text) && genes_text != "") {
                    genes <- strsplit(genes_text, "\n")[[1]]
                    genes <- trimws(genes)
                    genes <- genes[genes != ""]
                    gene_count <- length(unique(genes))
                }

                cat(sprintf("  %s: %d genes\n", dataset_name, gene_count))
            }
        }

        if (!is.null(gene_lists())) {
            cat("\n")
            cat("ANALYSIS RESULTS\n")
            cat("--------------------------------------------\n")
            lists <- gene_lists()
            for (name in names(lists)) {
                cat(sprintf("  %s: %d genes\n", name, length(lists[[name]])))
            }
        }
    })

    # ---- References ----
    output$references <- renderPrint({
        cat("Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2025). shiny: Web Application Framework for R. doi:10.32614/CRAN.package.shiny

Attali D (2021). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. doi:10.32614/CRAN.package.shinyjs

Attali D (2023). colourpicker: A Colour Picker Tool for Shiny and for Selecting Colours in Plots. doi:10.32614/CRAN.package.colourpicker

Chen H (2022). VennDiagram: Generate High-Resolution Venn and Euler Plots. doi:10.32614/CRAN.package.VennDiagram

Yan L (2025). ggvenn: Draw Venn Diagram by 'ggplot2'. doi:10.32614/CRAN.package.ggvenn

Gao C, Dusa A (2025). ggVennDiagram: A 'ggplot2' Implement of Venn Diagram. doi:10.32614/CRAN.package.ggVennDiagram

Wickham H, Francois R, Henry L, Muller K, Vaughan D (2023). dplyr: A Grammar of Data Manipulation. doi:10.32614/CRAN.package.dplyr

Xie Y, Cheng J, Tan X, Aden-Buie G (2025). DT: A Wrapper of the JavaScript Library 'DataTables'. doi:10.32614/CRAN.package.DT

Perrier V, Meyer F, Granjon D (2025). shinyWidgets: Custom Inputs Widgets for Shiny. doi:10.32614/CRAN.package.shinyWidgets

Wickham H, Bryan J (2025). readxl: Read Excel Files. doi:10.32614/CRAN.package.readxl

Schauberger P, Walker A (2025). openxlsx: Read, Write and Edit xlsx Files. doi:10.32614/CRAN.package.openxlsx

Gehlenborg N (2019). UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets. doi:10.32614/CRAN.package.UpSetR

Larsson J, Gustafsson P (2018). A Case Study in Fitting Area-Proportional Euler Diagrams with Ellipses Using eulerr. Proceedings of International Workshop on Set Visualization and Reasoning, 2116:84-91.

Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. ISBN 978-3-319-24277-4.

Qiu Y (2024). showtext: Using Fonts More Easily in R Graphs. doi:10.32614/CRAN.package.showtext

Kolberg L, Raudvere U, Kuzmin I, Vilo J, Peterson H (2020). gprofiler2- an R package for gene list functional enrichment analysis and namespace conversion toolset g:Profiler. F1000Research, 9(ELIXIR):709.

de Vries A, Ripley BD (2024). ggdendro: Create Dendrograms and Tree Diagrams Using 'ggplot2'. doi:10.32614/CRAN.package.ggdendro

Almende B.V. and Contributors, Thieurmel B (2025). visNetwork: Network Visualization using 'vis.js' Library. doi:10.32614/CRAN.package.visNetwork

Csardi G, Nepusz T, Traag V, Horvat Sz, Zanini F, Noom D, Muller K, Schoch D, Salmon M (2025). igraph: Network Analysis and Visualization. doi:10.5281/zenodo.7682609

Vaidyanathan R, Xie Y, Allaire J, Cheng J, Sievert C, Russell K (2023). htmlwidgets: HTML Widgets for R. doi:10.32614/CRAN.package.htmlwidgets

Wickham H, Pedersen T, Seidel D (2025). scales: Scale Functions for Visualization. doi:10.32614/CRAN.package.scales

R Core Team (2025). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna, Austria.")
    })
}
