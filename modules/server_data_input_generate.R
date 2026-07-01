# ==================================================================
# Server Data Input - Generate Analysis Section
# This file is sourced by server_data_input.R
# ==================================================================

# ---- Generate analysis ----
observeEvent(input$generate, {
    if (!is.null(analysis_status)) {
        analysis_status("processing")
    }

    reset_status <- function() {
        if (!is.null(analysis_status)) {
            if (!is.null(gene_lists())) {
                analysis_status("ready")
            } else {
                analysis_status("awaiting")
            }
        }
    }

    on.exit({
        if (!is.null(analysis_status) && identical(analysis_status(), "processing")) {
            reset_status()
        }
    }, add = TRUE)

    if (is.na(input$num_datasets) || input$num_datasets < 2 || input$num_datasets > 8) {
        showNotification("Please enter a valid number of datasets (2-8).", type = "warning", duration = 5)
        reset_status()
        return(NULL)
    }

    n_datasets <- input$num_datasets
    lists <- list()
    gene_names_map <- list()
    de_data_list <- list()
    de_data_unfiltered_list <- list()

    if (input$input_method == "file") {
        # DE RESULT FILES
        req(input$gene_col, input$padj_col)

        uploaded_files <- lapply(1:n_datasets, function(i) input[[paste0("file", i)]])
        if (any(vapply(uploaded_files, is.null, logical(1)))) {
            showNotification("Please upload all selected files.", type = "warning", duration = 5)
            reset_status()
            return(NULL)
        }

        if (input$use_lfc && (is.null(input$lfc_col) || input$lfc_col == "")) {
            showNotification(
                "Please specify Log2FC column when Log2FC filter is enabled.",
                type = "warning", duration = 5
            )
            reset_status()
            return(NULL)
        }

        withProgress(message = "Processing files...", value = 0, {
            for (i in 1:n_datasets) {
                incProgress(1 / n_datasets, detail = paste("Processing dataset", i))

                file <- input[[paste0("file", i)]]
                sep_i <- input[[paste0("sep", i)]]
                sheet_i <- input[[paste0("sheet", i)]]

                tryCatch(
                    {
                        df <- read_uploaded_file(file, sep_i, sheet_i, TRUE)
                        if (is.null(df)) next

                        if (!input$gene_col %in% colnames(df) || !input$padj_col %in% colnames(df)) {
                            showNotification(paste("Required columns not found in:", file$name),
                                type = "warning", duration = 5
                            )
                            next
                        }

                        if (input$use_lfc && !input$lfc_col %in% colnames(df)) {
                            showNotification(paste("Log2FC column not found in:", file$name),
                                type = "warning", duration = 5
                            )
                            next
                        }

                        dataset_name <- file_path_sans_ext(basename(file$name))

                        # Store unfiltered data
                        de_data_unfiltered_list[[dataset_name]] <- df

                        if (input$use_lfc) {
                            sig_df <- df %>%
                                mutate(
                                    padj_numeric = suppressWarnings(as.numeric(as.character(.data[[input$padj_col]]))),
                                    lfc_numeric = suppressWarnings(as.numeric(as.character(.data[[input$lfc_col]])))
                                ) %>%
                                filter(
                                    !is.na(padj_numeric), !is.na(lfc_numeric),
                                    padj_numeric < input$padj_cutoff,
                                    abs(lfc_numeric) > input$lfc_cutoff
                                )

                            if (input$gene_direction == "up") {
                                sig_df <- sig_df %>% filter(lfc_numeric > 0)
                            } else if (input$gene_direction == "down") {
                                sig_df <- sig_df %>% filter(lfc_numeric < 0)
                            }
                        } else {
                            sig_df <- df %>%
                                mutate(padj_numeric = suppressWarnings(as.numeric(as.character(.data[[input$padj_col]])))) %>%
                                filter(!is.na(padj_numeric), padj_numeric < input$padj_cutoff)
                        }

                        sig_genes <- unique(as.character(sig_df[[input$gene_col]]))
                        sig_genes <- sig_genes[!is.na(sig_genes) & sig_genes != ""]

                        if (length(sig_genes) == 0) {
                            showNotification(paste("No significant genes found in:", file$name),
                                type = "warning", duration = 5
                            )
                        }

                        lists[[dataset_name]] <- sig_genes
                        de_data_list[[dataset_name]] <- sig_df

                        if (!is.null(input$gene_name_col) && input$gene_name_col != "" &&
                            input$gene_name_col %in% colnames(sig_df)) {
                            gene_name_map <- setNames(
                                as.character(sig_df[[input$gene_name_col]]),
                                sig_genes
                            )
                            gene_names_map[[dataset_name]] <- gene_name_map
                        }
                    },
                    error = function(e) {
                        showNotification(paste("Error processing file:", file$name, "\nDetails:", e$message),
                            type = "error", duration = 10
                        )
                    }
                )
            }
        })

        de_data(de_data_list)
        de_data_unfiltered(de_data_unfiltered_list)
        gene_names_data(gene_names_map)
    } else if (input$input_method == "genelist") {
        # PRE-FILTERED GENE LIST FILES
        withProgress(message = "Processing gene list files...", value = 0, {
            for (i in 1:n_datasets) {
                incProgress(1 / n_datasets, detail = paste("Processing gene list", i))

                file <- input[[paste0("genelist_file", i)]]
                dataset_name <- input[[paste0("genelist_name", i)]]

                if (is.null(file)) {
                    showNotification(paste("Gene list file", i, "not uploaded."),
                        type = "warning", duration = 5
                    )
                    next
                }

                if (is.null(dataset_name) || dataset_name == "") {
                    dataset_name <- paste("Dataset", i)
                }

                tryCatch(
                    {
                        genes <- readLines(file$datapath, warn = FALSE)
                        genes <- trimws(genes)
                        genes <- genes[genes != "" & !is.na(genes)]
                        genes <- unique(genes)

                        if (length(genes) == 0) {
                            showNotification(paste("No genes found in file:", file$name),
                                type = "warning", duration = 5
                            )
                            next
                        }

                        lists[[dataset_name]] <- genes
                    },
                    error = function(e) {
                        showNotification(paste("Error reading file:", file$name, "\nDetails:", e$message),
                            type = "error", duration = 10
                        )
                    }
                )
            }
        })

        gene_names_data(NULL)
    } else if (input$input_method == "single") {
        # SINGLE FILE WITH MULTIPLE LISTS
        req(input$single_file)

        tryCatch(
            {
                header_flag <- input$single_has_header %||% TRUE
                sep_choice <- input$single_sep %||% "auto"
                if (input$single_file_type == "sheets") {
                    req(input$selected_sheets)
                    for (sheet in input$selected_sheets) {
                        df <- read_uploaded_file(input$single_file, sep_choice, sheet, header_flag)
                        genes <- df[[1]]
                        genes <- as.character(genes)
                        genes <- trimws(genes)
                        genes <- genes[genes != "" & !is.na(genes)]
                        genes <- unique(genes)
                        if (length(genes) > 0) {
                            sheet_name_input <- paste0("single_sheet_name_", make.names(sheet))
                            dataset_name <- input[[sheet_name_input]] %||% sheet
                            lists[[dataset_name]] <- genes
                        }
                    }
                } else {
                    req(input$selected_columns)
                    df <- read_uploaded_file(input$single_file, sep_choice, NULL, header_flag)
                    for (col in input$selected_columns) {
                        if (col %in% colnames(df)) {
                            genes <- df[[col]]
                            genes <- as.character(genes)
                            genes <- trimws(genes)
                            genes <- genes[genes != "" & !is.na(genes)]
                            genes <- unique(genes)
                            if (length(genes) > 0) {
                                col_name_input <- paste0("single_col_name_", make.names(col))
                                dataset_name <- input[[col_name_input]] %||% col
                                lists[[dataset_name]] <- genes
                            }
                        }
                    }
                }
            },
            error = function(e) {
                showNotification(paste("Error processing single file:", e$message),
                    type = "error", duration = 10
                )
            }
        )

        gene_names_data(NULL)
    } else {
        # PASTE GENES METHOD
        withProgress(message = "Processing gene lists...", value = 0, {
            for (i in 1:n_datasets) {
                incProgress(1 / n_datasets, detail = paste("Processing dataset", i))

                genes_text <- input[[paste0("genes", i)]]
                dataset_name <- input[[paste0("dataset_name", i)]]

                if (is.null(genes_text) || genes_text == "") {
                    showNotification(paste("Dataset", i, "is empty."),
                        type = "warning", duration = 5
                    )
                    next
                }

                if (is.null(dataset_name) || dataset_name == "") {
                    dataset_name <- paste("Dataset", i)
                }

                genes <- strsplit(genes_text, "\n")[[1]]
                genes <- trimws(genes)
                genes <- genes[genes != ""]
                genes <- unique(genes)

                if (length(genes) == 0) {
                    showNotification(paste("No genes found in Dataset", i),
                        type = "warning", duration = 5
                    )
                    next
                }

                lists[[dataset_name]] <- genes
            }
        })

        gene_names_data(NULL)
    }

    if (length(lists) == 0) {
        showNotification("No valid data to analyze.", type = "error", duration = 10)
        reset_status()
        return(NULL)
    }

    if (!is.null(names(lists)) && any(duplicated(names(lists)))) {
        names(lists) <- make.unique(names(lists))
        showNotification(
            "Duplicate dataset names detected - made unique.",
            type = "warning", duration = 5
        )
    }

    if (length(lists) < 2) {
        showNotification("At least 2 valid datasets are required.",
            type = "error", duration = 10
        )
        reset_status()
        return(NULL)
    }

    gene_lists(lists)

    overlaps <- calculate_overlaps(lists)
    all_overlaps(overlaps)
    regions <- calculate_venn_display_regions(lists)
    exact_regions(regions)

    updateSelectInput(
        session,
        "overlap_select",
        choices = overlap_select_choices(overlaps, regions),
        selected = if (length(regions) > 0) paste0("exact::", names(regions)[[1]]) else character(0)
    )

    n <- length(lists)
    diagram_settings$labels <- names(lists)
    diagram_settings$colors <- c(
        "#0073C2FF", "#EFC000FF", "#868686FF",
        "#CD534CFF", "#7AA6DCFF", "#8B4789FF",
        "#1B9E77FF", "#D95F02FF"
    )[1:n]

    updateNumericInput(session, "label_font_size", value = 1.0)
    updateNumericInput(session, "number_font_size", value = 1.5)

    if (!is.null(analysis_status)) {
        analysis_status("ready")
    }

    showNotification("Analysis complete!", type = "message", duration = 3)

    # Automatically switch to Visualization tab
    updateTabsetPanel(session, inputId = "main_tabs", selected = "visualization")
})
