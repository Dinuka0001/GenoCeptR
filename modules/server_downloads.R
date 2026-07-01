# ==================================================================
# Server Downloads Module
# Handles all download handlers for plots, summaries, and gene lists
# ==================================================================

server_downloads <- function(input, output, session, gene_lists, gene_names_data,
                             all_overlaps, exact_regions, de_data, diagram_settings) {
    # Helper function to get settings
    get_settings_for_download <- function() {
        get_plot_settings(input, gene_lists(), diagram_settings)
    }

    write_utf8_lines <- function(lines, file, bom = TRUE) {
        text <- paste(enc2utf8(lines), collapse = "\n")
        con <- file(file, open = "wb")
        on.exit(close(con), add = TRUE)
        if (bom) {
            writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
        }
        writeBin(charToRaw(enc2utf8(text)), con)
    }

    data_frame_utf8_lines <- function(df) {
        if (is.null(df) || nrow(df) == 0) {
            return(paste(names(df), collapse = "\t"))
        }
        df[] <- lapply(df, function(col) enc2utf8(as.character(col)))
        c(
            paste(enc2utf8(names(df)), collapse = "\t"),
            apply(df, 1, function(row) paste(enc2utf8(row), collapse = "\t"))
        )
    }

    # ---- Download Venn diagrams ----
    output$download_venn_png <- downloadHandler(
        filename = function() {
            paste0("venn_diagram_", Sys.Date(), ".png")
        },
        content = function(file) {
            tryCatch(
                {
                    png(file, width = 800, height = 800, res = 120)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    draw_plot(settings, "venn", input)
                },
                error = function(e) {
                    showNotification(paste("Error saving Venn PNG:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    output$download_venn_svg <- downloadHandler(
        filename = function() {
            paste0("venn_diagram_", Sys.Date(), ".svg")
        },
        content = function(file) {
            tryCatch(
                {
                    svg(file, width = 8, height = 8)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    draw_plot(settings, "venn", input)
                },
                error = function(e) {
                    showNotification(paste("Error saving Venn SVG:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    output$download_venn_pdf <- downloadHandler(
        filename = function() {
            paste0("venn_diagram_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            tryCatch(
                {
                    pdf(file, width = 8, height = 8)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    draw_plot(settings, "venn", input)
                },
                error = function(e) {
                    showNotification(paste("Error saving Venn PDF:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    output$download_interactive_venn_html <- downloadHandler(
        filename = function() {
            paste0("interactive_venn_", Sys.Date(), ".html")
        },
        content = function(file) {
            tryCatch(
                {
                    settings <- get_settings_for_download()
                    widget <- build_interactive_venn(settings, input)
                    save_interactive_venn_html(widget, file)
                },
                error = function(e) {
                    showNotification(paste("Error saving interactive Venn HTML:", e$message),
                        type = "error", duration = 10
                    )
                    stop(e)
                }
            )
        },
        contentType = "text/html"
    )

    # ---- Download UpSet plots ----
    output$download_upset_png <- downloadHandler(
        filename = function() {
            paste0("upset_plot_", Sys.Date(), ".png")
        },
        content = function(file) {
            tryCatch(
                {
                    png(file, width = 1200, height = 800, res = 120)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    lists <- settings$lists
                    all_genes <- unique(unlist(lists))

                    if (length(all_genes) > 0) {
                        mat <- sapply(lists, function(v) as.integer(all_genes %in% v))
                        mat <- as.data.frame(mat)
                        rownames(mat) <- all_genes

                        print(UpSetR::upset(
                            mat,
                            nsets = settings$n,
                            nintersects = 50,
                            order.by = "freq",
                            main.bar.color = settings$upset_main,
                            sets.bar.color = settings$upset_sets,
                            matrix.color = settings$upset_matrix,
                            text.scale = c(1.3, 1.3, 1, 1, 1.5, 1.2)
                        ))
                    }
                },
                error = function(e) {
                    showNotification(paste("Error saving UpSet PNG:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    output$download_upset_svg <- downloadHandler(
        filename = function() {
            paste0("upset_plot_", Sys.Date(), ".svg")
        },
        content = function(file) {
            tryCatch(
                {
                    svg(file, width = 12, height = 8)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    lists <- settings$lists
                    all_genes <- unique(unlist(lists))

                    if (length(all_genes) > 0) {
                        mat <- sapply(lists, function(v) as.integer(all_genes %in% v))
                        mat <- as.data.frame(mat)
                        rownames(mat) <- all_genes

                        print(UpSetR::upset(
                            mat,
                            nsets = settings$n,
                            nintersects = 50,
                            order.by = "freq",
                            main.bar.color = settings$upset_main,
                            sets.bar.color = settings$upset_sets,
                            matrix.color = settings$upset_matrix,
                            text.scale = c(1.3, 1.3, 1, 1, 1.5, 1.2)
                        ))
                    }
                },
                error = function(e) {
                    showNotification(paste("Error saving UpSet SVG:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    output$download_upset_pdf <- downloadHandler(
        filename = function() {
            paste0("upset_plot_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            tryCatch(
                {
                    pdf(file, width = 12, height = 8, onefile = FALSE)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    lists <- settings$lists
                    all_genes <- unique(unlist(lists))

                    if (length(all_genes) > 0) {
                        mat <- sapply(lists, function(v) as.integer(all_genes %in% v))
                        mat <- as.data.frame(mat)
                        rownames(mat) <- all_genes

                        print(UpSetR::upset(
                            mat,
                            nsets = settings$n,
                            nintersects = 50,
                            order.by = "freq",
                            main.bar.color = settings$upset_main,
                            sets.bar.color = settings$upset_sets,
                            matrix.color = settings$upset_matrix,
                            text.scale = c(1.3, 1.3, 1, 1, 1.5, 1.2)
                        ))
                    }
                },
                error = function(e) {
                    showNotification(paste("Error saving UpSet PDF:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    # ---- Download Euler diagrams ----
    output$download_euler_png <- downloadHandler(
        filename = function() {
            paste0("euler_diagram_", Sys.Date(), ".png")
        },
        content = function(file) {
            tryCatch(
                {
                    png(file, width = 800, height = 800, res = 120)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    lists <- settings$lists
                    euler_fit <- euler(lists)

                    print(plot(euler_fit,
                        fills = list(fill = settings$colors, alpha = 0.5),
                        labels = list(fontsize = settings$label_size * 10),
                        quantities = list(fontsize = settings$number_size * 8),
                        legend = list(labels = settings$labels)
                    ))
                },
                error = function(e) {
                    showNotification(paste("Error saving Euler PNG:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    output$download_euler_svg <- downloadHandler(
        filename = function() {
            paste0("euler_diagram_", Sys.Date(), ".svg")
        },
        content = function(file) {
            tryCatch(
                {
                    svg(file, width = 8, height = 8)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    lists <- settings$lists
                    euler_fit <- euler(lists)

                    print(plot(euler_fit,
                        fills = list(fill = settings$colors, alpha = 0.5),
                        labels = list(fontsize = settings$label_size * 10),
                        quantities = list(fontsize = settings$number_size * 8),
                        legend = list(labels = settings$labels)
                    ))
                },
                error = function(e) {
                    showNotification(paste("Error saving Euler SVG:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    output$download_euler_pdf <- downloadHandler(
        filename = function() {
            paste0("euler_diagram_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            tryCatch(
                {
                    pdf(file, width = 8, height = 8)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    lists <- settings$lists
                    euler_fit <- euler(lists)

                    print(plot(euler_fit,
                        fills = list(fill = settings$colors, alpha = 0.5),
                        labels = list(fontsize = settings$label_size * 10),
                        quantities = list(fontsize = settings$number_size * 8),
                        legend = list(labels = settings$labels)
                    ))
                },
                error = function(e) {
                    showNotification(paste("Error saving Euler PDF:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    # ---- Download Edwards diagrams ----
    output$download_edwards_png <- downloadHandler(
        filename = function() {
            paste0("edwards_diagram_", Sys.Date(), ".png")
        },
        content = function(file) {
            tryCatch(
                {
                    png(file, width = 800, height = 800, res = 120)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    lists <- settings$lists
                    euler_fit <- euler(lists, shape = "ellipse")

                    print(plot(euler_fit,
                        fills = list(fill = settings$colors, alpha = 0.4),
                        edges = list(col = settings$colors, lwd = 2),
                        labels = list(fontsize = settings$label_size * 10, col = "black", font = 2),
                        quantities = list(fontsize = settings$number_size * 8, col = "black", font = 1),
                        legend = list(labels = settings$labels)
                    ))
                },
                error = function(e) {
                    showNotification(paste("Error saving Edwards PNG:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    output$download_edwards_svg <- downloadHandler(
        filename = function() {
            paste0("edwards_diagram_", Sys.Date(), ".svg")
        },
        content = function(file) {
            tryCatch(
                {
                    svg(file, width = 8, height = 8)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    lists <- settings$lists
                    euler_fit <- euler(lists, shape = "ellipse")

                    print(plot(euler_fit,
                        fills = list(fill = settings$colors, alpha = 0.4),
                        edges = list(col = settings$colors, lwd = 2),
                        labels = list(fontsize = settings$label_size * 10, col = "black", font = 2),
                        quantities = list(fontsize = settings$number_size * 8, col = "black", font = 1),
                        legend = list(labels = settings$labels)
                    ))
                },
                error = function(e) {
                    showNotification(paste("Error saving Edwards SVG:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    output$download_edwards_pdf <- downloadHandler(
        filename = function() {
            paste0("edwards_diagram_", Sys.Date(), ".pdf")
        },
        content = function(file) {
            tryCatch(
                {
                    pdf(file, width = 8, height = 8)
                    on.exit(dev.off(), add = TRUE)

                    settings <- get_settings_for_download()
                    lists <- settings$lists
                    euler_fit <- euler(lists, shape = "ellipse")

                    print(plot(euler_fit,
                        fills = list(fill = settings$colors, alpha = 0.4),
                        edges = list(col = settings$colors, lwd = 2),
                        labels = list(fontsize = settings$label_size * 10, col = "black", font = 2),
                        quantities = list(fontsize = settings$number_size * 8, col = "black", font = 1),
                        legend = list(labels = settings$labels)
                    ))
                },
                error = function(e) {
                    showNotification(paste("Error saving Edwards PDF:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    # ---- Download summary ----
    output$download_summary_txt <- downloadHandler(
        filename = function() {
            paste0("overlap_summary_", Sys.Date(), ".txt")
        },
        content = function(file) {
            if (is.null(gene_lists()) || length(gene_lists()) == 0) {
                showNotification("No summary data to download.", type = "warning", duration = 5)
                return(NULL)
            }

            tryCatch(
                {
                    summary_df <- data.frame(
                        Dataset = names(gene_lists()),
                        `Number of Genes` = sapply(gene_lists(), length),
                        check.names = FALSE
                    )

                    regions <- exact_regions()
                    region_df <- if (!is.null(regions) && length(regions) > 0) {
                        data.frame(
                            `Exact Venn Region` = names(regions),
                            Count = vapply(regions, length, integer(1)),
                            check.names = FALSE
                        )
                    } else {
                        data.frame(`Exact Venn Region` = "No regions found", Count = NA, check.names = FALSE)
                    }

                    overlaps <- all_overlaps()
                    intersection_names <- if (!is.null(overlaps)) {
                        names(overlaps)[grepl(intersection_symbol(), names(overlaps), fixed = TRUE)]
                    } else {
                        character(0)
                    }

                    intersection_df <- if (length(intersection_names) > 0) {
                        data.frame(
                            `Inclusive Intersection` = intersection_names,
                            Count = vapply(overlaps[intersection_names], length, integer(1)),
                            check.names = FALSE
                        )
                    } else {
                        data.frame(`Inclusive Intersection` = "No intersections found", Count = NA, check.names = FALSE)
                    }

                    write_utf8_lines(c(
                        paste(rep("=", 62), collapse = ""),
                        "  GenoCeptR Analysis Summary",
                        "  Version 3.0",
                        paste("  Generated:", Sys.time()),
                        paste(rep("=", 62), collapse = ""),
                        "",
                        "Number of Genes per Set:",
                        paste(rep("-", 40), collapse = ""),
                        data_frame_utf8_lines(summary_df),
                        "",
                        "Venn Region Counts (exclusive; matches diagram):",
                        paste(rep("-", 40), collapse = ""),
                        data_frame_utf8_lines(region_df),
                        "",
                        "Inclusive Intersection Counts:",
                        paste(rep("-", 40), collapse = ""),
                        data_frame_utf8_lines(intersection_df),
                        "",
                        paste(rep("=", 62), collapse = "")
                    ), file)
                },
                error = function(e) {
                    showNotification(paste("Error creating summary file:", e$message),
                        type = "error", duration = 10
                    )
                }
            )
        }
    )

    # ---- Download file info ----
    output$download_file_info <- downloadHandler(
        filename = function() {
            paste0("data_input_info_", Sys.Date(), ".txt")
        },
        content = function(file) {
            if (is.na(input$num_datasets) || input$num_datasets < 2 || input$num_datasets > 8) {
                write_utf8_lines("Please enter a valid number of datasets (2-8)", file)
                return(NULL)
            }

            n <- input$num_datasets

            lines <- c()
            lines <- c(lines, paste(rep("=", 62), collapse = ""))
            lines <- c(lines, "  DATA INPUT INFORMATION")
            lines <- c(lines, paste(rep("=", 62), collapse = ""))
            lines <- c(lines, "")
            lines <- c(lines, paste("Number of datasets specified:", n))
            lines <- c(
                lines, paste("Input method:", switch(input$input_method,
                    "file" = "Upload DE result files",
                    "genelist" = "Upload pre-filtered gene lists",
                    "single" = "Single file with multiple lists",
                    "paste" = "Paste gene IDs/names"
                ))
            )
            lines <- c(lines, "")

            if (input$input_method == "file") {
                lines <- c(lines, "Uploaded Files:")
                lines <- c(lines, paste(rep("-", 40), collapse = ""))
                for (i in 1:n) {
                    file_obj <- input[[paste0("file", i)]]
                    if (!is.null(file_obj)) {
                        lines <- c(lines, sprintf("  Dataset %d: %s", i, file_obj$name))
                        lines <- c(lines, sprintf("    Size: %.2f KB", file_obj$size / 1024))
                    } else {
                        lines <- c(lines, sprintf("  Dataset %d: (not uploaded)", i))
                    }
                }

                lines <- c(lines, "")
                lines <- c(lines, "COLUMN MAPPINGS")
                lines <- c(lines, paste(rep("-", 40), collapse = ""))
                lines <- c(lines, "")
                lines <- c(lines, paste("  Gene ID column:    ", input$gene_col %||% "Not set"))
                lines <- c(
                    lines, paste(
                        "  Gene Name column:  ",
                        ifelse(is.null(input$gene_name_col) || input$gene_name_col == "",
                            "Not specified", input$gene_name_col
                        )
                    )
                )
                lines <- c(lines, paste("  P-adj column:      ", input$padj_col %||% "Not set"))
                lines <- c(lines, paste("  Log2FC column:     ", input$lfc_col %||% "Not set"))

                lines <- c(lines, "")
                lines <- c(lines, "FILTER SETTINGS")
                lines <- c(lines, paste(rep("-", 40), collapse = ""))
                lines <- c(lines, "")
                lines <- c(lines, paste("  P-adj cutoff:       ", input$padj_cutoff))
                lines <- c(lines, paste("  Log2FC cutoff:      ", input$lfc_cutoff))
                lines <- c(lines, paste("  Apply Log2FC filter:", input$use_lfc))
                lines <- c(
                    lines, paste(
                        "  Gene direction:     ",
                        switch(input$gene_direction,
                            "all" = "All significant genes",
                            "up" = "Upregulated only",
                            "down" = "Downregulated only"
                        )
                    )
                )
            } else if (input$input_method == "genelist") {
                lines <- c(lines, "Gene List Files:")
                lines <- c(lines, paste(rep("-", 40), collapse = ""))
                for (i in 1:n) {
                    file_obj <- input[[paste0("genelist_file", i)]]
                    name <- input[[paste0("genelist_name", i)]]
                    if (!is.null(file_obj)) {
                        lines <- c(lines, sprintf("  %s: %s", name, file_obj$name))
                    } else {
                        lines <- c(lines, sprintf("  %s: (not uploaded)", name))
                    }
                }
            } else if (input$input_method == "single") {
                lines <- c(lines, "Single File:")
                lines <- c(lines, paste(rep("-", 40), collapse = ""))
                if (!is.null(input$single_file)) {
                    lines <- c(lines, paste("  File:", input$single_file$name))
                    lines <- c(lines, paste("  Type:", input$single_file_type))
                    if (input$single_file_type == "sheets" && !is.null(input$selected_sheets)) {
                        lines <- c(lines, paste("  Selected sheets:", paste(input$selected_sheets, collapse = ", ")))
                    } else if (input$single_file_type == "columns" && !is.null(input$selected_columns)) {
                        lines <- c(lines, paste("  Selected columns:", paste(input$selected_columns, collapse = ", ")))
                    }
                }
            } else {
                lines <- c(lines, "Pasted Gene Lists:")
                lines <- c(lines, paste(rep("-", 40), collapse = ""))
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

                    lines <- c(lines, sprintf("  %s: %d genes", dataset_name, gene_count))
                }
            }

            if (!is.null(gene_lists())) {
                lines <- c(lines, "")
                lines <- c(lines, "ANALYSIS RESULTS")
                lines <- c(lines, paste(rep("-", 40), collapse = ""))
                lines <- c(lines, "")
                lists <- gene_lists()
                for (name in names(lists)) {
                    lines <- c(lines, sprintf("  %s: %d genes", name, length(lists[[name]])))
                }
            }

            lines <- c(lines, "")
            lines <- c(lines, paste(rep("=", 62), collapse = ""))

            write_utf8_lines(lines, file)
        }
    )

    # ---- Download overlaps (gene lists with DE stats) ----
    source("modules/server_downloads_overlaps.R", local = TRUE, encoding = "UTF-8")
}
