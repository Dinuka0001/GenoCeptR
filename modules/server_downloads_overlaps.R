# ==================================================================
# Server Downloads - Overlaps Section
# This file is sourced by server_downloads.R
# Handles the gene overlaps CSV download with DE statistics
# ==================================================================

# ---- Download overlaps ----
output$download_overlaps <- downloadHandler(
    filename = function() {
        selection <- input$overlap_select %||% ""
        selection_name <- switch(selection,
            "all::inclusive" = "all_inclusive_combinations",
            "all::exact" = "all_exact_venn_regions",
            "all::both" = "all_exact_and_inclusive_combinations",
            if (!is.null(selection) && nzchar(selection)) {
                resolved <- resolve_overlap_selection(selection, all_overlaps(), exact_regions())
                resolved$name
            } else {
                "gene_overlaps"
            }
        )
        selection_name <- gsub("[^A-Za-z0-9_-]+", "_", selection_name)
        paste0(selection_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
        req(all_overlaps(), exact_regions(), input$overlap_select)

        tryCatch(
            {
                out_df <- selected_gene_rows(input$overlap_select, all_overlaps(), exact_regions())
                gene_names_map <- gene_names_data()
                de_data_list <- de_data()

                # Check if input method is DE results file
                is_de_input <- !is.null(input$input_method) && input$input_method == "file"

                if (nrow(out_df) == 0) {
                    out_df <- if (is_de_input) {
                        data.frame(
                            Definition = character(0), Combination = character(0),
                            Gene_ID = character(0), Gene_Name = character(0),
                            Padj = character(0), Log2FoldChange = character(0),
                            stringsAsFactors = FALSE
                        )
                    } else {
                        data.frame(
                            Definition = character(0), Combination = character(0),
                            Gene_ID = character(0), stringsAsFactors = FALSE
                        )
                    }
                }

                all_names <- c()
                if (!is.null(gene_names_map) && length(gene_names_map) > 0) {
                    for (dataset in names(gene_names_map)) {
                        if (!is.null(gene_names_map[[dataset]])) {
                            all_names <- c(all_names, gene_names_map[[dataset]])
                        }
                    }
                }

                if (length(all_names) > 0 && nrow(out_df) > 0) {
                    out_df$Gene_Name <- vapply(out_df$Gene_ID, function(g) {
                        if (g %in% names(all_names)) {
                            nm <- all_names[[g]]
                            if (!is.null(nm) && !is.na(nm) && nm != "" && nm != "NA") {
                                return(as.character(nm))
                            }
                        }
                        "N/A"
                    }, character(1))
                } else if (is_de_input && !("Gene_Name" %in% colnames(out_df))) {
                    out_df$Gene_Name <- character(nrow(out_df))
                }

                # Add DE statistics (padj and log2FoldChange) if available
                if (is_de_input && nrow(out_df) > 0 &&
                    !is.null(de_data_list) && length(de_data_list) > 0 &&
                    !is.null(input$padj_col) && !is.null(input$lfc_col)) {
                    out_df$Padj <- vapply(out_df$Gene_ID, function(g) {
                            padj_vals <- c()
                            for (dataset_name in names(de_data_list)) {
                                dataset_df <- de_data_list[[dataset_name]]
                                if (!is.null(dataset_df) && input$gene_col %in% colnames(dataset_df) &&
                                    input$padj_col %in% colnames(dataset_df)) {
                                    match_idx <- which(as.character(dataset_df[[input$gene_col]]) == g)
                                    if (length(match_idx) > 0) {
                                        padj_vals <- c(padj_vals, dataset_df[[input$padj_col]][match_idx])
                                    }
                                }
                            }
                            if (length(padj_vals) > 0) {
                                min_padj <- min(as.numeric(padj_vals), na.rm = TRUE)
                                return(formatC(min_padj, format = "e", digits = 2))
                            }
                            return("N/A")
                        }, character(1))

                    out_df$Log2FoldChange <- vapply(out_df$Gene_ID, function(g) {
                            lfc_vals <- c()
                            for (dataset_name in names(de_data_list)) {
                                dataset_df <- de_data_list[[dataset_name]]
                                if (!is.null(dataset_df) && input$gene_col %in% colnames(dataset_df) &&
                                    input$lfc_col %in% colnames(dataset_df)) {
                                    match_idx <- which(as.character(dataset_df[[input$gene_col]]) == g)
                                    if (length(match_idx) > 0) {
                                        lfc_vals <- c(lfc_vals, dataset_df[[input$lfc_col]][match_idx])
                                    }
                                }
                            }
                            if (length(lfc_vals) > 0) {
                                mean_lfc <- mean(as.numeric(lfc_vals), na.rm = TRUE)
                                return(formatC(mean_lfc, format = "f", digits = 3))
                            }
                            return("N/A")
                        }, character(1))
                } else {
                    if (is_de_input && !("Padj" %in% colnames(out_df))) {
                        out_df$Padj <- character(nrow(out_df))
                    }
                    if (is_de_input && !("Log2FoldChange" %in% colnames(out_df))) {
                        out_df$Log2FoldChange <- character(nrow(out_df))
                    }
                }

                char_cols <- vapply(out_df, is.character, logical(1))
                out_df[char_cols] <- lapply(out_df[char_cols], enc2utf8)

                csv_escape <- function(x) {
                    x <- enc2utf8(as.character(x))
                    x[is.na(x)] <- ""
                    paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\"")
                }
                data_lines <- if (nrow(out_df) > 0) {
                    apply(out_df, 1, function(row) paste(csv_escape(row), collapse = ","))
                } else {
                    character(0)
                }
                csv_lines <- c(paste(csv_escape(names(out_df)), collapse = ","), data_lines)
                csv_text <- paste(enc2utf8(csv_lines), collapse = "\r\n")

                con <- file(file, open = "wb")
                on.exit(close(con), add = TRUE)
                writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
                writeBin(charToRaw(enc2utf8(csv_text)), con)
            },
            error = function(e) {
                showNotification(paste("Error creating CSV:", e$message),
                    type = "error", duration = 10
                )
            }
        )
    }
)
