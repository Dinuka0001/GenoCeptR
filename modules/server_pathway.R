# ==================================================================
# Server Pathway Analysis Module
# Handles pathway enrichment analysis and results visualization
# ==================================================================

server_pathway <- function(input, output, session, gene_lists, de_data, de_data_unfiltered, all_overlaps) {
    # Reactive values to store pathway analysis results
    pathway_results <- reactiveVal(NULL)
    enrichment_data <- reactiveVal(NULL)

    # Update overlap/dataset selector when gene lists or overlaps change
    observe({
        # Combine all available datasets
        all_choices <- c()

        # Add overlaps if available
        if (!is.null(all_overlaps()) && length(all_overlaps()) > 0) {
            all_choices <- c(all_choices, names(all_overlaps()))
        }

        # Update selector
        if (length(all_choices) > 0) {
            updateSelectInput(session, "pathway_overlap_select",
                choices = all_choices,
                selected = all_choices[1]
            )
        }
    })

    # Output flag to show/hide gene selection criteria when DE data is available
    output$pathway_has_de_data <- reactive({
        !is.null(de_data_unfiltered()) && length(de_data_unfiltered()) > 0
    })
    outputOptions(output, "pathway_has_de_data", suspendWhenHidden = FALSE)

    # Run pathway analysis
    observeEvent(input$run_pathway_analysis, {
        tryCatch(
            {
                # Show loading notification
                showNotification("Running pathway analysis...",
                    type = "message",
                    duration = NULL,
                    id = "pathway_loading"
                )

                # Prepare gene lists based on input method
                gene_query <- NULL
                background_genes <- NULL

                # First, get the query genes based on input method
                if (input$pathway_gene_input_method == "manual") {
                    # Parse manually pasted genes
                    gene_query <- parse_gene_list(input$pathway_manual_genes)
                    
                    if (is.null(gene_query) || length(gene_query) == 0) {
                        showNotification("Please paste a gene list.",
                            type = "error",
                            duration = 5
                        )
                        removeNotification("pathway_loading")
                        return(NULL)
                    }
                } else {
                    # Get genes from selected dataset/overlap
                    req(input$pathway_overlap_select)
                    req(all_overlaps())
                    
                    gene_query <- all_overlaps()[[input$pathway_overlap_select]]
                    
                    if (is.null(gene_query) || length(gene_query) == 0) {
                        showNotification("No genes available in selected dataset.",
                            type = "error",
                            duration = 5
                        )
                        removeNotification("pathway_loading")
                        return(NULL)
                    }

                    # Optional Gene Direction filter, using the Log2FC column
                    # mapped on the Data Input tab (only meaningful for DE
                    # result file inputs).
                    if (!is.null(de_data()) && length(de_data()) > 0 &&
                        !is.null(input$pathway_gene_direction) &&
                        input$pathway_gene_direction != "both") {
                        direction_result <- filter_genes_by_direction(
                            gene_query, de_data(), input$gene_col, input$lfc_col,
                            input$pathway_gene_direction
                        )
                        gene_query <- direction_result$genes

                        if (direction_result$dropped > 0) {
                            showNotification(
                                paste0(
                                    direction_result$dropped, " gene(s) excluded: no ",
                                    if (input$pathway_gene_direction == "up") "positive" else "negative",
                                    " Log2FC value found in the mapped data."
                                ),
                                type = "message", duration = 5
                            )
                        }

                        if (length(gene_query) == 0) {
                            showNotification(
                                "No genes remain after applying the Gene Direction filter.",
                                type = "error", duration = 5
                            )
                            removeNotification("pathway_loading")
                            return(NULL)
                        }
                    }
                }

                # Then, handle background based on background type
                if (input$pathway_background_type == "paste") {
                    # Parse pasted background genes
                    background_genes <- parse_gene_list(input$pathway_bg_list)

                    if (is.null(background_genes) || length(background_genes) == 0) {
                        showNotification("Please provide background genes.",
                            type = "error",
                            duration = 5
                        )
                        removeNotification("pathway_loading")
                        return(NULL)
                    }
                } else if (input$pathway_background_type == "auto") {
                    # Let gprofiler2 use automatic background
                    background_genes <- NULL
                }

                if (is.null(gene_query) || length(gene_query) == 0) {
                    showNotification("No genes available for analysis.",
                        type = "error",
                        duration = 5
                    )
                    removeNotification("pathway_loading")
                    return(NULL)
                }

                # Determine numeric namespace based on gene type selection.
                # Purely numeric identifiers (e.g. Entrez Gene IDs) are
                # ambiguous to gprofiler2 without an explicit namespace hint;
                # gene symbols/names are never numeric, so no hint is needed.
                numeric_ns <- if (input$pathway_gene_type == "gene_id") {
                    "ENTREZGENE_ACC"
                } else {
                    ""
                }

                # Run enrichment analysis using gprofiler2
                gostres <- run_gprofiler_enrichment(
                    query = gene_query,
                    organism = input$pathway_species,
                    sources = input$pathway_database,
                    background = background_genes,
                    evcodes = TRUE,
                    correction_method = "fdr",
                    domain_scope = if (is.null(background_genes)) "annotated" else "custom",
                    user_threshold = input$pathway_fdr_cutoff,
                    numeric_ns = numeric_ns
                )

                if (is.null(gostres) || nrow(gostres$result) == 0) {
                    showNotification("No significant pathways found.",
                        type = "warning",
                        duration = 5
                    )
                    removeNotification("pathway_loading")
                    return(NULL)
                }

                # Process results
                enrichment_results <- process_enrichment_results(
                    gostres,
                    num_pathways = input$pathway_num_show,
                    min_size = input$pathway_min_size,
                    max_size = input$pathway_max_size,
                    remove_redundancy = input$pathway_remove_redundancy,
                    abbreviate = input$pathway_abbreviate,
                    show_ids = input$pathway_show_ids
                )

                # Store results
                pathway_results(gostres)
                enrichment_data(enrichment_results)

                removeNotification("pathway_loading")
                showNotification("Pathway analysis completed successfully!",
                    type = "message",
                    duration = 3
                )
            },
            error = function(e) {
                removeNotification("pathway_loading")
                showNotification(paste("Error in pathway analysis:", e$message),
                    type = "error",
                    duration = 10
                )
            }
        )
    })

    # Render enrichment plot
    output$pathway_enrichment_plot <- renderPlot({
        req(enrichment_data())

        plot_pathway_enrichment(
            enrichment_data(),
            sort_by = input$pathway_sort_by,
            x_axis = input$pathway_xaxis,
            color_by = input$pathway_color_by,
            size_by = input$pathway_size_by,
            font_size = input$pathway_font_size,
            circle_size = input$pathway_circle_size,
            color_high = input$pathway_color_high,
            color_low = input$pathway_color_low,
            color_palette = input$pathway_color_palette,
            chart_type = input$pathway_chart_type,
            aspect_ratio = input$pathway_aspect_ratio,
            theme_choice = input$pathway_theme
        )
    }, width = function() { 600 * input$pathway_aspect_ratio }, height = 600)

    # Render pathway table
    output$pathway_table <- renderDT({
        req(enrichment_data())

        create_pathway_table(
            enrichment_data(),
            show_genes = input$pathway_show_genes
        )
    })

    # Render tree plot
    output$pathway_tree_plot <- renderPlot({
        req(enrichment_data())

        plot_pathway_tree(
            enrichment_data(),
            aspect_ratio = input$pathway_tree_aspect
        )
    })

    # Render network plot
    output$pathway_network_plot <- renderUI({
        req(enrichment_data())

        plot_pathway_network(
            enrichment_data(),
            edge_cutoff = input$pathway_edge_cutoff,
            wrap_text = input$pathway_wrap_text
        )
    })

    # Change network layout
    observeEvent(input$pathway_change_layout, {
        # Trigger layout change in network plot
        showNotification("Layout changed", type = "message", duration = 1)
    })

    # Generate static network plot
    observeEvent(input$pathway_static_plot, {
        req(enrichment_data())
        # Generate static version of network
        showNotification("Generating static plot...", type = "message", duration = 2)
    })

    # Download handlers
    # A shared helper keeps every pathway download handler consistent:
    # it requires results to exist and reports failures via notification
    # instead of letting the download crash silently.
    make_pathway_plot <- function() {
        plot_pathway_enrichment(
            enrichment_data(),
            sort_by = input$pathway_sort_by,
            x_axis = input$pathway_xaxis,
            color_by = input$pathway_color_by,
            size_by = input$pathway_size_by,
            font_size = input$pathway_font_size,
            circle_size = input$pathway_circle_size,
            color_high = input$pathway_color_high,
            color_low = input$pathway_color_low,
            color_palette = input$pathway_color_palette,
            chart_type = input$pathway_chart_type,
            aspect_ratio = input$pathway_aspect_ratio,
            theme_choice = input$pathway_theme
        )
    }

    with_pathway_download_error_handling <- function(label, expr_fn) {
        tryCatch(
            expr_fn(),
            error = function(e) {
                showNotification(paste0("Error saving ", label, ": ", e$message),
                    type = "error", duration = 10
                )
            }
        )
    }

    # Download enrichment plot PNG
    output$download_pathway_plot_png <- downloadHandler(
        filename = function() {
            paste0("pathway_enrichment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
        },
        content = function(file) {
            req(enrichment_data())
            with_pathway_download_error_handling("pathway plot PNG", function() {
                aspect <- input$pathway_aspect_ratio
                ggsave(file, plot = make_pathway_plot(), width = 8 * aspect, height = 8, dpi = 300)
            })
        }
    )

    # Download enrichment plot SVG
    output$download_pathway_plot_svg <- downloadHandler(
        filename = function() {
            paste0("pathway_enrichment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg")
        },
        content = function(file) {
            req(enrichment_data())
            with_pathway_download_error_handling("pathway plot SVG", function() {
                aspect <- input$pathway_aspect_ratio
                ggsave(file, plot = make_pathway_plot(), width = 8 * aspect, height = 8)
            })
        }
    )

    # Download enrichment plot PDF
    output$download_pathway_plot_pdf <- downloadHandler(
        filename = function() {
            paste0("pathway_enrichment_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
        },
        content = function(file) {
            req(enrichment_data())
            with_pathway_download_error_handling("pathway plot PDF", function() {
                aspect <- input$pathway_aspect_ratio
                ggsave(file, plot = make_pathway_plot(), width = 8 * aspect, height = 8)
            })
        }
    )

    # Download table
    output$download_pathway_table <- downloadHandler(
        filename = function() {
            paste0("pathway_table_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            results <- enrichment_data()
            req(results)
            with_pathway_download_error_handling("pathway table CSV", function() {
                table_data <- data.frame(
                    "Pathway" = results$display_name,
                    "Term_ID" = results$term_id,
                    "FDR" = results$p_value,
                    "Gene_Count" = results$intersection_size,
                    "Pathway_Size" = results$term_size,
                    "Fold_Enrichment" = results$fold_enrichment,
                    "Genes" = results$intersection,
                    check.names = FALSE
                )
                write.csv(table_data, file, row.names = FALSE)
            })
        },
        contentType = "text/csv"
    )

    # Download tree plot PNG
    output$download_pathway_tree_png <- downloadHandler(
        filename = function() {
            paste0("pathway_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
        },
        content = function(file) {
            req(enrichment_data())
            with_pathway_download_error_handling("pathway tree PNG", function() {
                ggsave(file,
                    plot = plot_pathway_tree(enrichment_data(), aspect_ratio = input$pathway_tree_aspect),
                    width = 12, height = 10, dpi = 300
                )
            })
        }
    )

    # Download tree plot SVG
    output$download_pathway_tree_svg <- downloadHandler(
        filename = function() {
            paste0("pathway_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg")
        },
        content = function(file) {
            req(enrichment_data())
            with_pathway_download_error_handling("pathway tree SVG", function() {
                ggsave(file,
                    plot = plot_pathway_tree(enrichment_data(), aspect_ratio = input$pathway_tree_aspect),
                    width = 12, height = 10
                )
            })
        }
    )

    # Download tree plot PDF
    output$download_pathway_tree_pdf <- downloadHandler(
        filename = function() {
            paste0("pathway_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
        },
        content = function(file) {
            req(enrichment_data())
            with_pathway_download_error_handling("pathway tree PDF", function() {
                ggsave(file,
                    plot = plot_pathway_tree(enrichment_data(), aspect_ratio = input$pathway_tree_aspect),
                    width = 12, height = 10
                )
            })
        }
    )

    # Download network image
    output$download_pathway_network_img <- downloadHandler(
        filename = function() {
            paste0("pathway_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
        },
        content = function(file) {
            req(enrichment_data())
            with_pathway_download_error_handling("pathway network image", function() {
                export_network_image(enrichment_data(), file, edge_cutoff = input$pathway_edge_cutoff)
            })
        }
    )

    # Download network HTML
    output$download_pathway_network_html <- downloadHandler(
        filename = function() {
            paste0("pathway_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
        },
        content = function(file) {
            req(enrichment_data())
            with_pathway_download_error_handling("pathway network HTML", function() {
                export_network_html(enrichment_data(), file, edge_cutoff = input$pathway_edge_cutoff)
            })
        }
    )

    # Download network edges
    output$download_pathway_edges <- downloadHandler(
        filename = function() {
            paste0("pathway_edges_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            req(enrichment_data())
            with_pathway_download_error_handling("pathway edges CSV", function() {
                edges <- calculate_pathway_edges(enrichment_data(), edge_cutoff = input$pathway_edge_cutoff)
                write.csv(edges, file, row.names = FALSE, quote = TRUE)
            })
        },
        contentType = "text/csv"
    )

    # Download network nodes
    output$download_pathway_nodes <- downloadHandler(
        filename = function() {
            paste0("pathway_nodes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            results <- enrichment_data()
            req(results)
            with_pathway_download_error_handling("pathway nodes CSV", function() {
                node_data <- data.frame(
                    "Node_ID" = seq_len(nrow(results)),
                    "Pathway" = results$display_name,
                    "Term_ID" = results$term_id,
                    "FDR" = results$p_value,
                    "Gene_Count" = results$intersection_size,
                    "Fold_Enrichment" = results$fold_enrichment,
                    "Genes" = results$intersection,
                    check.names = FALSE
                )
                write.csv(node_data, file, row.names = FALSE)
            })
        },
        contentType = "text/csv"
    )
}
