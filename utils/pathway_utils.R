# ==================================================================
# Pathway Analysis Utility Functions
# Helper functions for pathway enrichment analysis and visualization
# ==================================================================

# Parse gene list from text input
parse_gene_list <- function(text) {
    if (is.null(text) || text == "" || trimws(text) == "") {
        return(NULL)
    }
    genes <- unlist(strsplit(text, "\\s+|,|;"))
    genes <- genes[genes != ""]
    genes <- trimws(genes)
    return(unique(genes))
}

#' Filter a gene query list by expression direction
#'
#' Uses the Log2FC column mapped on the Data Input tab to determine, per
#' gene, whether it is up- or down-regulated across the datasets that make
#' up the currently selected overlap/dataset. Genes for which no numeric
#' Log2FC value can be found are excluded (their direction is unknown).
#'
#' @param genes Character vector of gene IDs to filter
#' @param de_data_list Named list of per-dataset, significance-filtered data frames
#' @param gene_col Name of the gene ID column
#' @param lfc_col Name of the Log2FC column
#' @param direction One of "up", "down", "both"
#' @return List with `genes` (filtered vector) and `dropped` (excluded count)
filter_genes_by_direction <- function(genes, de_data_list, gene_col, lfc_col, direction) {
    if (is.null(genes) || length(genes) == 0 || identical(direction, "both")) {
        return(list(genes = genes, dropped = 0L))
    }
    if (is.null(de_data_list) || length(de_data_list) == 0 ||
        is.null(gene_col) || is.null(lfc_col) || !nzchar(lfc_col)) {
        return(list(genes = genes, dropped = 0L))
    }

    lfc_for_gene <- function(g) {
        values <- c()
        for (dataset_df in de_data_list) {
            if (!is.null(dataset_df) && gene_col %in% colnames(dataset_df) &&
                lfc_col %in% colnames(dataset_df)) {
                match_idx <- which(as.character(dataset_df[[gene_col]]) == g)
                if (length(match_idx) > 0) {
                    values <- c(values, suppressWarnings(as.numeric(dataset_df[[lfc_col]][match_idx])))
                }
            }
        }
        values <- values[!is.na(values)]
        if (length(values) == 0) {
            return(NA_real_)
        }
        mean(values)
    }

    lfc_values <- vapply(genes, lfc_for_gene, numeric(1))
    keep <- if (identical(direction, "up")) {
        !is.na(lfc_values) & lfc_values > 0
    } else {
        !is.na(lfc_values) & lfc_values < 0
    }

    list(genes = genes[keep], dropped = sum(!keep))
}

# Run gprofiler2 enrichment analysis
run_gprofiler_enrichment <- function(query, organism, sources, background = NULL,
                                     evcodes = TRUE, correction_method = "fdr",
                                     domain_scope = "annotated", user_threshold = 0.05,
                                     numeric_ns = "") {
    if (!requireNamespace("gprofiler2", quietly = TRUE)) {
        stop("Package 'gprofiler2' is required but not installed.")
    }

    tryCatch(
        {
            gostres <- gprofiler2::gost(
                query = query,
                organism = organism,
                sources = sources,
                custom_bg = background,
                evcodes = evcodes,
                correction_method = correction_method,
                domain_scope = domain_scope,
                user_threshold = user_threshold,
                significant = TRUE,
                numeric_ns = numeric_ns
            )
            return(gostres)
        },
        error = function(e) {
            stop(paste("Error in gprofiler2 enrichment:", e$message))
        }
    )
}

# Process enrichment results
process_enrichment_results <- function(gostres, num_pathways = 20, min_size = 2,
                                       max_size = 5000, remove_redundancy = TRUE,
                                       abbreviate = TRUE, show_ids = FALSE) {
    if (is.null(gostres) || is.null(gostres$result)) {
        return(NULL)
    }

    results <- gostres$result

    # Filter by pathway size
    results <- results[results$term_size >= min_size & results$term_size <= max_size, ]

    if (nrow(results) == 0) {
        return(NULL)
    }

    # Calculate fold enrichment
    results$fold_enrichment <- (results$intersection_size / results$query_size) /
        (results$term_size / results$effective_domain_size)

    # Add log10 FDR
    results$log_fdr <- -log10(results$p_value)

    # Remove redundancy if requested
    if (remove_redundancy && nrow(results) > 1) {
        results <- remove_redundant_terms(results)
    }

    # Sort by significance
    results <- results[order(results$p_value), ]

    # Limit to top N pathways
    if (nrow(results) > num_pathways) {
        results <- results[1:num_pathways, ]
    }

    # Abbreviate pathway names if requested
    if (abbreviate) {
        results$term_name_short <- abbreviate_pathway_names(results$term_name)
    } else {
        results$term_name_short <- results$term_name
    }

    # Add or remove IDs from display names
    if (show_ids) {
        results$display_name <- paste0(results$term_id, ": ", results$term_name_short)
    } else {
        results$display_name <- results$term_name_short
    }

    return(results)
}

# Remove redundant terms based on similarity
remove_redundant_terms <- function(results) {
    # Simple approach: remove terms with high gene overlap
    # More sophisticated methods can use semantic similarity

    if (nrow(results) <= 1) {
        return(results)
    }

    keep <- rep(TRUE, nrow(results))

    for (i in 1:(nrow(results) - 1)) {
        if (!keep[i]) next

        genes_i <- unlist(strsplit(results$intersection[i], ","))

        for (j in (i + 1):nrow(results)) {
            if (!keep[j]) next

            genes_j <- unlist(strsplit(results$intersection[j], ","))

            # Calculate Jaccard similarity
            intersection <- length(intersect(genes_i, genes_j))
            union <- length(union(genes_i, genes_j))
            jaccard <- intersection / union

            # Remove if similarity > 0.7
            if (jaccard > 0.7) {
                keep[j] <- FALSE
            }
        }
    }

    return(results[keep, ])
}

# Abbreviate pathway names
abbreviate_pathway_names <- function(names, max_length = 50) {
    sapply(names, function(name) {
        if (nchar(name) <= max_length) {
            return(name)
        } else {
            return(paste0(substr(name, 1, max_length - 3), "..."))
        }
    })
}

# Plot pathway enrichment
plot_pathway_enrichment <- function(results, sort_by = "fold_enrichment",
                                    x_axis = "fold_enrichment",
                                    color_by = "log_fdr",
                                    size_by = "gene_count",
                                    font_size = 12, circle_size = 4,
                                    color_high = "#FF0000", color_low = "#0000FF",
                                    color_palette = "custom",
                                    chart_type = "barplot", aspect_ratio = 2,
                                    theme_choice = "default") {
    if (is.null(results) || nrow(results) == 0) {
        return(ggplot() +
            ggtitle("No results to display"))
    }

    # Prepare data
    plot_data <- results
    plot_data$gene_count <- plot_data$intersection_size

    # Sort data
    if (sort_by == "fold_enrichment") {
        plot_data <- plot_data[order(plot_data$fold_enrichment), ]
    } else if (sort_by == "fdr") {
        plot_data <- plot_data[order(-plot_data$p_value), ]
    } else if (sort_by == "gene_count") {
        plot_data <- plot_data[order(plot_data$gene_count), ]
    } else if (sort_by == "name") {
        plot_data <- plot_data[order(plot_data$display_name), ]
    }

    # Set factor levels for proper ordering
    plot_data$display_name <- factor(plot_data$display_name,
        levels = plot_data$display_name
    )

    # Set x-axis variable
    x_var <- switch(x_axis,
        "fold_enrichment" = plot_data$fold_enrichment,
        "gene_count" = plot_data$gene_count,
        "log_fdr" = plot_data$log_fdr
    )

    # Set color variable
    color_var <- switch(color_by,
        "log_fdr" = plot_data$log_fdr,
        "fdr" = plot_data$p_value,
        "fold_enrichment" = plot_data$fold_enrichment
    )

    # Set size variable
    size_var <- switch(size_by,
        "gene_count" = plot_data$gene_count,
        "fold_enrichment" = plot_data$fold_enrichment
    )

    resolved_colors <- resolve_gradient_palette(color_palette, color_low, color_high)
    color_low <- unname(resolved_colors[["low"]])
    color_high <- unname(resolved_colors[["high"]])

    # Create base plot
    if (chart_type == "barplot") {
        p <- ggplot(plot_data, aes(x = x_var, y = display_name, fill = color_var)) +
            geom_bar(stat = "identity") +
            scale_fill_gradient(low = color_low, high = color_high)
    } else if (chart_type == "dotplot") {
        p <- ggplot(plot_data, aes(
            x = x_var, y = display_name,
            color = color_var, size = size_var
        )) +
            geom_point() +
            scale_color_gradient(low = color_low, high = color_high) +
            scale_size_continuous(range = c(2, circle_size * 2))
    } else if (chart_type == "lollipop") {
        p <- ggplot(plot_data, aes(x = x_var, y = display_name, color = color_var)) +
            geom_segment(aes(x = 0, xend = x_var, y = display_name, yend = display_name),
                color = "grey70"
            ) +
            geom_point(size = circle_size) +
            scale_color_gradient(low = color_low, high = color_high)
    }

    # Add labels
    x_label <- switch(x_axis,
        "fold_enrichment" = "Fold Enrichment",
        "gene_count" = "Gene Count",
        "log_fdr" = "-log10(FDR)"
    )

    color_label <- switch(color_by,
        "log_fdr" = "-log10(FDR)",
        "fdr" = "FDR",
        "fold_enrichment" = "Fold Enrichment"
    )

    p <- p +
        labs(x = x_label, y = "", fill = color_label, color = color_label) +
        theme_bw(base_size = font_size)

    # Apply theme
    p <- apply_plot_theme(p, theme_choice)

    return(p)
}

# Apply plot theme
apply_plot_theme <- function(p, theme_choice) {
    theme_func <- switch(theme_choice,
        "default" = theme_bw,
        "gray" = theme_gray,
        "bw" = theme_bw,
        "light" = theme_light,
        "dark" = theme_dark,
        "classic" = theme_classic,
        "minimal" = theme_minimal,
        "linedraw" = theme_linedraw,
        "grid" = theme_light,
        theme_bw
    )

    p <- p + theme_func()

    if (theme_choice == "grid") {
        p <- p + theme(
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_line(color = "gray90")
        )
    }

    return(p)
}

# Plot pathway tree (hierarchical clustering)
plot_pathway_tree <- function(results, aspect_ratio = 1) {
    if (is.null(results) || nrow(results) < 2) {
        return(ggplot() +
            ggtitle("Need at least 2 pathways for tree visualization"))
    }

    # Create similarity matrix based on gene overlap
    n <- nrow(results)
    sim_matrix <- matrix(0, n, n)
    rownames(sim_matrix) <- results$display_name
    colnames(sim_matrix) <- results$display_name

    for (i in 1:n) {
        genes_i <- unlist(strsplit(results$intersection[i], ","))
        for (j in 1:n) {
            if (i == j) {
                sim_matrix[i, j] <- 1
            } else {
                genes_j <- unlist(strsplit(results$intersection[j], ","))
                intersection <- length(intersect(genes_i, genes_j))
                union <- length(union(genes_i, genes_j))
                sim_matrix[i, j] <- intersection / union
            }
        }
    }

    # Convert to distance and cluster
    dist_matrix <- as.dist(1 - sim_matrix)
    hc <- hclust(dist_matrix, method = "average")

    # Plot dendrogram
    dend <- as.dendrogram(hc)

    # Convert to ggplot
    library(ggdendro)
    ggdendrogram(hc, rotate = FALSE, size = 2) +
        theme_minimal() +
        labs(
            title = "Pathway Similarity Tree",
            subtitle = "Based on shared genes"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create pathway table
create_pathway_table <- function(results, show_genes = FALSE) {
    if (is.null(results) || nrow(results) == 0) {
        return(NULL)
    }

    table_data <- data.frame(
        "Enrichment FDR" = format(results$p_value, scientific = TRUE, digits = 3),
        "nGenes" = results$intersection_size,
        "Pathway Genes" = results$term_size,
        "Fold Enrichment" = round(results$fold_enrichment, 2),
        "Pathways" = results$display_name,
        check.names = FALSE
    )

    if (show_genes) {
        table_data$"Gene IDs" <- results$intersection
    }

    DT::datatable(table_data,
        options = list(
            pageLength = 20,
            scrollX = TRUE,
            scrollY = "600px",
            order = list(list(0, "asc")) # Sort by FDR
        ),
        rownames = FALSE
    )
}

# Plot pathway network
plot_pathway_network <- function(results, edge_cutoff = 0.3, wrap_text = FALSE) {
    if (!requireNamespace("visNetwork", quietly = TRUE)) {
        return(htmltools::div("Package 'visNetwork' is required for network visualization."))
    }

    if (is.null(results) || nrow(results) < 2) {
        return(htmltools::div("Need at least 2 pathways for network visualization."))
    }

    # Calculate edges based on gene overlap
    edges_data <- calculate_pathway_edges(results, edge_cutoff)

    if (nrow(edges_data) == 0) {
        return(htmltools::div("No pathway connections found with current edge cutoff."))
    }

    # Prepare nodes
    nodes <- data.frame(
        id = seq_len(nrow(results)),
        label = if (wrap_text) {
            sapply(results$display_name, function(x) {
                paste(strwrap(x, width = 20), collapse = "\n")
            })
        } else {
            results$display_name
        },
        value = results$intersection_size,
        title = paste0(
            results$display_name, "<br>Genes: ", results$intersection_size,
            "<br>FDR: ", format(results$p_value, scientific = TRUE, digits = 3)
        ),
        color = scales::col_numeric("Blues", domain = NULL)(results$log_fdr)
    )

    # Create network
    visNetwork::visNetwork(nodes, edges_data, width = "100%", height = "700px") %>%
        visNetwork::visEdges(smooth = TRUE) %>%
        visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visNetwork::visInteraction(navigationButtons = TRUE) %>%
        visNetwork::visLayout(randomSeed = 123)
}

# Calculate pathway edges for network
calculate_pathway_edges <- function(results, edge_cutoff = 0.3) {
    empty_edges <- data.frame(
        from = integer(0), to = integer(0), value = numeric(0), title = character(0)
    )
    if (is.null(results) || nrow(results) < 2) {
        return(empty_edges)
    }

    n <- nrow(results)
    edges <- data.frame()

    # seq_len() (rather than 1:(n - 1)) avoids the classic R pitfall where
    # `1:(n - 1)` counts *down* from 1 to 0 when n == 1.
    for (i in seq_len(n - 1)) {
        genes_i <- unlist(strsplit(results$intersection[i], ","))
        for (j in (i + 1):n) {
            genes_j <- unlist(strsplit(results$intersection[j], ","))

            # Calculate Jaccard similarity
            intersection <- length(intersect(genes_i, genes_j))
            union <- length(union(genes_i, genes_j))
            jaccard <- intersection / union

            if (jaccard >= edge_cutoff) {
                edges <- rbind(edges, data.frame(
                    from = i,
                    to = j,
                    value = jaccard,
                    title = paste0("Similarity: ", round(jaccard, 3))
                ))
            }
        }
    }

    return(edges)
}

# Export network as image
export_network_image <- function(results, file, edge_cutoff = 0.3) {
    # Create static network plot using igraph
    if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("Package 'igraph' is required for network export.")
    }

    edges_data <- calculate_pathway_edges(results, edge_cutoff)

    if (nrow(edges_data) == 0) {
        warning("No edges to plot with current cutoff")
        return(NULL)
    }

    g <- igraph::graph_from_data_frame(edges_data,
        directed = FALSE,
        vertices = data.frame(name = seq_len(nrow(results)))
    )

    png(file, width = 1200, height = 900, res = 150)
    plot(g,
        vertex.label = results$display_name,
        vertex.size = results$intersection_size / 2,
        vertex.color = scales::col_numeric("Blues", domain = NULL)(results$log_fdr),
        edge.width = edges_data$value * 5,
        layout = igraph::layout_with_fr(g)
    )
    dev.off()
}

# Export network as HTML
export_network_html <- function(results, file, edge_cutoff = 0.3) {
    network_widget <- plot_pathway_network(results, edge_cutoff, wrap_text = FALSE)
    htmlwidgets::saveWidget(network_widget, file, selfcontained = TRUE)
}
