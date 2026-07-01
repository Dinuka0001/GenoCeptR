# ==================================================================
# Plot Utilities Module
# Functions for creating and rendering various diagram types
# ==================================================================

#' Get plot settings from reactive inputs
#'
#' @param input Shiny input object
#' @param gene_lists Current gene lists
#' @param diagram_settings Reactive values for diagram settings
#' @return List with plot settings
get_plot_settings <- function(input, gene_lists, diagram_settings) {
    req(gene_lists)
    lists <- gene_lists
    n <- length(lists)

    plot_labels <- sapply(seq_along(diagram_settings$labels), function(i) {
        input[[paste0("label_", i)]] %||% diagram_settings$labels[i]
    })

    # Get colors based on diagram type and venn type
    if (!is.null(input$diagram_type) && input$diagram_type == "venn") {
        venn_type <- input$venn_type %||% "ggvenndiagram"

        if (venn_type == "ggvenn") {
            plot_colors <- sapply(seq_along(diagram_settings$colors), function(i) {
                input[[paste0("color_ggvenn_", i)]] %||% diagram_settings$colors[i]
            })
        } else if (venn_type == "venndiagram") {
            plot_colors <- sapply(seq_along(diagram_settings$colors), function(i) {
                input[[paste0("color_venndiagram_", i)]] %||% diagram_settings$colors[i]
            })
        } else {
            plot_colors <- sapply(seq_along(diagram_settings$colors), function(i) {
                input[[paste0("color_", i)]] %||% diagram_settings$colors[i]
            })
        }
    } else {
        plot_colors <- sapply(seq_along(diagram_settings$colors), function(i) {
            input[[paste0("color_", i)]] %||% diagram_settings$colors[i]
        })
    }

    plot_labels <- head(c(plot_labels, names(lists)), n)
    plot_colors <- head(c(plot_colors, diagram_settings$colors), n)
    plot_colors <- resolve_discrete_palette(input$venn_color_palette %||% "custom", n, plot_colors)

    plot_label_size <- input$label_font_size %||% 1.2
    plot_number_size <- input$number_font_size %||% 1.5
    show_percent <- input$show_percent %||% TRUE
    venn_gradient <- resolve_gradient_palette(
        input$venn_gradient_palette %||% "custom",
        input$ggvenndiagram_low_color %||% "#F4FAFE",
        input$ggvenndiagram_high_color %||% "#4981BF"
    )

    if (length(plot_labels) == n) {
        names(lists) <- plot_labels
    }

    list(
        lists        = lists,
        n            = n,
        labels       = plot_labels,
        colors       = plot_colors,
        label_size   = plot_label_size,
        number_size  = plot_number_size,
        show_percent = show_percent,
        venn_low     = unname(venn_gradient[["low"]]),
        venn_high    = unname(venn_gradient[["high"]]),
        upset_main   = input$upset_main_bar_color %||% "#0073C2FF",
        upset_sets   = input$upset_sets_bar_color %||% "#EFC000FF",
        upset_matrix = input$upset_matrix_color %||% "#404040"
    )
}

#' Main plotting function that routes to specific diagram types
#'
#' @param settings Plot settings from get_plot_settings
#' @param diagram_type Type of diagram to draw
#' @param input Shiny input object
draw_plot <- function(settings, diagram_type, input = NULL) {
    if (is.null(settings)) stop("Plot settings are not available.")

    if (diagram_type == "venn") {
        draw_venn_plot(settings, input)
    } else if (diagram_type == "upset") {
        draw_upset_plot(settings)
    } else if (diagram_type == "euler") {
        draw_euler_plot(settings)
    } else if (diagram_type == "edwards") {
        draw_edwards_plot(settings)
    }
}

#' Escape text used in interactive hover and detail panels
escape_hover_text <- function(x) {
    as.character(htmltools::htmlEscape(as.character(x)))
}

#' Build an interactive Venn diagram with hoverable gene-region details
build_interactive_venn <- function(settings, input) {
    if (settings$n < 2 || settings$n > 5) {
        stop("Interactive Venn diagrams support 2 to 5 datasets. Use an UpSet plot for 6 or more datasets.")
    }

    venn_data <- ggVennDiagram::process_data(
        ggVennDiagram::Venn(settings$lists)
    )
    region_edges <- venn_data$regionEdge
    region_labels <- venn_data$regionLabel
    set_edges <- venn_data$setEdge
    set_labels <- venn_data$setLabel

    low_color <- settings$venn_low %||% input$ggvenndiagram_low_color %||% "#F4FAFE"
    high_color <- settings$venn_high %||% input$ggvenndiagram_high_color %||% "#4981BF"
    max_count <- max(region_labels$count, 1L)
    fill_palette <- grDevices::colorRampPalette(c(low_color, high_color))(max_count + 1L)

    widget <- plotly::plot_ly()
    region_details <- vapply(seq_len(nrow(region_labels)), function(i) {
        genes <- unlist(region_labels$item[[i]], use.names = FALSE)
        gene_lines <- if (length(genes) > 0) {
            paste0(
                "<ol style='margin:8px 0 0 22px;padding:0;'>",
                paste0("<li>", escape_hover_text(genes), "</li>", collapse = ""),
                "</ol>"
            )
        } else {
            "<i>No genes</i>"
        }

        paste0(
            "<div style='font-size:16px;font-weight:600;margin-bottom:4px;'>",
            escape_hover_text(region_labels$name[[i]]),
            "</div>",
            "<div style='margin-bottom:8px;'><b>Count:</b> ",
            region_labels$count[[i]], "</div>",
            gene_lines
        )
    }, character(1))
    region_tooltips <- paste0(
        "<b>", escape_hover_text(region_labels$name), "</b>",
        "<br>Count: ", region_labels$count,
        "<br>All genes are shown in the panel."
    )

    for (region_id in unique(region_edges$id)) {
        edge <- region_edges[region_edges$id == region_id, , drop = FALSE]
        label_row <- region_labels[region_labels$id == region_id, , drop = FALSE]
        if (nrow(label_row) == 0) next

        region_index <- which(region_labels$id == region_id)[[1]]
        tooltip <- region_tooltips[[region_index]]
        region_detail <- region_details[[region_index]]
        fill_color <- fill_palette[label_row$count[[1]] + 1L]

        widget <- plotly::add_trace(
            widget,
            x = edge$X,
            y = edge$Y,
            type = "scatter",
            mode = "lines",
            fill = "toself",
            fillcolor = fill_color,
            line = list(color = "rgba(255,255,255,0.9)", width = 1),
            text = tooltip,
            meta = region_detail,
            hoveron = "fills+points",
            hoverinfo = "text",
            hovertemplate = paste0(tooltip, "<extra></extra>"),
            showlegend = FALSE
        )
    }

    for (set_id in unique(set_edges$id)) {
        edge <- set_edges[set_edges$id == set_id, , drop = FALSE]
        color_index <- suppressWarnings(as.integer(set_id))
        outline_color <- if (!is.na(color_index) && color_index <= length(settings$colors)) {
            settings$colors[[color_index]]
        } else {
            "#404040"
        }

        widget <- plotly::add_trace(
            widget,
            x = edge$X,
            y = edge$Y,
            type = "scatter",
            mode = "lines",
            line = list(color = outline_color, width = 2),
            hoverinfo = "skip",
            showlegend = FALSE
        )
    }

    widget <- plotly::add_trace(
        widget,
        x = region_labels$X,
        y = region_labels$Y,
        type = "scatter",
        mode = "markers+text",
        text = region_labels$count,
        customdata = region_details,
        marker = list(
            size = 50,
            color = "rgba(0,0,0,0.001)",
            line = list(width = 0)
        ),
        textfont = list(size = settings$number_size * 12, color = "#1f2933"),
        hovertemplate = paste0(region_tooltips, "<extra></extra>"),
        showlegend = FALSE
    )

    widget <- plotly::add_trace(
        widget,
        x = set_labels$X,
        y = set_labels$Y,
        type = "scatter",
        mode = "text",
        text = set_labels$name,
        textfont = list(size = settings$label_size * 13, color = "#1f2933"),
        hoverinfo = "skip",
        showlegend = FALSE
    )

    widget <- plotly::layout(
        widget,
        xaxis = list(visible = FALSE, fixedrange = FALSE, domain = c(0, 0.66)),
        yaxis = list(visible = FALSE, fixedrange = FALSE, scaleanchor = "x", scaleratio = 1),
        margin = list(l = 20, r = 20, b = 20, t = 20),
        hovermode = "closest",
        hoverdistance = 75,
        hoverlabel = list(
            align = "left",
            bgcolor = "white",
            bordercolor = "#495057",
            font = list(color = "#212529", size = 12)
        ),
        dragmode = "pan",
        plot_bgcolor = "white",
        paper_bgcolor = "white"
    )

    widget <- plotly::config(
        widget,
        displaylogo = FALSE,
        responsive = TRUE,
        scrollZoom = TRUE
    )

    htmlwidgets::onRender(
        widget,
        "
        function(el) {
          el.style.position = 'relative';

          var oldPanel = el.querySelector('.venn-gene-panel');
          if (oldPanel) oldPanel.remove();

          var panel = document.createElement('div');
          panel.className = 'venn-gene-panel';
          panel.style.position = 'absolute';
          panel.style.top = '10px';
          panel.style.right = '8px';
          panel.style.bottom = '10px';
          panel.style.width = '31%';
          panel.style.overflowY = 'auto';
          panel.style.overflowX = 'hidden';
          panel.style.padding = '12px 14px';
          panel.style.boxSizing = 'border-box';
          panel.style.background = 'rgba(255,255,255,0.97)';
          panel.style.border = '1px solid #ced4da';
          panel.style.borderRadius = '6px';
          panel.style.boxShadow = '0 2px 6px rgba(0,0,0,0.12)';
          panel.style.color = '#212529';
          panel.style.fontFamily = 'Arial, sans-serif';
          panel.style.fontSize = '13px';
          panel.style.lineHeight = '1.35';
          panel.style.zIndex = '20';
          panel.innerHTML =
            '<div style=\"font-size:16px;font-weight:600;margin-bottom:8px;\">Hovered region genes</div>' +
            '<div style=\"color:#6c757d;\">Hover over a region or its count. The complete gene list will remain here so it can be scrolled.</div>';
          el.appendChild(panel);

          function updatePanel(eventData) {
            if (!eventData || !eventData.points || !eventData.points.length) return;
            var point = eventData.points[0];
            var detail = point.customdata || (point.data && point.data.meta);
            if (Array.isArray(detail)) detail = detail[point.pointNumber];
            if (detail) panel.innerHTML = detail;
          }

          el.on('plotly_hover', updatePanel);
          el.on('plotly_click', updatePanel);
        }
        "
    )
}

configure_pandoc <- function() {
    if (rmarkdown::pandoc_available()) return(TRUE)

    program_files <- unique(c(
        Sys.getenv("ProgramFiles"),
        Sys.getenv("ProgramW6432"),
        Sys.getenv("LOCALAPPDATA")
    ))
    program_files <- program_files[nzchar(program_files)]

    candidate_dirs <- unique(c(
        Sys.getenv("RSTUDIO_PANDOC"),
        file.path(program_files, "RStudio", "resources", "app", "bin", "quarto", "bin", "tools"),
        file.path(program_files, "RStudio", "bin", "pandoc"),
        file.path(program_files, "Posit", "RStudio", "resources", "app", "bin", "quarto", "bin", "tools"),
        file.path(program_files, "Positron", "resources", "app", "quarto", "bin", "tools")
    ))
    candidate_dirs <- candidate_dirs[nzchar(candidate_dirs)]
    pandoc_name <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
    available_dirs <- candidate_dirs[file.exists(file.path(candidate_dirs, pandoc_name))]

    if (length(available_dirs) == 0) return(FALSE)
    Sys.setenv(RSTUDIO_PANDOC = available_dirs[[1]])
    rmarkdown::pandoc_available()
}

save_interactive_venn_html <- function(widget, file) {
    if (!requireNamespace("rmarkdown", quietly = TRUE) || !configure_pandoc()) {
        stop(
            "A standalone HTML export requires Pandoc. Install Quarto/RStudio or Pandoc, then restart the app."
        )
    }
    htmlwidgets::saveWidget(widget, file = file, selfcontained = TRUE)
}

#' Draw Venn diagram (ggVennDiagram, ggvenn, or VennDiagram)
draw_venn_plot <- function(settings, input) {
    venn_type <- input$venn_type %||% "ggvenndiagram"

    if (settings$n >= 2 && settings$n <= 5) {
        if (venn_type == "ggvenndiagram") {
            show_scale <- input$ggvenndiagram_show_scale %||% FALSE
            low_color <- settings$venn_low %||% input$ggvenndiagram_low_color %||% "#F4FAFE"
            high_color <- settings$venn_high %||% input$ggvenndiagram_high_color %||% "#4981BF"

            p <- ggVennDiagram(settings$lists,
                label = "count",
                label_alpha = 0,
                category.names = settings$labels,
                set_size = settings$label_size * 4,
                label_size = settings$number_size * 3,
                edge_size = 1.5
            )

            if (show_scale) {
                p <- p +
                    scale_fill_gradient(low = low_color, high = high_color, name = "Count") +
                    scale_color_manual(values = settings$colors[1:settings$n]) +
                    theme(
                        legend.position = "right",
                        legend.title = element_text(size = 12, face = "bold"),
                        legend.text = element_text(size = 10)
                    )
            } else {
                p <- p +
                    scale_fill_gradient(low = low_color, high = high_color) +
                    scale_color_manual(values = settings$colors[1:settings$n]) +
                    theme(legend.position = "none")
            }
            print(p)
        } else if (venn_type == "ggvenn") {
            fill_alpha <- input$ggvenn_alpha %||% 0.5
            stroke_size <- if (input$ggvenn_stroke_size %||% TRUE) 1 else 0
            show_percentage <- input$ggvenn_show_percentage %||% FALSE

            p <- ggvenn(settings$lists,
                fill_color = settings$colors,
                fill_alpha = fill_alpha,
                stroke_size = stroke_size,
                set_name_size = settings$label_size * 4,
                text_size = settings$number_size * 3,
                show_percentage = show_percentage
            )
            print(p)
        } else if (venn_type == "venndiagram") {
            fill_alpha <- input$venndiagram_alpha %||% 0.3
            draw_venndiagram_classic(settings, fill_alpha)
        }
    } else {
        # For 6-8 datasets: Venn diagrams are not practical
        plot.new()
        par(mar = c(1, 1, 1, 1))
        text(0.5, 0.6, paste("Traditional Venn diagrams are not suitable for", settings$n, "datasets."),
            cex = 1.6, col = "#CD534CFF", font = 2
        )
        text(0.5, 0.45, "Please use one of the following alternatives:",
            cex = 1.2, col = "#495057"
        )
        text(0.5, 0.35, "\u2022 UpSet Plot (recommended for many sets)",
            cex = 1.1, col = "#0073C2FF"
        )
        text(0.5, 0.28, "\u2022 Euler Diagram (proportional areas)",
            cex = 1.1, col = "#0073C2FF"
        )
    }
}

#' Draw classic VennDiagram package plots
draw_venndiagram_classic <- function(settings, fill_alpha) {
    grid.newpage()
    if (settings$n == 2) {
        draw.pairwise.venn(
            area1 = length(settings$lists[[1]]),
            area2 = length(settings$lists[[2]]),
            cross.area = length(intersect(settings$lists[[1]], settings$lists[[2]])),
            category = settings$labels,
            fill = settings$colors[1:2],
            alpha = rep(fill_alpha, 2),
            cat.cex = settings$label_size * 1.5,
            cex = settings$number_size * 1.5,
            cat.pos = c(-20, 20),
            cat.dist = rep(0.025, 2)
        )
    } else if (settings$n == 3) {
        a1 <- settings$lists[[1]]
        a2 <- settings$lists[[2]]
        a3 <- settings$lists[[3]]

        draw.triple.venn(
            area1 = length(a1),
            area2 = length(a2),
            area3 = length(a3),
            n12 = length(intersect(a1, a2)),
            n23 = length(intersect(a2, a3)),
            n13 = length(intersect(a1, a3)),
            n123 = length(Reduce(intersect, list(a1, a2, a3))),
            category = settings$labels,
            fill = settings$colors[1:3],
            alpha = rep(fill_alpha, 3),
            cat.cex = settings$label_size * 1.5,
            cex = settings$number_size * 1.5,
            cat.pos = c(-40, 40, 180),
            cat.dist = c(0.05, 0.05, 0.025)
        )
    } else if (settings$n == 4) {
        a1 <- settings$lists[[1]]
        a2 <- settings$lists[[2]]
        a3 <- settings$lists[[3]]
        a4 <- settings$lists[[4]]

        draw.quad.venn(
            area1 = length(a1), area2 = length(a2),
            area3 = length(a3), area4 = length(a4),
            n12 = length(intersect(a1, a2)),
            n13 = length(intersect(a1, a3)),
            n14 = length(intersect(a1, a4)),
            n23 = length(intersect(a2, a3)),
            n24 = length(intersect(a2, a4)),
            n34 = length(intersect(a3, a4)),
            n123 = length(Reduce(intersect, list(a1, a2, a3))),
            n124 = length(Reduce(intersect, list(a1, a2, a4))),
            n134 = length(Reduce(intersect, list(a1, a3, a4))),
            n234 = length(Reduce(intersect, list(a2, a3, a4))),
            n1234 = length(Reduce(intersect, list(a1, a2, a3, a4))),
            category = settings$labels,
            fill = settings$colors[1:4],
            alpha = rep(fill_alpha, 4),
            cat.cex = settings$label_size * 1.5,
            cex = settings$number_size * 1.5
        )
    } else if (settings$n == 5) {
        a1 <- settings$lists[[1]]
        a2 <- settings$lists[[2]]
        a3 <- settings$lists[[3]]
        a4 <- settings$lists[[4]]
        a5 <- settings$lists[[5]]

        draw.quintuple.venn(
            area1 = length(a1), area2 = length(a2),
            area3 = length(a3), area4 = length(a4), area5 = length(a5),
            n12 = length(intersect(a1, a2)), n13 = length(intersect(a1, a3)),
            n14 = length(intersect(a1, a4)), n15 = length(intersect(a1, a5)),
            n23 = length(intersect(a2, a3)), n24 = length(intersect(a2, a4)),
            n25 = length(intersect(a2, a5)), n34 = length(intersect(a3, a4)),
            n35 = length(intersect(a3, a5)), n45 = length(intersect(a4, a5)),
            n123 = length(Reduce(intersect, list(a1, a2, a3))),
            n124 = length(Reduce(intersect, list(a1, a2, a4))),
            n125 = length(Reduce(intersect, list(a1, a2, a5))),
            n134 = length(Reduce(intersect, list(a1, a3, a4))),
            n135 = length(Reduce(intersect, list(a1, a3, a5))),
            n145 = length(Reduce(intersect, list(a1, a4, a5))),
            n234 = length(Reduce(intersect, list(a2, a3, a4))),
            n235 = length(Reduce(intersect, list(a2, a3, a5))),
            n245 = length(Reduce(intersect, list(a2, a4, a5))),
            n345 = length(Reduce(intersect, list(a3, a4, a5))),
            n1234 = length(Reduce(intersect, list(a1, a2, a3, a4))),
            n1235 = length(Reduce(intersect, list(a1, a2, a3, a5))),
            n1245 = length(Reduce(intersect, list(a1, a2, a4, a5))),
            n1345 = length(Reduce(intersect, list(a1, a3, a4, a5))),
            n2345 = length(Reduce(intersect, list(a2, a3, a4, a5))),
            n12345 = length(Reduce(intersect, list(a1, a2, a3, a4, a5))),
            category = settings$labels,
            fill = settings$colors[1:5],
            alpha = rep(fill_alpha, 5),
            cat.cex = settings$label_size * 1.5,
            cex = settings$number_size * 1.5
        )
    }
}

#' Draw UpSet plot
draw_upset_plot <- function(settings) {
    lists <- settings$lists
    all_genes <- unique(unlist(lists))
    if (length(all_genes) == 0) {
        plot.new()
        text(0.5, 0.5, "No genes to display in UpSet plot", cex = 1.2)
        return()
    }
    mat <- sapply(lists, function(v) as.integer(all_genes %in% v))
    mat <- as.data.frame(mat)
    rownames(mat) <- all_genes

    UpSetR::upset(
        mat,
        nsets = settings$n,
        nintersects = 50,
        order.by = "freq",
        main.bar.color = settings$upset_main,
        sets.bar.color = settings$upset_sets,
        matrix.color = settings$upset_matrix,
        text.scale = c(1.3, 1.3, 1, 1, 1.5, 1.2)
    )
}

#' Draw Euler diagram
draw_euler_plot <- function(settings) {
    lists <- settings$lists
    euler_fit <- euler(lists)
    plot(euler_fit,
        fills = list(fill = settings$colors, alpha = 0.5),
        labels = list(fontsize = settings$label_size * 10),
        quantities = list(fontsize = settings$number_size * 8),
        legend = list(labels = settings$labels)
    )
}

#' Draw Edwards' Venn diagram (elliptical Euler)
draw_edwards_plot <- function(settings) {
    lists <- settings$lists
    euler_fit <- euler(lists, shape = "ellipse")
    plot(euler_fit,
        fills = list(fill = settings$colors, alpha = 0.4),
        edges = list(col = settings$colors, lwd = 2),
        labels = list(fontsize = settings$label_size * 10, col = "black", font = 2),
        quantities = list(fontsize = settings$number_size * 8, col = "black", font = 1),
        legend = list(labels = settings$labels)
    )
}
