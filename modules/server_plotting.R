# ==================================================================
# Server Plotting Module
# Handles plot rendering and diagram customization
# ==================================================================

server_plotting <- function(input, output, session, gene_lists, gene_names_data,
                            all_overlaps, de_data, diagram_settings) {
    # ---- Dynamic UI: labels ----
    output$label_inputs <- renderUI({
        req(diagram_settings$labels)
        labels <- diagram_settings$labels

        tagList(
            lapply(seq_along(labels), function(i) {
                textInput(paste0("label_", i),
                    label = paste("Set", i, "label:"),
                    value = labels[i]
                )
            })
        )
    })

    # ---- Dynamic UI: colors for Euler/Edwards ----
    output$color_inputs <- renderUI({
        req(diagram_settings$colors, diagram_settings$labels)
        colors <- diagram_settings$colors
        labels <- diagram_settings$labels

        tagList(
            lapply(seq_along(colors), function(i) {
                colourInput(paste0("color_", i),
                    label = paste("Set", i, "color:", labels[i]),
                    value = colors[i]
                )
            })
        )
    })

    # ---- Dynamic UI: colors for ggvenn ----
    output$color_inputs_ggvenn <- renderUI({
        req(diagram_settings$colors, diagram_settings$labels)
        colors <- diagram_settings$colors
        labels <- diagram_settings$labels

        tagList(
            lapply(seq_along(colors), function(i) {
                colourInput(paste0("color_ggvenn_", i),
                    label = paste("Set", i, "fill:", labels[i]),
                    value = colors[i]
                )
            })
        )
    })

    # ---- Dynamic UI: colors for VennDiagram ----
    output$color_inputs_venndiagram <- renderUI({
        req(diagram_settings$colors, diagram_settings$labels)
        colors <- diagram_settings$colors
        labels <- diagram_settings$labels

        tagList(
            lapply(seq_along(colors), function(i) {
                colourInput(paste0("color_venndiagram_", i),
                    label = paste("Set", i, "fill:", labels[i]),
                    value = colors[i]
                )
            })
        )
    })

    # ---- Main plot output ----
    output$main_plot <- renderPlot({
        req(gene_lists(), input$diagram_type, input$diagram_type != "interactive_venn")
        tryCatch(
            {
                settings <- get_plot_settings(input, gene_lists(), diagram_settings)
                draw_plot(settings, input$diagram_type, input)
            },
            error = function(e) {
                showNotification(paste("Error generating diagram:", e$message),
                    type = "error", duration = 10
                )
                plot.new()
                text(0.5, 0.5, paste("Error generating diagram\n", e$message),
                    cex = 1.2, col = "red"
                )
            }
        )
    })

    # ---- Interactive Venn diagram ----
    output$interactive_venn_plot <- plotly::renderPlotly({
        req(gene_lists(), input$diagram_type == "interactive_venn")
        tryCatch(
            {
                settings <- get_plot_settings(input, gene_lists(), diagram_settings)
                build_interactive_venn(settings, input)
            },
            error = function(e) {
                showNotification(paste("Error generating interactive Venn:", e$message),
                    type = "error", duration = 10
                )
                plotly::layout(
                    plotly::plot_ly(),
                    annotations = list(list(
                        text = paste("Error generating interactive Venn:<br>", escape_hover_text(e$message)),
                        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                        showarrow = FALSE, font = list(color = "red", size = 16)
                    )),
                    xaxis = list(visible = FALSE),
                    yaxis = list(visible = FALSE)
                )
            }
        )
    })
}
