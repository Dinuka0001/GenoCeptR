# ==================================================================
# Server Data Input Module
# Handles all data input processing and validation
# ==================================================================

server_data_input <- function(input, output, session, gene_lists, gene_names_data,
                              all_overlaps, exact_regions, de_data, de_data_unfiltered, diagram_settings,
                              analysis_status = NULL) {
    # Debounce the dataset count so the dynamic upload UI only rebuilds after
    # the user stops typing. Without this, every keystroke tears down and
    # recreates the file inputs, which makes the upload field feel unresponsive.
    num_datasets_debounced <- debounce(reactive({
        v <- suppressWarnings(as.integer(input$num_datasets))
        if (is.na(v) || v < 2 || v > 8) NA_integer_ else v
    }), 450)

    # Validate number of datasets input
    observe({
        v <- num_datasets_debounced()
        if (is.na(v) || is.null(v)) {
            shinyjs::show("dataset_count_warning")
            shinyjs::disable("generate")
        } else {
            shinyjs::hide("dataset_count_warning")
            shinyjs::enable("generate")
        }
    })

    # Auto-switch to UpSet plot for 6+ datasets
    observeEvent(gene_lists(), {
        lists <- gene_lists()
        if (!is.null(lists) && length(lists) >= 6) {
            if (input$diagram_type %in% c("venn", "interactive_venn")) {
                updateSelectInput(session, "diagram_type", selected = "upset")
                showNotification(
                    paste(
                        "Automatically switched to UpSet Plot.",
                        "Venn diagrams are not suitable for", length(lists), "datasets."
                    ),
                    type = "message", duration = 5
                )
            }
        }
    })

    # ---- Dynamic upload UI for DE files ----
    output$file_upload_ui <- renderUI({
        n <- num_datasets_debounced()
        if (is.na(n)) return(NULL)

        tagList(
            lapply(1:n, function(i) {
                wellPanel(
                    h5(icon("database"), paste("Dataset", i)),
                    fileInput(paste0("file", i),
                        label = "Upload CSV / TSV / Excel",
                        multiple = FALSE,
                        accept = c("text/csv", ".csv", ".tsv", ".txt", ".xlsx", ".xls")
                    ),
                    selectInput(paste0("sep", i), "Column separator (for text files):",
                        choices = c(
                            "Auto" = "auto", "Comma (,)" = ",",
                            "Tab (\\t)" = "\t", "Semicolon (;)" = ";"
                        ),
                        selected = "auto"
                    ),
                    uiOutput(paste0("sheet_ui", i))
                )
            })
        )
    })

    # ---- Dynamic upload UI for gene list files ----
    output$genelist_upload_ui <- renderUI({
        n <- num_datasets_debounced()
        if (is.na(n)) return(NULL)

        tagList(
            lapply(1:n, function(i) {
                wellPanel(
                    h5(icon("list"), paste("Gene List", i)),
                    textInput(paste0("genelist_name", i),
                        label = "Dataset name:",
                        value = paste("Dataset", i)
                    ),
                    fileInput(paste0("genelist_file", i),
                        label = "Upload gene list file:",
                        accept = c("text/plain", ".txt", ".csv", ".tsv", "text/csv")
                    ),
                    helpText("File should contain one gene per line")
                )
            })
        )
    })

    # ---- Single file options UI ----
    output$single_file_options_ui <- renderUI({
        req(input$single_file)

        tagList(
            uiOutput("single_sep_ui"),
            checkboxInput("single_has_header", "First row contains column headers", value = TRUE),
            helpText("If unchecked, first row will be treated as data."),
            hr(),
            radioButtons("single_file_type", "File contains:",
                choices = c(
                    "Multiple columns (each column = one gene list)" = "columns",
                    "Multiple sheets (each sheet = one gene list)" = "sheets"
                ),
                selected = "columns"
            ),
            conditionalPanel(
                condition = "input.single_file_type == 'sheets'",
                uiOutput("sheet_select_ui")
            ),
            conditionalPanel(
                condition = "input.single_file_type == 'columns'",
                uiOutput("column_select_ui")
            ),
            conditionalPanel(
                condition = "input.single_file_type == 'columns'",
                uiOutput("single_column_names_ui")
            ),
            conditionalPanel(
                condition = "input.single_file_type == 'sheets'",
                uiOutput("single_sheet_names_ui")
            )
        )
    })

    # ---- Single file column/sheet selection ----
    output$sheet_select_ui <- renderUI({
        req(input$single_file)

        ext <- tolower(file_ext(input$single_file$name))
        if (ext %in% c("xlsx", "xls")) {
            sheets <- tryCatch(
                {
                    excel_sheets(input$single_file$datapath)
                },
                error = function(e) {
                    showNotification(paste("Error reading Excel file:", e$message), type = "warning", duration = 5)
                    NULL
                }
            )

            if (!is.null(sheets) && length(sheets) > 0) {
                checkboxGroupInput("selected_sheets",
                    "Select sheets to include:",
                    choices = sheets,
                    selected = sheets[1:min(input$num_datasets, length(sheets))]
                )
            } else {
                helpText("No sheets found in Excel file or unable to read file.")
            }
        } else {
            helpText("Sheet selection is only available for Excel files (.xlsx, .xls)")
        }
    })

    output$column_select_ui <- renderUI({
        req(input$single_file)

        header_flag <- input$single_has_header %||% TRUE
        sep_choice <- input$single_sep %||% "auto"

        df <- tryCatch(
            {
                ext <- tolower(file_ext(input$single_file$name))
                if (ext %in% c("xlsx", "xls")) {
                    read_uploaded_file(input$single_file, NULL, NULL, header_flag)
                } else {
                    read_uploaded_file(input$single_file, sep_choice, NULL, header_flag)
                }
            },
            error = function(e) {
                showNotification(paste("Error reading file:", e$message), type = "warning", duration = 5)
                NULL
            }
        )

        if (!is.null(df)) {
            cols <- colnames(df)
            if (length(cols) > 0) {
                checkboxGroupInput("selected_columns",
                    "Select columns to include:",
                    choices = cols,
                    selected = cols[1:min(input$num_datasets, length(cols))]
                )
            } else {
                helpText("No columns found in file")
            }
        } else {
            helpText("Unable to read file. Please check file format and settings.")
        }
    })

    output$single_sep_ui <- renderUI({
        req(input$single_file)
        ext <- tolower(file_ext(input$single_file$name))
        if (!(ext %in% c("xlsx", "xls"))) {
            tagList(
                selectInput("single_sep", "Column separator:",
                    choices = c("Auto" = "auto", "Comma (,)" = ",", "Tab (\\t)" = "\t", "Semicolon (;)" = ";"),
                    selected = "auto"
                ),
                helpText("The separator used to delimit columns in the text file.")
            )
        } else {
            helpText("Excel file detected - no separator needed.")
        }
    })

    output$single_column_names_ui <- renderUI({
        req(input$single_file_type == "columns", input$selected_columns)
        cols <- input$selected_columns
        if (length(cols) == 0) {
            return(NULL)
        }
        tagList(
            h5("Rename Column Lists"),
            helpText("Optionally assign custom dataset names for each selected column."),
            lapply(cols, function(col) {
                textInput(paste0("single_col_name_", make.names(col)),
                    label = paste("Name for column:", col),
                    value = col
                )
            })
        )
    })

    output$single_sheet_names_ui <- renderUI({
        req(input$single_file_type == "sheets", input$selected_sheets)
        sheets <- input$selected_sheets
        if (length(sheets) == 0) {
            return(NULL)
        }
        tagList(
            h5("Rename Sheet Lists"),
            helpText("Optionally assign custom dataset names for each selected sheet."),
            lapply(sheets, function(sh) {
                textInput(paste0("single_sheet_name_", make.names(sh)),
                    label = paste("Name for sheet:", sh),
                    value = sh
                )
            })
        )
    })

    # ---- Dynamic paste genes UI ----
    output$paste_genes_ui <- renderUI({
        n <- num_datasets_debounced()
        if (is.na(n)) return(NULL)

        tagList(
            lapply(1:n, function(i) {
                wellPanel(
                    textInput(paste0("dataset_name", i),
                        label = paste("Dataset", i, "name:"),
                        value = paste("Dataset", i)
                    ),
                    textAreaInput(paste0("genes", i),
                        label = paste("Gene IDs/Names (one per line):"),
                        value = "", height = "150px",
                        placeholder = "GENE1\nGENE2\nGENE3\n..."
                    ),
                    helpText("Genes entered:", textOutput(paste0("gene_count", i), inline = TRUE))
                )
            })
        )
    })

    # Gene count displays
    max_datasets <- 8
    for (ii in 1:max_datasets) {
        local({
            i <- ii
            output[[paste0("gene_count", i)]] <- renderText({
                genes_text <- input[[paste0("genes", i)]]
                if (is.null(genes_text) || genes_text == "") {
                    return("0")
                }
                genes <- strsplit(genes_text, "\n")[[1]]
                genes <- trimws(genes)
                genes <- genes[genes != ""]
                as.character(length(genes))
            })
        })
    }

    observeEvent(input$clear_paste, {
        req(input$num_datasets)
        n <- input$num_datasets
        for (i in 1:n) {
            updateTextAreaInput(session, paste0("genes", i), value = "")
            updateTextInput(session, paste0("dataset_name", i), value = paste("Dataset", i))
        }
        showNotification("Pasted data has been cleared.", type = "message", duration = 3)
    })

    # ---- Auto-detect columns ----
    observeEvent(
        {
            file_triggers <- lapply(1:8, function(i) input[[paste0("file", i)]])
            sheet_triggers <- lapply(1:8, function(i) input[[paste0("sheet", i)]])
            list(file_triggers, sheet_triggers, input$input_method, num_datasets_debounced())
        },
        {
            n <- num_datasets_debounced()
            if (is.null(input$input_method) || input$input_method != "file") {
                return(NULL)
            }
            if (is.na(n)) {
                return(NULL)
            }

            gene_lists(NULL)
            gene_names_data(NULL)
            all_overlaps(NULL)
            exact_regions(NULL)
            de_data(NULL)
            updateSelectInput(session, "overlap_select", choices = character(0))

            first_index <- NULL

            for (i in 1:n) {
                if (!is.null(input[[paste0("file", i)]])) {
                    first_index <- i
                    break
                }
            }

            if (is.null(first_index)) {
                updateSelectizeInput(session, "gene_col", choices = character(0), selected = character(0))
                updateSelectizeInput(session, "gene_name_col", choices = character(0), selected = character(0))
                updateSelectizeInput(session, "padj_col", choices = character(0), selected = character(0))
                updateSelectizeInput(session, "lfc_col", choices = character(0), selected = character(0))
                return(NULL)
            }

            file <- input[[paste0("file", first_index)]]
            sep <- input[[paste0("sep", first_index)]]
            sheet_input <- input[[paste0("sheet", first_index)]]

            df <- tryCatch(
                {
                    read_uploaded_file(file, sep, sheet_input)
                },
                error = function(e) {
                    showNotification(paste("Error reading file:", e$message), type = "error", duration = 6)
                    NULL
                }
            )

            if (is.null(df)) {
                return(NULL)
            }

            col_names <- colnames(df)
            defaults <- get_column_defaults()

            gene_id_match <- find_best_match(col_names, defaults$gene_id)
            gene_name_match <- find_best_match(col_names, defaults$gene_name)
            padj_match <- find_best_match(col_names, defaults$padj)
            lfc_match <- find_best_match(col_names, defaults$lfc)

            updateSelectizeInput(session, "gene_col",
                choices = col_names,
                selected = if (!is.null(gene_id_match)) gene_id_match else col_names[1], server = TRUE
            )
            updateSelectizeInput(session, "gene_name_col",
                choices = c("", col_names),
                selected = if (!is.null(gene_name_match)) gene_name_match else "", server = TRUE
            )
            updateSelectizeInput(session, "padj_col",
                choices = col_names,
                selected = if (!is.null(padj_match)) padj_match else col_names[1], server = TRUE
            )
            updateSelectizeInput(session, "lfc_col",
                choices = col_names,
                selected = if (!is.null(lfc_match)) lfc_match else col_names[1], server = TRUE
            )

            showNotification("Files changed. Data reset and columns detected.", type = "message", duration = 3)
        }
    )

    # ---- Sheet UI for individual files ----
    max_files <- 8
    for (ii in 1:max_files) {
        local({
            i <- ii
            output[[paste0("sheet_ui", i)]] <- renderUI({
                file <- input[[paste0("file", i)]]
                if (is.null(file)) {
                    return(NULL)
                }

                ext <- tolower(file_ext(file$name))
                if (ext %in% c("xlsx", "xls")) {
                    sheets <- tryCatch(
                        {
                            excel_sheets(file$datapath)
                        },
                        error = function(e) NULL
                    )

                    if (!is.null(sheets) && length(sheets) > 0) {
                        selectInput(paste0("sheet", i), "Excel sheet:",
                            choices = sheets, selected = sheets[1]
                        )
                    }
                }
            })
        })
    }

    # ---- Generate analysis (continued in next section) ----
    source("modules/server_data_input_generate.R", local = TRUE, encoding = "UTF-8")
}
