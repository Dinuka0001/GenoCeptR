# ==================================================================
# File Utilities Module
# Functions for reading and processing uploaded files
# ==================================================================

#' Read uploaded file with automatic format detection
#'
#' @param file_input File input from Shiny fileInput
#' @param sep_input Separator for text files
#' @param sheet_input Sheet name for Excel files
#' @param header_flag Whether file has header row
#' @return Data frame with file contents
read_uploaded_file <- function(file_input, sep_input, sheet_input = NULL, header_flag = TRUE) {
    req(file_input)

    tryCatch(
        {
            ext <- tolower(file_ext(file_input$name))

            if (ext %in% c("xlsx", "xls")) {
                sheet_to_read <- if (is.null(sheet_input) || sheet_input == "") 1 else sheet_input
                df <- read_excel(
                    path = file_input$datapath, sheet = sheet_to_read,
                    .name_repair = "minimal"
                )
                df <- as.data.frame(df, stringsAsFactors = FALSE)
            } else {
                sep <- sep_input
                if (is.null(sep) || sep == "auto") {
                    first_line <- readLines(file_input$datapath, n = 1)
                    if (grepl("\t", first_line)) {
                        sep <- "\t"
                    } else if (grepl(";", first_line)) {
                        sep <- ";"
                    } else {
                        sep <- ","
                    }
                }

                df <- read.table(
                    file = file_input$datapath, sep = sep, header = header_flag,
                    stringsAsFactors = FALSE, check.names = FALSE,
                    quote = "\"", comment.char = "", fill = TRUE,
                    na.strings = c("", "NA", "N/A", "null", "NULL")
                )
            }

            colnames(df) <- trimws(colnames(df))
            df <- df[rowSums(is.na(df)) != ncol(df), ]
            df <- df[, colSums(is.na(df)) != nrow(df), drop = FALSE]

            return(df)
        },
        error = function(e) {
            stop(e$message)
        }
    )
}

#' Auto-detect column names for DE analysis
#'
#' @param col_names Vector of column names from data frame
#' @param defaults Vector of potential column name patterns
#' @return Best matching column name or NULL
find_best_match <- function(cols, defaults) {
    for (default in defaults) {
        matches <- grep(paste0("^", default, "$"), cols, ignore.case = TRUE, value = TRUE)
        if (length(matches) > 0) {
            return(matches[1])
        }
    }
    for (default in defaults) {
        matches <- grep(default, cols, ignore.case = TRUE, value = TRUE)
        if (length(matches) > 0) {
            return(matches[1])
        }
    }
    return(NULL)
}

#' Get default column mappings for DE analysis
#'
#' @return List with default column name patterns
get_column_defaults <- function() {
    list(
        gene_id = c("gene_id", "gene", "Gene_ID", "GeneID", "ID", "ensembl_gene_id"),
        gene_name = c("gene_name", "gene_symbol", "GeneName", "Symbol", "Name"),
        padj = c("padj", "adj.P.Val", "FDR", "p.adjust", "pvalue_adj"),
        lfc = c("log2FoldChange", "logFC", "log2FC", "LFC", "FC")
    )
}
