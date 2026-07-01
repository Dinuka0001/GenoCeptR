# ==================================================================
# Overlap Utilities Module
# Functions for calculating gene set overlaps and intersections
# ==================================================================

#' Calculate all possible overlaps between gene lists
#'
#' @param gene_lists Named list of gene vectors
#' @return Named list with all overlaps and intersections
intersection_symbol <- function() {
    intToUtf8(0x2229)
}

join_intersection_names <- function(values) {
    enc2utf8(paste(enc2utf8(values), collapse = paste0(" ", intersection_symbol(), " ")))
}

paste_intersection_name <- function(...) {
    enc2utf8(paste(enc2utf8(c(...)), collapse = paste0(" ", intersection_symbol(), " ")))
}

calculate_overlaps <- function(gene_lists) {
    n <- length(gene_lists)
    list_names <- names(gene_lists)
    overlaps <- list()

    # Individual sets
    for (i in 1:n) {
        overlaps[[list_names[i]]] <- gene_lists[[i]]
    }

    # 2-way intersections
    if (n >= 2) {
        for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
                name <- paste_intersection_name(list_names[i], list_names[j])
                overlaps[[name]] <- intersect(gene_lists[[i]], gene_lists[[j]])
            }
        }
    }

    # 3-way intersections
    if (n >= 3) {
        for (i in 1:(n - 2)) {
            for (j in (i + 1):(n - 1)) {
                for (k in (j + 1):n) {
                    name <- paste_intersection_name(list_names[i], list_names[j], list_names[k])
                    overlaps[[name]] <- Reduce(intersect, gene_lists[c(i, j, k)])
                }
            }
        }
    }

    # 4-way intersections
    if (n >= 4) {
        for (i in 1:(n - 3)) {
            for (j in (i + 1):(n - 2)) {
                for (k in (j + 1):(n - 1)) {
                    for (l in (k + 1):n) {
                        name <- paste_intersection_name(
                            list_names[i], list_names[j], list_names[k], list_names[l]
                        )
                        overlaps[[name]] <- Reduce(intersect, gene_lists[c(i, j, k, l)])
                    }
                }
            }
        }
    }

    # 5-way intersections
    if (n >= 5) {
        for (i in 1:(n - 4)) {
            for (j in (i + 1):(n - 3)) {
                for (k in (j + 1):(n - 2)) {
                    for (l in (k + 1):(n - 1)) {
                        for (m in (l + 1):n) {
                            name <- paste_intersection_name(
                                list_names[i], list_names[j], list_names[k], list_names[l], list_names[m]
                            )
                            overlaps[[name]] <- Reduce(intersect, gene_lists[c(i, j, k, l, m)])
                        }
                    }
                }
            }
        }
    }

    # 6-way intersections
    if (n >= 6) {
        for (i in 1:(n - 5)) {
            for (j in (i + 1):(n - 4)) {
                for (k in (j + 1):(n - 3)) {
                    for (l in (k + 1):(n - 2)) {
                        for (m in (l + 1):(n - 1)) {
                            for (o in (m + 1):n) {
                                name <- paste_intersection_name(
                                    list_names[i], list_names[j], list_names[k],
                                    list_names[l], list_names[m], list_names[o]
                                )
                                overlaps[[name]] <- Reduce(intersect, gene_lists[c(i, j, k, l, m, o)])
                            }
                        }
                    }
                }
            }
        }
    }

    # 7-way intersections
    if (n >= 7) {
        for (i in 1:(n - 6)) {
            for (j in (i + 1):(n - 5)) {
                for (k in (j + 1):(n - 4)) {
                    for (l in (k + 1):(n - 3)) {
                        for (m in (l + 1):(n - 2)) {
                            for (o in (m + 1):(n - 1)) {
                                for (p in (o + 1):n) {
                                    name <- paste_intersection_name(
                                        list_names[i], list_names[j], list_names[k], list_names[l],
                                        list_names[m], list_names[o], list_names[p]
                                    )
                                    overlaps[[name]] <- Reduce(intersect, gene_lists[c(i, j, k, l, m, o, p)])
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    # 8-way intersections
    if (n == 8) {
        name <- join_intersection_names(list_names)
        overlaps[[name]] <- Reduce(intersect, gene_lists)
    }

    overlaps
}

#' Calculate mutually exclusive Venn membership regions
#'
#' Each gene appears in exactly one region, matching the region counts shown
#' inside Venn-style diagrams.
#'
#' @param gene_lists Named list of gene vectors
#' @return Named list of exact, mutually exclusive regions
calculate_exact_regions <- function(gene_lists) {
    n <- length(gene_lists)
    list_names <- names(gene_lists)
    exact <- list()

    for (region_size in seq_len(n)) {
        combinations <- combn(seq_len(n), region_size, simplify = FALSE)
        for (included_indices in combinations) {
            included_genes <- Reduce(intersect, gene_lists[included_indices])
            excluded_indices <- setdiff(seq_len(n), included_indices)

            if (length(excluded_indices) > 0 && length(included_genes) > 0) {
                excluded_genes <- unique(unlist(gene_lists[excluded_indices], use.names = FALSE))
                included_genes <- setdiff(included_genes, excluded_genes)
            }

            region_name <- if (region_size == 1) {
                paste0(list_names[included_indices], " only")
            } else {
                paste0(join_intersection_names(list_names[included_indices]), " only")
            }
            exact[[region_name]] <- included_genes
        }
    }

    exact
}

#' Calculate exact Venn regions with the same engine used by ggVennDiagram
#'
#' For 2-5 sets, this mirrors the displayed Venn-region values in the
#' ggVennDiagram visualization used by V2.1. For larger set counts, where a
#' traditional Venn is not drawn, it falls back to the generic exact-region
#' calculation.
calculate_venn_display_regions <- function(gene_lists) {
    if (length(gene_lists) < 2 || length(gene_lists) > 5 ||
        !requireNamespace("ggVennDiagram", quietly = TRUE)) {
        return(calculate_exact_regions(gene_lists))
    }

    venn_data <- ggVennDiagram::process_data(
        ggVennDiagram::Venn(gene_lists)
    )
    region_data <- venn_data$regionData
    if (is.null(region_data) || nrow(region_data) == 0) {
        return(calculate_exact_regions(gene_lists))
    }

    regions <- setNames(vector("list", nrow(region_data)), character(nrow(region_data)))
    for (i in seq_len(nrow(region_data))) {
        region_sets <- strsplit(region_data$name[[i]], "/", fixed = TRUE)[[1]]
        region_name <- if (length(region_sets) == 1) {
            paste0(region_sets, " only")
        } else {
            paste0(join_intersection_names(region_sets), " only")
        }
        regions[[i]] <- enc2utf8(unlist(region_data$item[[i]], use.names = FALSE))
        names(regions)[[i]] <- enc2utf8(region_name)
    }

    regions
}

#' Build grouped choices for the Gene Lists overlap selector
overlap_select_choices <- function(inclusive, exact) {
    list(
        "Show all" = c(
            "All samples & inclusive combinations" = "all::inclusive",
            "All exact Venn regions" = "all::exact",
            "All exact + inclusive combinations" = "all::both"
        ),
        "Exact regions (matches Venn)" = setNames(
            enc2utf8(paste0("exact::", names(exact))),
            enc2utf8(names(exact))
        ),
        "Inclusive sets/intersections" = setNames(
            enc2utf8(paste0("inclusive::", names(inclusive))),
            enc2utf8(names(inclusive))
        )
    )
}

#' Resolve one selector value to its genes and definition
resolve_overlap_selection <- function(selection, inclusive, exact) {
    if (startsWith(selection, "exact::")) {
        selection_name <- sub("^exact::", "", selection)
        list(
            name = selection_name,
            definition = "Exact Venn region",
            genes = exact[[selection_name]]
        )
    } else if (startsWith(selection, "inclusive::")) {
        selection_name <- sub("^inclusive::", "", selection)
        list(
            name = selection_name,
            definition = "Inclusive set/intersection",
            genes = inclusive[[selection_name]]
        )
    } else {
        list(
            name = selection,
            definition = "Inclusive set/intersection",
            genes = inclusive[[selection]]
        )
    }
}

#' Convert a named gene-list collection to table rows
combination_rows <- function(definition, combinations) {
    rows <- lapply(names(combinations), function(combination_name) {
        genes <- combinations[[combination_name]]
        if (length(genes) == 0) return(NULL)

        data.frame(
            Definition = rep(definition, length(genes)),
            Combination = rep(enc2utf8(combination_name), length(genes)),
            Gene_ID = enc2utf8(genes),
            stringsAsFactors = FALSE
        )
    })

    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) {
        return(data.frame(
            Definition = character(0),
            Combination = character(0),
            Gene_ID = character(0),
            stringsAsFactors = FALSE
        ))
    }

    do.call(rbind, rows)
}

#' Build gene rows for one selector value, including Show all choices
selected_gene_rows <- function(selection, inclusive, exact) {
    if (selection == "all::inclusive") {
        combination_rows("Inclusive set/intersection", inclusive)
    } else if (selection == "all::exact") {
        combination_rows("Exact Venn region", exact)
    } else if (selection == "all::both") {
        rbind(
            combination_rows("Exact Venn region", exact),
            combination_rows("Inclusive set/intersection", inclusive)
        )
    } else {
        selected_region <- resolve_overlap_selection(selection, inclusive, exact)
        combination_rows(
            selected_region$definition,
            setNames(list(selected_region$genes), selected_region$name)
        )
    }
}
