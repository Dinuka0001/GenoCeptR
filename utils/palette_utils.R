# ==================================================================
# Palette Utilities
# Colorblind-friendly palettes for plots while preserving custom defaults
# ==================================================================

gc_discrete_palettes <- list(
    custom = NULL,
    okabe_ito = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999"),
    tol_bright = c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377", "#BBBBBB", "#000000"),
    tol_muted = c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255"),
    color_universal = c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442", "#999999")
)

gc_gradient_palettes <- list(
    custom = NULL,
    viridis = c("#440154", "#FDE725"),
    cividis = c("#00204D", "#FFE945"),
    blue_orange = c("#0072B2", "#E69F00"),
    purple_green = c("#762A83", "#1B7837"),
    teal_magenta = c("#009E73", "#CC79A7")
)

gc_palette_choices <- function(type = c("discrete", "gradient")) {
    type <- match.arg(type)
    if (type == "discrete") {
        c(
            "Custom current colors" = "custom",
            "Okabe-Ito" = "okabe_ito",
            "Tol Bright" = "tol_bright",
            "Tol Muted" = "tol_muted",
            "Color Universal Design" = "color_universal"
        )
    } else {
        c(
            "Custom current colors" = "custom",
            "Viridis" = "viridis",
            "Cividis" = "cividis",
            "Blue to Orange" = "blue_orange",
            "Purple to Green" = "purple_green",
            "Teal to Magenta" = "teal_magenta"
        )
    }
}

resolve_discrete_palette <- function(choice, n, fallback) {
    choice <- choice %||% "custom"
    fallback <- fallback %||% character(0)
    if (length(fallback) == 0) {
        fallback <- gc_discrete_palettes$okabe_ito
    }
    if (identical(choice, "custom") || !choice %in% names(gc_discrete_palettes)) {
        return(head(c(fallback, rep(fallback, length.out = n)), n))
    }

    palette <- gc_discrete_palettes[[choice]]
    if (length(palette) < n) {
        palette <- grDevices::colorRampPalette(palette)(n)
    }
    head(palette, n)
}

resolve_gradient_palette <- function(choice, low, high) {
    choice <- choice %||% "custom"
    if (identical(choice, "custom") || !choice %in% names(gc_gradient_palettes)) {
        return(c(low = low, high = high))
    }

    palette <- gc_gradient_palettes[[choice]]
    c(low = palette[[1]], high = palette[[2]])
}
