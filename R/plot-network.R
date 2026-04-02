## Network visualization of panel data structure
## Bipartite/k-partite graph of fixed effects
## Inspired by Correia (2016) Figure 2

.pv_plot_network <- function(s) {

    ## Step 1: Check igraph availability
    if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("Package 'igraph' is required for type = 'network'. ",
             "Install it with: install.packages('igraph')",
             call. = FALSE)
    }

    ## Step 2: Extract data from environment list
    I       <- s$I
    N       <- s$N
    TT      <- s$TT
    input.id <- s$input.id
    raw.time <- s$raw.time
    index   <- s$index
    fe      <- s$fe
    data    <- s$data
    main    <- s$main
    xlab    <- s$xlab
    ylab    <- s$ylab
    cex.main   <- s$cex.main
    cex.axis   <- s$cex.axis
    cex.lab    <- s$cex.lab
    cex.legend <- s$cex.legend
    legendOff  <- s$legendOff
    theme.bw   <- s$theme.bw
    color      <- s$color

    ## Step 2b: Input validation
    if (N < 1 || TT < 1) {
        stop("Panel must have at least one unit and one time period.",
             call. = FALSE)
    }

    ## Balanced panel fallback: if I is NULL, construct all-ones matrix
    if (is.null(I)) {
        I <- matrix(1, TT, N)
    }

    if (sum(I) == 0) {
        stop("No observations in the panel.", call. = FALSE)
    }

    ## Validate fe parameter
    if (!is.null(fe)) {
        if (!is.character(fe)) {
            stop("'fe' must be a character vector of column names in the data, distinct from 'index'.",
                 call. = FALSE)
        }
        if (!all(fe %in% colnames(data))) {
            stop("'fe' must be a character vector of column names in the data, distinct from 'index'.",
                 call. = FALSE)
        }
        if (any(fe %in% index)) {
            stop("'fe' must be a character vector of column names in the data, distinct from 'index'.",
                 call. = FALSE)
        }
    }

    ## Determine k (number of FE dimensions)
    k <- 2L + length(fe)

    ## Step 3: Build the graph
    if (k == 2L) {
        ## 3a. Bipartite case
        g <- igraph::graph_from_biadjacency_matrix(I, directed = FALSE,
                                                    weighted = TRUE)

        ## Set vertex attributes
        ## First TT vertices are rows (time), next N vertices are columns (unit)
        igraph::V(g)$name <- c(as.character(raw.time),
                               as.character(input.id))
        igraph::V(g)$fe_type <- c(rep(index[2], TT),
                                  rep(index[1], N))
        igraph::V(g)$fe_index <- c(seq_len(TT), seq_len(N))

    } else {
        ## 3b. k-partite case (k > 2)
        fe_dims <- c(index[1], index[2], fe)

        ## Collect unique categories per dimension and build vertex table
        vert_list <- list()
        for (d in fe_dims) {
            cats <- sort(unique(as.character(data[, d])))
            vert_list[[d]] <- data.frame(
                name    = paste0(d, ":", cats),
                fe_type = d,
                cat     = cats,
                stringsAsFactors = FALSE
            )
        }
        vert_df <- do.call(rbind, vert_list)
        rownames(vert_df) <- NULL

        ## Build edge list from all observation rows
        edge_rows <- list()
        for (r in seq_len(nrow(data))) {
            ## Get node names for this observation across all FE dimensions
            nodes <- vapply(fe_dims, function(d) {
                paste0(d, ":", as.character(data[r, d]))
            }, character(1))
            ## All pairs (d1, d2) where d1 < d2
            for (i in seq_len(length(nodes) - 1)) {
                for (j in (i + 1):length(nodes)) {
                    edge_rows[[length(edge_rows) + 1]] <- c(nodes[i], nodes[j])
                }
            }
        }
        edge_mat <- do.call(rbind, edge_rows)

        ## Create graph
        g <- igraph::graph_from_edgelist(edge_mat, directed = FALSE)

        ## Set vertex attributes
        v_names <- igraph::V(g)$name
        v_idx <- match(v_names, vert_df$name)
        igraph::V(g)$fe_type <- vert_df$fe_type[v_idx]

        ## Assign type_id (integer 1..k)
        igraph::V(g)$type_id <- match(igraph::V(g)$fe_type, fe_dims)

        ## Collapse parallel edges into weighted edges
        g <- igraph::simplify(g, edge.attr.comb = list(weight = "sum"))
    }

    ## Step 4: Compute degrees and identify singletons
    deg <- igraph::degree(g)
    is_singleton <- (deg == 1)
    singleton_nodes <- which(is_singleton)

    singletons_df <- data.frame(
        node    = igraph::V(g)$name[singleton_nodes],
        fe_type = igraph::V(g)$fe_type[singleton_nodes],
        degree  = deg[singleton_nodes],
        stringsAsFactors = FALSE
    )

    ## Step 5: Compute connected components
    comp <- igraph::components(g)

    comp_info <- data.frame(
        component = seq_len(comp$no),
        size      = comp$csize,
        stringsAsFactors = FALSE
    )

    igraph::V(g)$component <- comp$membership

    ## Step 6: Compute layout — force-directed for all cases
    ## Fruchterman-Reingold produces the organic network look from Correia (2016)
    lay <- igraph::layout_with_fr(g)

    ## Normalize layout to [0, 1] x [0, 1] — reduces whitespace
    xr <- range(lay[, 1])
    yr <- range(lay[, 2])
    if (xr[2] > xr[1]) lay[, 1] <- (lay[, 1] - xr[1]) / (xr[2] - xr[1])
    if (yr[2] > yr[1]) lay[, 2] <- (lay[, 2] - yr[1]) / (yr[2] - yr[1])

    ## Step 7: Build ggplot2 visualization — Correia (2016) style

    ## Adaptive node sizing based on graph size
    nv <- igraph::vcount(g)
    if (nv <= 15) {
        sz_regular  <- 5
        sz_single   <- 3.5
        edge_lw     <- 0.6
        node_stroke <- 0.8
    } else if (nv <= 50) {
        sz_regular  <- 4
        sz_single   <- 2.8
        edge_lw     <- 0.5
        node_stroke <- 0.7
    } else if (nv <= 150) {
        sz_regular  <- 3
        sz_single   <- 2
        edge_lw     <- 0.35
        node_stroke <- 0.5
    } else {
        sz_regular  <- 2
        sz_single   <- 1.4
        edge_lw     <- 0.2
        node_stroke <- 0.3
    }

    ## 7a. Node data frame
    node_df <- data.frame(
        x            = lay[, 1],
        y            = lay[, 2],
        name         = igraph::V(g)$name,
        fe_type      = igraph::V(g)$fe_type,
        is_singleton = is_singleton,
        component    = comp$membership,
        stringsAsFactors = FALSE
    )

    node_df$node_size <- ifelse(node_df$is_singleton, sz_single, sz_regular)

    ## 7b. Edge data frame
    el <- igraph::as_edgelist(g, names = FALSE)
    edge_df <- data.frame(
        x    = lay[el[, 1], 1],
        y    = lay[el[, 1], 2],
        xend = lay[el[, 2], 1],
        yend = lay[el[, 2], 2]
    )

    ## 7c. Convex hull data frame (components with 3+ nodes)
    ## Expand hull outward from centroid so it wraps around nodes, not through them
    hull_pad <- 0.03  # padding in normalized [0,1] coordinates
    hull_list <- list()
    for (cid in seq_len(comp$no)) {
        idx <- which(comp$membership == cid)
        if (length(idx) >= 3) {
            pts <- lay[idx, , drop = FALSE]
            h <- grDevices::chull(pts)
            hx <- pts[h, 1]
            hy <- pts[h, 2]
            ## Push each hull vertex outward from centroid
            cx <- mean(hx)
            cy <- mean(hy)
            dx <- hx - cx
            dy <- hy - cy
            d  <- sqrt(dx^2 + dy^2)
            d[d == 0] <- 1
            hx <- hx + hull_pad * dx / d
            hy <- hy + hull_pad * dy / d
            hull_list[[length(hull_list) + 1]] <- data.frame(
                x = hx, y = hy, component = cid
            )
        }
    }
    if (length(hull_list) > 0) {
        hull_df <- do.call(rbind, hull_list)
    } else {
        hull_df <- NULL
    }

    ## 7d. Assemble ggplot — Correia (2016) Figure 2 style

    ## Determine FE types for shape/fill scales
    fe_types <- unique(node_df$fe_type)

    ## Shape palette: filled circle (21), hollow square (22), triangle (24), diamond (23)
    shape_pool <- c(21, 22, 24, 23, 25, 21, 22, 24)
    shape_vals <- shape_pool[seq_along(fe_types)]
    names(shape_vals) <- fe_types

    ## Fill palette — Correia style: black filled circles, white hollow squares
    if (!is.null(color) && length(color) >= length(fe_types)) {
        fill_vals <- color[seq_along(fe_types)]
    } else {
        default_fills <- c("black", "white", "grey50", "grey80",
                           "black", "white", "grey50", "grey80")
        fill_vals <- default_fills[seq_along(fe_types)]
    }
    names(fill_vals) <- fe_types

    ## Default title
    if (is.null(main)) {
        main <- "Network Structure"
    }

    ## Base plot — clean white background
    p <- ggplot2::ggplot() + ggplot2::theme_void()

    ## Filled light blue convex hulls
    if (!is.null(hull_df)) {
        p <- p + ggplot2::geom_polygon(
            data = hull_df,
            ggplot2::aes(x = .data$x, y = .data$y,
                         group = .data$component),
            fill = "#B0D8E8", alpha = 0.35, color = NA
        )
    }

    ## Edges
    p <- p + ggplot2::geom_segment(
        data = edge_df,
        ggplot2::aes(x = .data$x, y = .data$y,
                     xend = .data$xend, yend = .data$yend),
        color = "grey50", linewidth = edge_lw
    )

    ## Nodes — all nodes, uniform style per FE type
    p <- p + ggplot2::geom_point(
        data = node_df,
        ggplot2::aes(x = .data$x, y = .data$y,
                     shape = .data$fe_type,
                     fill  = .data$fe_type,
                     size  = .data$node_size),
        color = "black", stroke = node_stroke
    )

    ## Scales
    p <- p +
        ggplot2::scale_shape_manual(values = shape_vals) +
        ggplot2::scale_fill_manual(values = fill_vals) +
        ggplot2::scale_size_identity()

    ## Tight coordinate limits with small padding
    p <- p + ggplot2::coord_cartesian(
        xlim = c(-0.06, 1.06), ylim = c(-0.06, 1.06),
        expand = FALSE
    )

    ## Title
    p <- p + ggplot2::ggtitle(main)

    ## Theme — tight margins, no axes, crisp background
    p <- p + ggplot2::theme(
        plot.title      = ggplot2::element_text(size = cex.main, hjust = 0.5,
                                                 face = "bold"),
        plot.margin     = ggplot2::margin(5, 5, 5, 5),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )

    ## Legend
    if (isTRUE(legendOff)) {
        p <- p + ggplot2::theme(legend.position = "none")
    } else {
        p <- p + ggplot2::guides(
            shape = ggplot2::guide_legend(
                override.aes = list(
                    fill   = fill_vals,
                    shape  = shape_vals,
                    size   = 4
                )
            ),
            fill = "none"
        )
        p <- p + ggplot2::theme(
            legend.position  = "bottom",
            legend.text      = ggplot2::element_text(size = cex.legend),
            legend.title     = ggplot2::element_blank(),
            legend.key       = ggplot2::element_rect(fill = "white", color = NA)
        )
    }

    ## 7e. Print the plot
    print(p)

    ## Step 8: Construct return value
    result <- list(
        graph      = g,
        singletons = singletons_df,
        components = comp_info
    )
    invisible(result)
}
