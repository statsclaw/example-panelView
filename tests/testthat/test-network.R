library(panelView)
data(panelView)

## Helper: run panelview() with type="network", suppressing the plot device
pv_net <- function(...) {
    pdf(NULL)
    on.exit(dev.off())
    panelview(..., type = "network")
}

# -----------------------------------------------------------------------
# 2.1 Basic bipartite graph from balanced panel
# -----------------------------------------------------------------------

test_that("balanced panel: basic bipartite graph", {
    skip_if_not_installed("igraph")
    set.seed(42)
    df_balanced <- data.frame(
        unit = rep(1:4, each = 3),
        time = rep(1:3, 4),
        y = rnorm(12)
    )
    result <- pv_net(data = df_balanced, index = c("unit", "time"))

    expect_true(is.list(result))
    expect_named(result, c("graph", "singletons", "components"))
    expect_true(igraph::is_igraph(result$graph))

    # 4 units + 3 time periods = 7 vertices
    expect_equal(igraph::vcount(result$graph), 7)
    # Complete bipartite K_{4,3} = 12 edges
    expect_equal(igraph::ecount(result$graph), 12)

    # No singletons in balanced panel
    expect_true(is.data.frame(result$singletons))
    expect_equal(nrow(result$singletons), 0)

    # Single connected component
    expect_true(is.data.frame(result$components))
    expect_equal(nrow(result$components), 1)
    expect_equal(result$components$size, 7)

    # All vertices have degree >= 2
    degs <- igraph::degree(result$graph)
    expect_true(all(degs >= 2))
})

# -----------------------------------------------------------------------
# 2.2 Unbalanced panel with singletons
# -----------------------------------------------------------------------

test_that("unbalanced panel: singletons detected correctly", {
    skip_if_not_installed("igraph")
    df_unbal <- data.frame(
        unit = c(1, 1, 1, 2, 2, 3, 4, 4, 5),
        time = c(1, 2, 3, 1, 2, 3, 4, 5, 6),
        y = rnorm(9)
    )
    result <- pv_net(data = df_unbal, index = c("unit", "time"))

    # 5 units + 6 periods = 11 vertices, 9 edges
    expect_equal(igraph::vcount(result$graph), 11)
    expect_equal(igraph::ecount(result$graph), 9)

    # 5 singletons: unit 3, unit 5, period 4, period 5, period 6
    expect_equal(nrow(result$singletons), 5)
    expect_true(all(result$singletons$degree == 1))

    # 3 connected components
    expect_equal(nrow(result$components), 3)
    # Component sizes: 6, 3, 2 (in some order)
    expect_equal(sort(result$components$size, decreasing = TRUE), c(6, 3, 2))
})

# -----------------------------------------------------------------------
# 2.3 No formula required
# -----------------------------------------------------------------------

test_that("network type does not require formula", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = rep(1:3, each = 2), time = rep(1:2, 3), y = 1:6)

    # Without formula
    expect_no_error(result1 <- pv_net(data = df, index = c("unit", "time")))
    expect_true(is.list(result1))

    # With formula (should work too)
    expect_no_error(result2 <- pv_net(y ~ 1, data = df, index = c("unit", "time")))
    expect_true(is.list(result2))
})

# -----------------------------------------------------------------------
# 2.4 Return value structure
# -----------------------------------------------------------------------

test_that("return value has correct structure", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = rep(1:3, each = 2), time = rep(1:2, 3), y = 1:6)
    result <- pv_net(data = df, index = c("unit", "time"))

    expect_true(is.list(result))
    expect_named(result, c("graph", "singletons", "components"))

    # graph
    expect_true(igraph::is_igraph(result$graph))

    # singletons
    expect_true(is.data.frame(result$singletons))
    expect_true(all(c("node", "fe_type", "degree") %in% names(result$singletons)))

    # components
    expect_true(is.data.frame(result$components))
    expect_true(all(c("component", "size") %in% names(result$components)))
})

# -----------------------------------------------------------------------
# 2.5 Large unbalanced panel (package data)
# -----------------------------------------------------------------------

test_that("turnout data: network type runs without error", {
    skip_if_not_installed("igraph")
    result <- pv_net(data = turnout, index = c("abb", "year"))

    expect_true(is.list(result))
    expect_named(result, c("graph", "singletons", "components"))
    expect_true(igraph::is_igraph(result$graph))

    # Vertex count = unique states + unique years
    n_states <- length(unique(turnout$abb))
    n_years <- length(unique(turnout$year))
    expect_equal(igraph::vcount(result$graph), n_states + n_years)

    # At least 1 component
    expect_true(nrow(result$components) >= 1)
})

# -----------------------------------------------------------------------
# 2.6 Vertex attributes are correct
# -----------------------------------------------------------------------

test_that("vertex attributes are set correctly", {
    skip_if_not_installed("igraph")
    set.seed(42)
    df_balanced <- data.frame(
        unit = rep(1:4, each = 3),
        time = rep(1:3, 4),
        y = rnorm(12)
    )
    result <- pv_net(data = df_balanced, index = c("unit", "time"))

    vnames <- igraph::V(result$graph)$name
    expect_length(vnames, 7)

    fe_types <- igraph::V(result$graph)$fe_type
    expect_equal(sort(unique(fe_types)), sort(c("unit", "time")))

    # All in same component for balanced panel
    comp_attr <- igraph::V(result$graph)$component
    expect_equal(length(unique(comp_attr)), 1)
})

# -----------------------------------------------------------------------
# 3.1 Edge case: single unit
# -----------------------------------------------------------------------

test_that("edge case: single unit", {
    skip_if_not_installed("igraph")
    df_one_unit <- data.frame(unit = rep(1, 3), time = 1:3, y = rnorm(3))
    result <- pv_net(data = df_one_unit, index = c("unit", "time"))

    # 1 unit + 3 times = 4 vertices
    expect_equal(igraph::vcount(result$graph), 4)

    # Unit node has degree 3 (not singleton)
    # Each time node has degree 1 (singleton)
    expect_equal(nrow(result$singletons), 3)

    # 1 connected component
    expect_equal(nrow(result$components), 1)
})

# -----------------------------------------------------------------------
# 3.2 Edge case: single time period
# -----------------------------------------------------------------------

test_that("edge case: single time period", {
    skip_if_not_installed("igraph")
    df_one_time <- data.frame(unit = 1:5, time = rep(1, 5), y = rnorm(5))
    result <- pv_net(data = df_one_time, index = c("unit", "time"))

    # 5 units + 1 time = 6 vertices
    expect_equal(igraph::vcount(result$graph), 6)

    # Each unit node has degree 1 (singleton), time node degree 5
    expect_equal(nrow(result$singletons), 5)

    # 1 connected component
    expect_equal(nrow(result$components), 1)
})

# -----------------------------------------------------------------------
# 3.3 Edge case: fully disconnected panel
# -----------------------------------------------------------------------

test_that("edge case: fully disconnected panel", {
    skip_if_not_installed("igraph")
    df_disconn <- data.frame(unit = 1:4, time = 1:4, y = rnorm(4))
    result <- pv_net(data = df_disconn, index = c("unit", "time"))

    # 4 units + 4 times = 8 vertices, 4 edges
    expect_equal(igraph::vcount(result$graph), 8)
    expect_equal(igraph::ecount(result$graph), 4)

    # All 8 nodes are singletons (degree 1)
    expect_equal(nrow(result$singletons), 8)

    # 4 connected components, each of size 2
    expect_equal(nrow(result$components), 4)
    expect_true(all(result$components$size == 2))
})

# -----------------------------------------------------------------------
# 3.4 igraph not available
# -----------------------------------------------------------------------

test_that("graceful error when igraph not available", {
    # Mock requireNamespace to return FALSE for igraph, simulating
    # an environment where igraph is not installed
    local_mocked_bindings(
        requireNamespace = function(package, ...) {
            if (package == "igraph") return(FALSE)
            base::requireNamespace(package, ...)
        },
        .package = "base"
    )
    df <- data.frame(unit = rep(1:3, each = 2), time = rep(1:2, 3), y = 1:6)
    expect_error(
        pv_net(data = df, index = c("unit", "time")),
        "igraph"
    )
})

# -----------------------------------------------------------------------
# 3.5 Invalid fe parameter
# -----------------------------------------------------------------------

test_that("invalid fe parameter raises errors", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = rep(1:3, each = 2), time = rep(1:2, 3), y = 1:6)

    # fe not character
    expect_error(pv_net(data = df, index = c("unit", "time"), fe = 123))

    # fe column not in data
    expect_error(pv_net(data = df, index = c("unit", "time"), fe = "nonexistent"))

    # fe duplicates index column
    expect_error(pv_net(data = df, index = c("unit", "time"), fe = "unit"))
})

# -----------------------------------------------------------------------
# 3.6 Minimal complete bipartite (2x2)
# -----------------------------------------------------------------------

test_that("edge case: 2x2 balanced panel", {
    skip_if_not_installed("igraph")
    df_2x2 <- data.frame(unit = c(1,1,2,2), time = c(1,2,1,2), y = 1:4)
    result <- pv_net(data = df_2x2, index = c("unit", "time"))

    # K_{2,2}: 4 vertices, 4 edges
    expect_equal(igraph::vcount(result$graph), 4)
    expect_equal(igraph::ecount(result$graph), 4)

    # No singletons
    expect_equal(nrow(result$singletons), 0)

    # 1 component of size 4
    expect_equal(nrow(result$components), 1)
    expect_equal(result$components$size, 4)
})

# -----------------------------------------------------------------------
# 4.1 Three-way FE (k-partite)
# -----------------------------------------------------------------------

test_that("k-partite: three-way FE (unit x time x region)", {
    skip_if_not_installed("igraph")
    df_3way <- data.frame(
        unit = c(1, 1, 2, 2, 3, 3),
        time = c(1, 2, 1, 2, 3, 4),
        region = c("A", "A", "A", "B", "B", "B"),
        y = rnorm(6)
    )
    result <- pv_net(y ~ 1, data = df_3way,
                     index = c("unit", "time"), fe = "region")

    expect_true(is.list(result))
    expect_named(result, c("graph", "singletons", "components"))
    expect_true(igraph::is_igraph(result$graph))

    # fe_type should have 3 distinct values
    fe_types <- unique(igraph::V(result$graph)$fe_type)
    expect_equal(length(fe_types), 3)
    expect_true(all(c("unit", "time", "region") %in% fe_types))

    # 3 units + 4 times + 2 regions = 9 nodes
    expect_equal(igraph::vcount(result$graph), 9)
})

# -----------------------------------------------------------------------
# 4.2 k-partite with fe = character vector of length 2
# -----------------------------------------------------------------------

test_that("k-partite: four-way FE", {
    skip_if_not_installed("igraph")
    df_4way <- data.frame(
        unit = c(1, 1, 2, 2),
        time = c(1, 2, 1, 2),
        region = c("A", "A", "B", "B"),
        sector = c("X", "Y", "X", "Y"),
        y = rnorm(4)
    )
    result <- pv_net(y ~ 1, data = df_4way,
                     index = c("unit", "time"), fe = c("region", "sector"))

    expect_true(igraph::is_igraph(result$graph))

    fe_types <- unique(igraph::V(result$graph)$fe_type)
    expect_equal(length(fe_types), 4)
    expect_named(result, c("graph", "singletons", "components"))
})

# -----------------------------------------------------------------------
# 5. Property-based invariants
# -----------------------------------------------------------------------

test_that("invariant: vertex count = unique units + unique periods (bipartite)", {
    skip_if_not_installed("igraph")
    # Test on multiple datasets
    datasets <- list(
        data.frame(unit = rep(1:4, each = 3), time = rep(1:3, 4), y = rnorm(12)),
        data.frame(unit = c(1,1,2,3), time = c(1,2,1,3), y = rnorm(4)),
        data.frame(unit = 1:10, time = rep(1:2, 5), y = rnorm(10))
    )
    for (df in datasets) {
        result <- pv_net(data = df, index = c("unit", "time"))
        n_units <- length(unique(df$unit))
        n_times <- length(unique(df$time))
        expect_equal(igraph::vcount(result$graph), n_units + n_times)
    }
})

test_that("invariant: edge count = number of observations (bipartite)", {
    skip_if_not_installed("igraph")
    datasets <- list(
        data.frame(unit = rep(1:4, each = 3), time = rep(1:3, 4), y = rnorm(12)),
        data.frame(unit = c(1,1,2,3), time = c(1,2,1,3), y = rnorm(4)),
        data.frame(unit = 1:4, time = 1:4, y = rnorm(4))
    )
    for (df in datasets) {
        result <- pv_net(data = df, index = c("unit", "time"))
        expect_equal(igraph::ecount(result$graph), nrow(df))
    }
})

test_that("invariant: bipartite property holds for 2-way FE", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = rep(1:4, each = 3), time = rep(1:3, 4), y = rnorm(12))
    result <- pv_net(data = df, index = c("unit", "time"))
    expect_true(igraph::is_bipartite(result$graph))
})

test_that("invariant: singleton degree is always 1", {
    skip_if_not_installed("igraph")
    df_unbal <- data.frame(
        unit = c(1, 1, 1, 2, 2, 3, 4, 4, 5),
        time = c(1, 2, 3, 1, 2, 3, 4, 5, 6),
        y = rnorm(9)
    )
    result <- pv_net(data = df_unbal, index = c("unit", "time"))
    if (nrow(result$singletons) > 0) {
        # Verify all reported singletons actually have degree 1
        # Use the degree column from the singletons data.frame itself
        expect_true(all(result$singletons$degree == 1))
        # Also verify against the graph: all degree-1 vertices are captured
        all_degs <- igraph::degree(result$graph)
        n_deg1_in_graph <- sum(all_degs == 1)
        expect_equal(nrow(result$singletons), n_deg1_in_graph)
    }
})

test_that("invariant: component membership consistency", {
    skip_if_not_installed("igraph")
    df_unbal <- data.frame(
        unit = c(1, 1, 1, 2, 2, 3, 4, 4, 5),
        time = c(1, 2, 3, 1, 2, 3, 4, 5, 6),
        y = rnorm(9)
    )
    result <- pv_net(data = df_unbal, index = c("unit", "time"))
    comp_ids <- unique(igraph::V(result$graph)$component)
    expect_equal(length(comp_ids), nrow(result$components))
})

test_that("invariant: component sizes sum to vertex count", {
    skip_if_not_installed("igraph")
    df_unbal <- data.frame(
        unit = c(1, 1, 1, 2, 2, 3, 4, 4, 5),
        time = c(1, 2, 3, 1, 2, 3, 4, 5, 6),
        y = rnorm(9)
    )
    result <- pv_net(data = df_unbal, index = c("unit", "time"))
    expect_equal(sum(result$components$size), igraph::vcount(result$graph))
})

test_that("invariant: balanced panel has no singletons", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = rep(1:5, each = 4), time = rep(1:4, 5), y = rnorm(20))
    result <- pv_net(data = df, index = c("unit", "time"))
    expect_equal(nrow(result$singletons), 0)
})

test_that("invariant: balanced panel has one component", {
    skip_if_not_installed("igraph")
    df <- data.frame(unit = rep(1:5, each = 4), time = rep(1:4, 5), y = rnorm(20))
    result <- pv_net(data = df, index = c("unit", "time"))
    expect_equal(nrow(result$components), 1)
})
