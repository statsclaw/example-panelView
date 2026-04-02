#!/usr/bin/env Rscript
## ============================================================
## panelView type = "network" — showcase examples
## Bipartite graph visualization inspired by Correia (2016)
## ============================================================

devtools::load_all(".")
library(igraph)
library(ggplot2)

outdir <- "examples/figures"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

## Uniform figure dimensions for README gallery
W <- 1200
H <- 900
RES <- 150

cat("=== panelView network visualization showcase ===\n\n")

## ------------------------------------------------------------
## Example 1: Correia (2016) Figure 2 — CEO-Firm connections
## Synthetic dataset matching the paper's network structure:
##   ~25 CEOs, ~25 Firms, 3-4 connected components,
##   many singletons (degree-1 leaf nodes), organic layout
## ------------------------------------------------------------
cat("1. Correia (2016) Figure 2: CEO-Firm connections\n")

set.seed(2016)

## Build a synthetic CEO-Firm matched dataset that produces
## a bipartite graph visually similar to Figure 2
##
## Component 1 (largest, ~20 nodes): a dense core of CEOs who
## move between overlapping firms, plus peripheral singletons
## Component 2 (~12 nodes): a mid-size cluster
## Component 3 (~8 nodes): a smaller cluster
## Several isolated CEO-Firm pairs (singletons)

edges <- rbind(
    ## --- Component 1: Large connected core ---
    ## Dense hub: CEOs 1-6 share Firms A-H
    data.frame(CEO = 1,  Firm = c("A", "B")),
    data.frame(CEO = 2,  Firm = c("B", "C", "D")),
    data.frame(CEO = 3,  Firm = c("A", "C")),
    data.frame(CEO = 4,  Firm = c("D", "E", "F")),
    data.frame(CEO = 5,  Firm = c("E", "G")),
    data.frame(CEO = 6,  Firm = c("F", "G", "H")),
    ## Peripheral branches off the core
    data.frame(CEO = 7,  Firm = c("H", "I")),
    data.frame(CEO = 8,  Firm = c("I", "J")),
    data.frame(CEO = 9,  Firm = "J"),               # singleton (degree 1)
    data.frame(CEO = 10, Firm = "A"),                # singleton (degree 1)
    data.frame(CEO = 11, Firm = c("B", "K")),
    data.frame(CEO = 12, Firm = "K"),                # singleton (degree 1)

    ## --- Component 2: Mid-size cluster ---
    data.frame(CEO = 13, Firm = c("L", "M")),
    data.frame(CEO = 14, Firm = c("M", "N", "O")),
    data.frame(CEO = 15, Firm = c("N", "P")),
    data.frame(CEO = 16, Firm = c("O", "P", "Q")),
    data.frame(CEO = 17, Firm = "Q"),                # singleton (degree 1)
    data.frame(CEO = 18, Firm = "L"),                # singleton (degree 1)

    ## --- Component 3: Smaller cluster ---
    data.frame(CEO = 19, Firm = c("R", "S")),
    data.frame(CEO = 20, Firm = c("S", "T")),
    data.frame(CEO = 21, Firm = c("T", "U")),
    data.frame(CEO = 22, Firm = "U"),                # singleton (degree 1)
    data.frame(CEO = 23, Firm = "R"),                # singleton (degree 1)

    ## --- Isolated pairs (each is its own component) ---
    data.frame(CEO = 24, Firm = "V"),
    data.frame(CEO = 25, Firm = "W")
)

edges$y <- rnorm(nrow(edges))

png(file.path(outdir, "01-correia-ceo-firm.png"), width = W, height = H, res = RES)
res1 <- panelview(data = edges, index = c("CEO", "Firm"),
                  type = "network",
                  main = "Correia (2016) Fig 2: CEO-Firm Connections")
dev.off()

cat(sprintf("   Vertices: %d | Edges: %d | Components: %d | Singletons: %d\n",
            vcount(res1$graph), ecount(res1$graph),
            nrow(res1$components), nrow(res1$singletons)))

## ------------------------------------------------------------
## Example 2: Unbalanced panel — singletons & components
## 4 disconnected components, degree-1 nodes visible
## ------------------------------------------------------------
cat("\n2. Unbalanced panel with singletons and multiple components\n")

set.seed(42)
comp1 <- expand.grid(unit = LETTERS[1:4], time = 1:4,
                     stringsAsFactors = FALSE)
## Remove some obs to create singletons within the component
comp1 <- comp1[!(comp1$unit == "D" & comp1$time %in% c(1, 2, 3)), ]
comp1 <- comp1[!(comp1$unit == "A" & comp1$time == 4), ]

comp2 <- expand.grid(unit = c("E","F","G"), time = 6:8,
                     stringsAsFactors = FALSE)
comp2 <- comp2[!(comp2$unit == "G" & comp2$time %in% c(6, 7)), ]

comp3 <- data.frame(unit = c("H","H","I","I","J"),
                    time = c(10, 11, 10, 11, 10),
                    stringsAsFactors = FALSE)
comp4 <- data.frame(unit = "K", time = 13, stringsAsFactors = FALSE)
comp5 <- data.frame(unit = "L", time = 14, stringsAsFactors = FALSE)

df_unbal <- rbind(comp1, comp2, comp3, comp4, comp5)
df_unbal$Y <- rnorm(nrow(df_unbal))

png(file.path(outdir, "02-unbalanced-singletons.png"), width = W, height = H, res = RES)
res2 <- panelview(data = df_unbal, index = c("unit", "time"),
                  type = "network",
                  main = "Unbalanced Panel: Singletons & Components")
dev.off()

cat(sprintf("   Vertices: %d | Edges: %d | Components: %d | Singletons: %d\n",
            vcount(res2$graph), ecount(res2$graph),
            nrow(res2$components), nrow(res2$singletons)))
if (nrow(res2$singletons) > 0) {
    cat("   Singleton nodes:\n")
    print(res2$singletons)
}

## ------------------------------------------------------------
## Example 3: Sparse panel with many singletons
## Demonstrates Correia's degree-1 pruning
## Dense core + many peripheral leaf nodes
## ------------------------------------------------------------
cat("\n3. Sparse panel with many singletons\n")

set.seed(123)
units <- paste0("U", 1:25)
times <- paste0("T", 1:12)

## Dense core: units 1-6 x times 1-6
core <- expand.grid(unit = units[1:6], time = times[1:6],
                    stringsAsFactors = FALSE)
## Medium cluster: units 7-10 x times 7-9
mid <- expand.grid(unit = units[7:10], time = times[7:9],
                   stringsAsFactors = FALSE)
## Singletons: each remaining unit in exactly one period
singletons_part <- data.frame(
    unit = units[11:25],
    time = sample(times[10:12], 15, replace = TRUE),
    stringsAsFactors = FALSE
)

df_sparse <- rbind(core, mid, singletons_part)
df_sparse$Y <- rnorm(nrow(df_sparse))

png(file.path(outdir, "03-sparse-singletons.png"), width = W, height = H, res = RES)
res3 <- panelview(data = df_sparse, index = c("unit", "time"),
                  type = "network",
                  main = "Sparse Panel: Degree-1 Nodes for Pruning")
dev.off()

cat(sprintf("   Vertices: %d | Edges: %d | Components: %d | Singletons: %d\n",
            vcount(res3$graph), ecount(res3$graph),
            nrow(res3$components), nrow(res3$singletons)))

## ------------------------------------------------------------
## Example 4: k-partite (3-way fixed effects)
## Unit × Time × Region — three distinct node types
## Larger dataset for better visual
## ------------------------------------------------------------
cat("\n4. Three-way fixed effects: Unit × Time × Region\n")

set.seed(99)
df_3way <- expand.grid(
    unit   = LETTERS[1:8],
    time   = 1:5,
    stringsAsFactors = FALSE
)
## Assign regions
df_3way$region <- ifelse(df_3way$unit %in% LETTERS[1:4], "East",
                  ifelse(df_3way$unit %in% LETTERS[5:6], "West", "South"))
## Remove some observations for visual interest
df_3way <- df_3way[!(df_3way$unit %in% c("C","D") & df_3way$time %in% 4:5), ]
df_3way <- df_3way[!(df_3way$unit == "H" & df_3way$time %in% 1:2), ]
df_3way$Y <- rnorm(nrow(df_3way))

png(file.path(outdir, "04-kpartite-3way.png"), width = W, height = H, res = RES)
res4 <- panelview(data = df_3way, index = c("unit", "time"),
                  fe = "region", type = "network",
                  main = "Three-Way FE: Unit x Time x Region\n(k-partite graph)")
dev.off()

cat(sprintf("   Vertices: %d | Edges: %d | Components: %d | FE types: %s\n",
            vcount(res4$graph), ecount(res4$graph),
            nrow(res4$components),
            paste(unique(V(res4$graph)$fe_type), collapse=", ")))

## ------------------------------------------------------------
## Example 5: Balanced panel — complete bipartite graph
## US voter turnout data (47 states × 24 years)
## One big connected component, zero singletons
## ------------------------------------------------------------
cat("\n5. Balanced panel: US voter turnout (47 states x 24 years)\n")

png(file.path(outdir, "05-turnout-balanced.png"), width = W, height = H, res = RES)
res5 <- panelview(turnout ~ policy_edr + policy_mail_in + policy_motor,
                  data = turnout, index = c("abb", "year"),
                  type = "network",
                  main = "US Voter Turnout: Balanced Panel\n(47 states x 24 years)")
dev.off()

cat(sprintf("   Vertices: %d | Edges: %d | Components: %d | Singletons: %d\n",
            vcount(res5$graph), ecount(res5$graph),
            nrow(res5$components), nrow(res5$singletons)))

## ------------------------------------------------------------
## Example 6: Large real-world panel
## State capacity data (~178 countries × ~45 years)
## ------------------------------------------------------------
cat("\n6. State capacity: large real-world panel\n")

png(file.path(outdir, "06-capacity-large.png"), width = W, height = H, res = RES)
res6 <- panelview(Capacity ~ demo + lnpop + lngdp,
                  data = capacity, index = c("country", "year"),
                  type = "network",
                  main = "State Capacity: Country-Year Network")
dev.off()

cat(sprintf("   Vertices: %d | Edges: %d | Components: %d | Singletons: %d\n",
            vcount(res6$graph), ecount(res6$graph),
            nrow(res6$components), nrow(res6$singletons)))

cat("\n=== All figures saved to examples/figures/ ===\n")
