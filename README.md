# categorical

**Lightweight specific MCA + HCPC with geometric data analysis — base R, no dependencies for the statistics.**

`categorical` fits a specific Multiple Correspondence Analysis to a table of categorical
codes, clusters the individuals with HCPC (Ward + k-means consolidation), and returns
the full apparatus of geometric data analysis in one seeded, reproducible object:

- Benzécri-adjusted eigenvalues and a scree.
- Category geometry: principal coordinates, contributions, squared cosines.
- Haberman **adjusted residuals** (v-tests) of categories against clusters *and* against an
  optional supplementary grouping (e.g. time period).
- **Geometric typicality** tests, **correlation-ratio** (η²) with F and permutation p.
- **Bootstrap confidence ellipses** — centroid (vanilla) or top-k *signature*.
- **Content-based cluster labelling** (bijective) so clusters are named by their meaning,
  not by arbitrary number.
- Enriched master tables (category- and row-level).
- Configurable base-graphics maps (black-&-white or colour; several label modes; ellipses).
- Optional cross-check against **FactoMineR** (agrees to machine precision).

The MCA/HCPC mathematics use only base R. `FactoMineR` is needed **only** for the optional
`mca_validate()` cross-check.

## Install

```r
# install.packages("remotes")
remotes::install_github("raymondli-me/categorical")
```

Or drop the single-file build (`R/` concatenated) into any script with `source("categorical.R")`.

## Quick start

Uses `Titanic`, a classic categorical dataset that ships with base R (`datasets`) — no
downloads. We expand the contingency table to individuals, use **Class / Sex / Age** as the
active variables, and treat **Survived** as a supplementary grouping.

```r
library(categorical)

# expand the built-in Titanic table to individual level (2201 people)
tt <- as.data.frame(Titanic)
df <- tt[rep(seq_len(nrow(tt)), tt$Freq), c("Class","Sex","Age")]
df$Survived <- tt$Survived[rep(seq_len(nrow(tt)), tt$Freq)]

fit <- mca_run(df, active = c("Class","Sex","Age"), group = "Survived", k = 3)
fit                         # N, inertia, cluster sizes
mca_report(fit)             # top categories per dimension / cluster / group

# geometry & tests
mca_typicality(fit)
mca_eta(fit)                # does Survived structure the space? (it does: p ~ .003 on D1)
mca_ellipses(fit, dims = c(1,2), topk = NULL)   # centroid ellipses; topk = k for signature

# figures (base graphics, no dependencies)
plot_scree(fit)
plot_map(fit, dims = c(1,2), bw = FALSE, label_mode = "direct",
         label_top = 3, ellipse = "centroid")
```

Dimension 1 contrasts *Sex = Female* and *Age = Child* against *Class = Crew* — the
"women and children vs. crew" axis — and `Survived` loads strongly on it.

## Function reference

| area | functions |
|---|---|
| fit | `mca_run()` |
| residuals / tests | `mca_residuals()`, `mca_typicality()`, `mca_eta()`, `mca_ellipses()`, `mca_ari()` |
| tables | `mca_master()`, `mca_master_rows()`, `mca_top_dim()`, `mca_top_cluster()`, `mca_top_group()`, `mca_report()` |
| plots | `plot_scree()`, `plot_map()`, `mca_export_fig3d()` |
| validation | `mca_validate()` (requires FactoMineR) |

### `mca_run()` key arguments

- `active` — active variable columns.
- `group` — optional supplementary grouping for the residual / typicality / η² / ellipse analyses.
- `min_n` — drop categories with fewer than `min_n` observations (specific MCA).
- `k`, `ndim` — number of clusters, retained dimensions.
- `vocab` — optional named recode vector (`old = new`).
- `cluster_labels` — optional named list of signature keywords for content-based labelling.
- `anchors` — optional list of `list(pattern=, dim=, positive=)` to fix axis signs.
- `dedup`, `seed`.

### `plot_map()` controls

- `bw` — black-&-white (clusters by line-style + marker shape) or colour.
- `label_mode` — `"off"`, `"direct"` (labels at points), or `"boxed"`.
- `label_top` — categories per cluster to label (contribution-ranked on the shown axes).
- `repel` — spread labels apart with leader lines.
- `ellipse` — `"none"`, `"centroid"` (vanilla), or `"signature"` (top-`ellipse_topk`).

## License

MIT © 2026 Raymond Li
