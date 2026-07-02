# Reproducible generator for man/figures/hero.png (illustrative synthetic data, MIT).
# 4 latent archetypes over 8 categorical variables -> a well-separated MCA cloud.
# Requires: source the package, then run this, then tools/hero3d.py (matplotlib).
for (f in list.files("R", full.names = TRUE)) source(f)
set.seed(42)
K <- 4; nper <- c(150, 130, 110, 90); vars <- paste0("q", 1:8); levs <- c("a","b","c","d")
arche <- matrix(sample(levs, K * length(vars), TRUE), K, length(vars))
df <- do.call(rbind, lapply(1:K, function(k) {
  n <- nper[k]
  as.data.frame(setNames(lapply(seq_along(vars), function(v)
    ifelse(runif(n) < 0.72, arche[k, v], sample(levs, n, TRUE))), vars), stringsAsFactors = FALSE) }))
fit <- mca_run(df, active = vars, k = 4)
write.csv(data.frame(d1 = fit$row_coords[,1], d2 = fit$row_coords[,2], d3 = fit$row_coords[,3],
  clu = as.integer(fit$clusters) - 1L), "fig3d_seg.csv", row.names = FALSE)
write.csv(data.frame(dim = 1:3, benz = fit$inertia$pct_benzecri[1:3]), "fig3d_meta.csv", row.names = FALSE)
cat("wrote fig3d_seg.csv / fig3d_meta.csv — now run: python tools/hero3d.py\n")
