# ============================================================================
# mcakit validation: cross-check every MCA number against FactoMineR (optional).
# Requires the FactoMineR package. Returns a ranked difference table.
# ============================================================================

#' Compare an mca_fit against FactoMineR's specific MCA + HCPC.
#' @return list(compare = ranked difference table, summary = per-metric max/mean diff, hcpc_ari = numeric).
mca_validate <- function(fit) {
  if (!requireNamespace("FactoMineR", quietly = TRUE)) stop("install.packages('FactoMineR')")
  fin <- fit$data; active <- fit$active
  facdf <- as.data.frame(lapply(active, function(v)
    factor(paste0(v, "=", fin[[v]]), levels = paste0(v, "=", sort(unique(fin[[v]]))))))
  names(facdf) <- active
  allcats  <- unlist(lapply(facdf, levels), use.names = FALSE)
  excl_idx <- if (length(fit$dropped)) match(fit$dropped, allcats) else NULL
  mca <- FactoMineR::MCA(facdf, excl = excl_idx, ncp = 5, graph = FALSE)

  G <- fit$coords; lam <- fit$lam; cm <- fit$mass
  ctr_ours <- sweep(sweep(G^2, 1, cm, "*"), 2, lam, "/") * 100
  cos2_ours <- G^2 / rowSums(G^2)
  fm_eig <- mca$eig[,1]; fm_coord <- mca$var$coord; fm_ctr <- mca$var$contrib; fm_cos2 <- mca$var$cos2
  shared <- intersect(rownames(G), rownames(fm_coord)); K <- min(3, fit$ndim)
  sgn <- sapply(1:K, function(d) sign(sum(G[shared,d]*fm_coord[shared,d])))
  fm_coord[,1:K] <- sweep(fm_coord[,1:K,drop=FALSE], 2, sgn, "*")

  row <- function(metric,item,a,b) data.frame(metric,item,ours=a,factominer=b)
  Ke <- min(length(lam), length(fm_eig))
  cmp <- rbind(
    row("eigenvalue", paste0("Dim",1:Ke), lam[1:Ke], fm_eig[1:Ke]),
    do.call(rbind, lapply(1:K, function(d) row("coord",   paste0(shared," | D",d), G[shared,d],        fm_coord[shared,d]))),
    do.call(rbind, lapply(1:K, function(d) row("contrib%",paste0(shared," | D",d), ctr_ours[shared,d], fm_ctr[shared,d]))),
    do.call(rbind, lapply(1:K, function(d) row("cos2",    paste0(shared," | D",d), cos2_ours[shared,d],fm_cos2[shared,d]))))
  cmp$diff <- cmp$ours - cmp$factominer; cmp$abs_diff <- abs(cmp$diff)
  cmp <- cmp[order(-cmp$abs_diff), ]
  summ <- aggregate(abs_diff ~ metric, cmp, function(x) c(max = max(x), mean = mean(x), n = length(x)))

  hc <- FactoMineR::HCPC(mca, nb.clust = fit$call$k, consol = TRUE, graph = FALSE)
  fm_cl <- as.integer(hc$data.clust[as.character(seq_len(fit$n)), "clust"])
  list(compare = cmp, summary = summ, hcpc_ari = round(mca_ari(as.integer(fit$clusters), fm_cl), 3))
}
