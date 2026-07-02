# ============================================================================
# mcakit plotting: base-R scree + 2D cluster/category map (no dependencies).
# For the colour 3D map, use mca_export_fig3d() + the bundled matplotlib script.
# ============================================================================

#' Scree plot (Benzecri-adjusted), base graphics.
plot_scree <- function(fit, keep = fit$ndim, n_show = 10, col_keep = "grey30", col_drop = "grey80") {
  b <- fit$inertia$pct_benzecri; k <- min(n_show, sum(b > 0) + 2); b <- b[1:k]; cum <- cumsum(b)
  op <- par(mar = c(4.2, 4.4, 2, 4.4)); on.exit(par(op))
  cols <- ifelse(seq_len(k) <= keep, col_keep, col_drop)
  bp <- barplot(b, names.arg = 1:k, col = cols, border = "grey30",
                ylim = c(0, max(b) * 1.15), xlab = "Dimension", ylab = "Benzecri-adjusted inertia (%)")
  abline(v = (bp[keep] + bp[keep + 1]) / 2, lty = 2, col = "grey50")
  text(bp[1:keep], b[1:keep], sprintf("%.1f", b[1:keep]), pos = 3, cex = .8)
  par(new = TRUE)
  plot(bp, cum, type = "o", pch = 16, axes = FALSE, xlab = "", ylab = "", ylim = c(0, 100),
       xlim = c(min(bp) - .6, max(bp) + .6))
  axis(4); mtext("Cumulative (%)", side = 4, line = 2.7)
  invisible(fit)
}

# base-R iterative label de-collision (screen-space); no dependencies.
.repel <- function(x, y, labels, cex, iter = 400, k = 0.9) {
  n <- length(x); if (n < 2) return(cbind(x, y))
  w <- strwidth(labels, cex = cex) * 1.05; h <- strheight(labels, cex = cex) * 1.7
  px <- x; py <- y
  for (it in 1:iter) { moved <- FALSE
    for (a in 1:(n-1)) for (b in (a+1):n) {
      dx <- px[b]-px[a]; dy <- py[b]-py[a]
      ox <- (w[a]+w[b])/2 - abs(dx); oy <- (h[a]+h[b])/2 - abs(dy)
      if (ox > 0 && oy > 0) { moved <- TRUE; s <- k*oy/2*sign(ifelse(dy==0,1,dy))
        py[a] <- py[a]-s; py[b] <- py[b]+s } }
    if (!moved) break }
  cbind(px, py)
}
# 95% ellipse polygon from mean + 2x2 covariance
.ellipse_xy <- function(mu, S, level = 0.95, npt = 100) {
  rad <- sqrt(qchisq(level, 2)); ev <- eigen(S); th <- seq(0, 2*pi, len = npt)
  sweep(cbind(cos(th), sin(th)) %*% diag(rad*sqrt(pmax(ev$values,0))) %*% t(ev$vectors), 2, mu, "+")
}

#' 2D category/cluster map, base graphics — fully configurable, no dependencies.
#' @param bw          TRUE = black & white (cluster = line-style + marker shape); FALSE = colour.
#' @param label_mode  "direct" (small labels at points), "off", or "boxed" (white-boxed labels).
#' @param label_top   categories to label per cluster (ranked by contribution on these two axes).
#' @param repel       TRUE = iteratively spread labels apart to avoid overlap (with leader lines).
#' @param ellipse     "none", "centroid" (vanilla bootstrap ellipse of the group mean), or
#'                    "signature" (bootstrap ellipse of the group's top-`ellipse_topk` categories).
#' @param ellipse_topk k for the signature ellipse.
plot_map <- function(fit, dims = c(1, 2), bw = FALSE,
                     label_mode = c("direct","off","boxed"), label_top = 3, repel = FALSE,
                     ellipse = c("none","centroid","signature"), ellipse_topk = 5,
                     group_path = TRUE, palette = NULL, cex_lab = 0.62, B = 1000, seed = 2026) {
  label_mode <- match.arg(label_mode); ellipse <- match.arg(ellipse)
  K <- nlevels(fit$clusters); lv <- levels(fit$clusters)
  greys <- c("#4d4d4d","#8c8c8c","#bdbdbd","#1a1a1a","#666666","#999999")[1:K]
  if (is.null(palette)) palette <- c("#1b9e77","#2c7fb8","#d95f02","#7570b3","#e7298a","#66a61e")[1:K]
  col_c <- if (bw) greys else palette; names(col_c) <- lv
  lt_c  <- setNames(c(1,2,4,3,5,6)[1:K], lv)          # hull line-style (bw)
  pch_c <- setNames(c(1,0,2,5,6,3)[1:K], lv)          # group markers per cluster (unused legend fill)
  i <- dims[1]; j <- dims[2]
  G <- fit$coords[, c(i, j)]; lab <- sub("^[^=]+=", "", rownames(G))
  adj <- as.matrix(fit$master[, paste0("adjC_", lv)]); ccat <- lv[max.col(adj)]
  Fc <- fit$row_coords[, c(i, j)]; bz <- fit$inertia$pct_benzecri
  op <- par(mar = c(4.3, 4.3, 2.2, 1)); on.exit(par(op))
  plot(G, type = "n", asp = 1, xlab = sprintf("Dim %d (%.1f%%)", i, bz[i]),
       ylab = sprintf("Dim %d (%.1f%%)", j, bz[j]))
  abline(h = 0, v = 0, col = "grey85", lty = 3)
  # segment dots + cluster hulls
  for (C in lv) { pts <- Fc[fit$clusters == C, , drop = FALSE]
    points(pts, pch = 16, cex = .45, col = adjustcolor(if (bw) "grey60" else col_c[C], .5))
    if (nrow(pts) >= 3) { h <- chull(pts)
      polygon(pts[h, ], col = if (bw) NA else adjustcolor(col_c[C], .12),
              border = adjustcolor(col_c[C], if (bw) 1 else .5), lty = if (bw) lt_c[C] else 1) } }
  # optional group ellipses
  if (ellipse != "none" && !is.null(fit$group)) {
    tk <- if (ellipse == "signature") ellipse_topk else NULL
    el <- mca_ellipses(fit, dims = dims, topk = tk, B = B, seed = seed)
    for (g in names(el$ellipses)) polygon(.ellipse_xy(el$ellipses[[g]]$mu, el$ellipses[[g]]$S),
      border = "grey30", col = adjustcolor("grey40", .08), lwd = 1, lty = 1)
  }
  # category '+' marks (top-k per cluster by contribution on this plane)
  keep <- unlist(lapply(lv, function(C){ idx <- which(ccat == C)
    idx[order(fit$master[[paste0("ctr_D",i)]][idx] + fit$master[[paste0("ctr_D",j)]][idx], decreasing = TRUE)][1:min(label_top, length(idx))] }))
  points(G[keep, , drop=FALSE], pch = 3, cex = .8, col = if (bw) "black" else col_c[ccat[keep]], lwd = 1.4)
  # labels
  if (label_mode != "off" && length(keep)) {
    lx <- G[keep,1]; ly <- G[keep,2]; lb <- lab[keep]
    if (repel) { pos <- .repel(lx, ly, lb, cex_lab); segments(lx, ly, pos[,1], pos[,2], col="grey70", lwd=.5)
      lx2 <- pos[,1]; ly2 <- pos[,2] } else { lx2 <- lx; ly2 <- ly }
    if (label_mode == "boxed") {
      wd <- strwidth(lb, cex=cex_lab)*1.1; ht <- strheight(lb, cex=cex_lab)*1.4
      rect(lx2-wd/2, ly2-ht/2, lx2+wd/2, ly2+ht/2, col="white", border="grey60", lwd=.5)
      text(lx2, ly2, lb, cex=cex_lab, col="black")
    } else text(lx2, ly2, lb, cex=cex_lab, col="grey10", pos = if (repel) NULL else 3, offset=.3)
  }
  # group barycentre path
  if (group_path && !is.null(fit$group)) { gs <- sort(unique(fit$group))
    cen <- t(sapply(gs, function(p) colMeans(Fc[fit$group == p, , drop = FALSE])))
    lines(cen, lwd = 2); points(cen, pch = 21, bg = "white", cex = 2.2, lwd = 1.5); text(cen, gs, cex = .7, font = 2) }
  legend("topleft", legend = paste0(lv, " — ", fit$cluster_pretty[lv]),
         col = col_c, lty = if (bw) lt_c else 1, pch = if (bw) NA else 3, lwd = 1.4, cex = .62, bty = "n")
  invisible(fit)
}

#' Export the 5 CSVs the bundled matplotlib 3D renderer consumes.
mca_export_fig3d <- function(fit, dir = ".", B = 400, seed = 2026) {
  G <- fit$coords[, 1:3]; catnm <- sub("^[^=]+=", "", rownames(G))
  adj <- as.matrix(fit$master[, paste0("adjC_", levels(fit$clusters))]); defclu <- max.col(adj) - 1L
  top3 <- lapply(seq_along(levels(fit$clusters)), function(k) order(adj[, k], decreasing = TRUE)[1:3])
  is_top <- rep(FALSE, nrow(G)); for (t in top3) is_top[t] <- TRUE
  write.csv(data.frame(d1=fit$row_coords[,1], d2=fit$row_coords[,2], d3=fit$row_coords[,3],
    clu=as.integer(fit$clusters)-1L, group=fit$group), file.path(dir,"fig3d_seg.csv"), row.names=FALSE)
  write.csv(data.frame(name=catnm, d1=G[,1], d2=G[,2], d3=G[,3], clu=defclu, is_top=is_top),
    file.path(dir,"fig3d_cat.csv"), row.names=FALSE)
  nk <- fit$count; n <- fit$n; Z <- fit$Z; gs <- sort(unique(fit$group))
  sigloc <- function(p) { res <- mca_residuals(fit,"group")[, p]; o <- order(res, decreasing=TRUE)[1:5]
    w <- pmax(res[o], 0); if (!sum(w)) w <- rep(1,5); colSums(G[o,]*w)/sum(w) }
  per <- do.call(rbind, lapply(gs, function(p) rbind(
    data.frame(group=p, kind="sig",  t(sigloc(p))),
    data.frame(group=p, kind="cent", t(colMeans(fit$row_coords[fit$group==p,1:3])))))); names(per)[3:5]<-c("d1","d2","d3")
  write.csv(per, file.path(dir,"fig3d_period.csv"), row.names=FALSE)
  set.seed(seed)
  bcov <- function(p){ ix<-which(fit$group==p); m<-length(ix)
    locs<-t(replicate(B,{ s<-sample(ix,m,TRUE); O<-colSums(Z[s,]); E<-m*nk/n; res<-ifelse(E>0,(O-E)/sqrt(E),0)
      o<-order(res,decreasing=TRUE)[1:5]; w<-pmax(res[o],0); if(!sum(w)) w<-rep(1,5); colSums(G[o,]*w)/sum(w) })); as.numeric(cov(locs)) }
  cov <- do.call(rbind, lapply(gs, function(p) data.frame(group=p, t(bcov(p))))); names(cov)[2:10]<-paste0("c",1:9)
  write.csv(cov, file.path(dir,"fig3d_cov.csv"), row.names=FALSE)
  write.csv(data.frame(dim=1:3, benz=fit$inertia$pct_benzecri[1:3]), file.path(dir,"fig3d_meta.csv"), row.names=FALSE)
  invisible(file.path(dir, c("fig3d_seg.csv","fig3d_cat.csv","fig3d_period.csv","fig3d_cov.csv","fig3d_meta.csv")))
}
