# ============================================================================
# mcakit : lightweight, base-R specific MCA + HCPC + geometric data analysis.
# No package dependencies for the stats. Reproducible (seeded). Works anywhere.
# ============================================================================

#' Fit specific MCA + HCPC clustering to a coded data frame.
#'
#' @param data      data.frame of categorical (coded) variables.
#' @param active    character vector: the active variable column names.
#' @param group     optional column name of a supplementary grouping (e.g. period)
#'                  used for residual / typicality / eta / ellipse analyses.
#' @param min_n     drop categories with fewer than min_n observations (specific MCA).
#' @param k         number of HCPC clusters.
#' @param ndim      number of retained dimensions used for clustering.
#' @param vocab     optional named character vector of label recodes (old = new).
#' @param cluster_labels optional named list of signature keyword vectors; clusters are
#'                  matched to these names bijectively by content (prevents mislabeling).
#'                  Names like "C1_Foo" -> cluster id "C1". If NULL, clusters are C1..Ck by size-order.
#' @param anchors   optional list of list(pattern=, dim=, positive=) to fix axis signs.
#' @param seed      RNG seed.
#' @return object of class "mca_fit".
mca_run <- function(data, active, group = NULL, min_n = 0, k = 4, ndim = 3,
                    vocab = NULL, cluster_labels = NULL, anchors = NULL, dedup = FALSE,
                    seed = 2026, preset = NULL) {
  stopifnot(is.data.frame(data), all(active %in% names(data)))
  if (!is.null(preset)) {                       # a preset bundles vocab/labels/anchors; explicit args win
    if (is.null(vocab))          vocab          <- preset$vocab
    if (is.null(cluster_labels)) cluster_labels <- preset$cluster_labels
    if (is.null(anchors))        anchors        <- preset$anchors
  }
  set.seed(seed)
  fin <- as.data.frame(data, stringsAsFactors = FALSE)
  if (dedup) fin <- fin[!duplicated(fin), , drop = FALSE]
  if (!is.null(vocab)) for (v in active) {
    x <- trimws(as.character(fin[[v]])); i <- x %in% names(vocab); x[i] <- vocab[x[i]]; fin[[v]] <- x
  }
  blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
  keep  <- rowSums(sapply(active, function(v) blank(fin[[v]]))) == 0
  fin   <- fin[keep, , drop = FALSE]
  grpv  <- if (!is.null(group)) as.character(fin[[group]]) else NULL
  Q     <- length(active)

  Z0 <- do.call(cbind, lapply(active, function(v) {
    f <- factor(fin[[v]]); m <- model.matrix(~0 + f); colnames(m) <- paste0(v, "=", levels(f)); m }))
  Ntot <- sum(Z0); P <- Z0 / Ntot; r <- rowSums(P); cm0 <- colSums(P)
  Smat <- (P - outer(r, cm0)) / sqrt(outer(r, cm0))
  act  <- colSums(Z0) >= min_n
  sv   <- svd(Smat[, act]); Z <- Z0[, act]; cm <- cm0[act]; lam <- sv$d^2; nk <- colSums(Z); n <- nrow(Z)
  G    <- (sv$v * rep(sv$d, each = ncol(Z))) / sqrt(cm); rownames(G) <- colnames(Z)
  Fc   <- (sv$u * rep(sv$d, each = nrow(Z))) / sqrt(r)
  ndim <- min(ndim, ncol(G))

  if (!is.null(anchors)) for (a in anchors) {
    i <- grep(a$pattern, rownames(G), fixed = TRUE)[1]
    s <- if (is.na(i)) 1 else if (sign(G[i, a$dim]) == ifelse(a$positive, 1, -1)) 1 else -1
    G[, a$dim] <- G[, a$dim] * s; Fc[, a$dim] <- Fc[, a$dim] * s
  }
  ctr  <- sweep(sweep(G^2, 1, cm, "*"), 2, lam, "/"); rownames(ctr) <- colnames(Z)
  cos2 <- G^2 / rowSums(G^2)

  residf <- function(gv, adj) {
    gv <- as.character(gv); lv <- sort(unique(gv))
    m <- sapply(lv, function(g) { ix <- which(gv == g); ng <- length(ix)
      O <- colSums(Z[ix, , drop = FALSE]); E <- ng * nk / n
      if (adj) (O - E) / sqrt(E * (1 - ng / n) * (1 - nk / n)) else (O - E) / sqrt(E) })
    if (is.null(dim(m))) m <- matrix(m, ncol = length(lv))
    colnames(m) <- lv; rownames(m) <- colnames(Z); m }

  W <- Fc[, 1:ndim, drop = FALSE]; set.seed(seed)
  cl <- cutree(hclust(dist(W), "ward.D2"), k)
  cl <- kmeans(W, t(sapply(1:k, function(g) colMeans(W[cl == g, , drop = FALSE]))))$cluster

  if (!is.null(cluster_labels)) {
    vt0 <- residf(cl, TRUE); colnames(vt0) <- paste0("k", colnames(vt0))
    topN <- apply(vt0, 2, function(co) names(sort(co, decreasing = TRUE))[1:6])
    sco  <- sapply(cluster_labels, function(sig) apply(topN, 2, function(cd)
              sum(sapply(sig, function(s) any(grepl(s, cd, fixed = TRUE))))))
    amap <- character(nrow(sco)); names(amap) <- rownames(sco); tmp <- sco
    for (i in 1:nrow(sco)) { ij <- which(tmp == max(tmp), arr.ind = TRUE)[1, ]
      amap[rownames(tmp)[ij[1]]] <- colnames(sco)[ij[2]]; tmp[ij[1], ] <- -Inf; tmp[, ij[2]] <- -Inf }
    cid  <- ifelse(grepl("_", names(cluster_labels)), sub("_.*", "", names(cluster_labels)), names(cluster_labels))
    names(cid) <- names(cluster_labels)
    clC  <- factor(cid[amap[paste0("k", cl)]], levels = cid)
    pretty <- setNames(names(cluster_labels), cid)
  } else {
    ord <- names(sort(table(cl), decreasing = TRUE)); relab <- setNames(paste0("C", seq_along(ord)), ord)
    clC <- factor(relab[as.character(cl)], levels = paste0("C", 1:k)); amap <- NULL
    pretty <- setNames(paste0("C", 1:k), paste0("C", 1:k))
  }

  benz <- ifelse(lam > 1/Q, (Q/(Q-1))^2 * (lam - 1/Q)^2, 0)
  inertia <- data.frame(dim = seq_along(lam), raw_eig = round(lam, 4),
    pct_raw = round(100*lam/sum(lam), 1), pct_benzecri = round(100*benz/sum(benz), 1))

  adC <- residf(clC, TRUE)
  master <- data.frame(category = colnames(Z), count = nk, mass = round(cm, 4),
    round(as.data.frame(G[, 1:ndim]),    3) |> setNames(paste0("coord_D", 1:ndim)),
    round(as.data.frame(ctr[, 1:ndim]),  3) |> setNames(paste0("ctr_D",   1:ndim)),
    round(as.data.frame(cos2[, 1:ndim]), 3) |> setNames(paste0("cos2_D",  1:ndim)),
    check.names = FALSE, row.names = NULL)
  adCd <- as.data.frame(round(adC, 2)); names(adCd) <- paste0("adjC_", colnames(adC))
  master <- cbind(master, adCd)
  if (!is.null(grpv)) {
    adP <- residf(grpv, TRUE); adPd <- as.data.frame(round(adP, 2)); names(adPd) <- paste0("adjG_", colnames(adP))
    master <- cbind(master, adPd)
  }

  gm <- colMeans(W); tot <- sum(sweep(W, 2, gm)^2)
  bpct <- function(cc) 100*sum(sapply(split(1:n, cc), function(ix){ mk <- colMeans(W[ix,,drop=FALSE]); length(ix)*sum((mk-gm)^2) }))/tot
  hcw <- hclust(dist(W), "ward.D2")
  gain_tab <- data.frame(k = 2:max(6,k+1),
    between_pct = round(sapply(2:max(6,k+1), function(kk) bpct(cutree(hcw, kk))), 1))
  gain_tab$gain <- c(NA, round(diff(gain_tab$between_pct), 1))

  structure(list(
    call = list(min_n=min_n, k=k, ndim=ndim, group=group, seed=seed),
    n = n, n_cats = ncol(Z), Q = Q, dropped = colnames(Z0)[!act], cluster_map = amap,
    coords = G, row_coords = Fc, lam = lam, mass = cm, rmass = r, Ntot = Ntot, count = nk,
    ctr = ctr, cos2 = cos2, Z = Z, data = fin, active = active, group = grpv,
    clusters = clC, cluster_pretty = pretty, ndim = ndim,
    inertia = inertia, master = master, between_inertia = round(bpct(clC), 1), gain_tab = gain_tab
  ), class = "mca_fit")
}

print.mca_fit <- function(x, ...) {
  cat("<mca_fit>  N =", x$n, "segments |", x$n_cats, "active categories | k =", x$call$k,
      "clusters | dims retained =", x$ndim, "\n")
  cat("Benzecri-adjusted inertia (D1-3):", paste0(head(x$inertia$pct_benzecri, 3), "%", collapse=" / "),
      "| between-cluster inertia:", x$between_inertia, "%\n")
  cat("cluster sizes:", paste(paste0(levels(x$clusters), "=", as.integer(table(x$clusters))), collapse=" "), "\n")
  invisible(x)
}

# ------------------------------------------------------------------ residuals
#' Standardized residuals of categories against a grouping.
#' @param by "cluster" (default) or "group" (the supplementary grouping).
#' @param type "adjusted" (Haberman) or "pearson".
mca_residuals <- function(fit, by = c("cluster","group"), type = c("adjusted","pearson")) {
  by <- match.arg(by); type <- match.arg(type)
  gv <- if (by == "cluster") fit$clusters else fit$group
  if (is.null(gv)) stop("no grouping available for by='group'")
  gv <- as.character(gv); lv <- sort(unique(gv)); nk <- fit$count; n <- fit$n; Z <- fit$Z
  m <- sapply(lv, function(g){ ix <- which(gv==g); ng <- length(ix); O <- colSums(Z[ix,,drop=FALSE]); E <- ng*nk/n
    if (type=="adjusted") (O-E)/sqrt(E*(1-ng/n)*(1-nk/n)) else (O-E)/sqrt(E) })
  rownames(m) <- colnames(Z); m
}

# ------------------------------------------------------- top-k drivers helpers
mca_top_dim <- function(fit, k = 5) do.call(rbind, lapply(1:fit$ndim, function(d){
  m <- fit$master; o <- order(m[[paste0("ctr_D",d)]], decreasing=TRUE)[1:k]
  data.frame(dim=paste0("D",d), category=m$category[o], count=m$count[o],
    coord=m[[paste0("coord_D",d)]][o], ctr_pct=round(100*m[[paste0("ctr_D",d)]][o],1),
    cos2=m[[paste0("cos2_D",d)]][o], row.names=NULL) }))

mca_top_cluster <- function(fit, k = 5) do.call(rbind, lapply(levels(fit$clusters), function(C){
  m <- fit$master; col <- paste0("adjC_",C); o <- order(m[[col]], decreasing=TRUE)[1:k]
  inc <- colSums(fit$Z[fit$clusters==C, m$category[o], drop=FALSE]); sz <- sum(fit$clusters==C)
  data.frame(cluster=C, category=m$category[o], vtest=m[[col]][o], n_in_cluster=inc, cluster_size=sz,
    pct_of_cluster=round(100*inc/sz,1), total=m$count[o], pct_of_category=round(100*inc/m$count[o],1), row.names=NULL) }))

mca_top_group <- function(fit, k = 5) { stopifnot(!is.null(fit$group))
  do.call(rbind, lapply(sort(unique(fit$group)), function(p){
    m <- fit$master; col <- paste0("adjG_",p); o <- order(m[[col]], decreasing=TRUE)[1:k]
    inp <- colSums(fit$Z[fit$group==p, m$category[o], drop=FALSE]); sz <- sum(fit$group==p)
    data.frame(group=p, category=m$category[o], adj_resid=m[[col]][o], n_in_group=inp, group_size=sz,
      pct_of_group=round(100*inp/sz,1), total=m$count[o], pct_of_category=round(100*inp/m$count[o],1), row.names=NULL) })) }

# --------------------------------------------------- geometric typicality test
#' Geometric typicality Z for each group on each axis: Z = mean(F_g,d)/sqrt(V), V=((N-ng)/(ng(N-1)))*lam_d.
mca_typicality <- function(fit) { stopifnot(!is.null(fit$group))
  gv <- fit$group; n <- fit$n; Fc <- fit$row_coords; lam <- fit$lam
  z <- sapply(sort(unique(gv)), function(p) sapply(1:fit$ndim, function(d){
    ix <- which(gv==p); ng <- length(ix); mean(Fc[ix,d]) / sqrt((n-ng)/(ng*(n-1))*lam[d]) }))
  rownames(z) <- paste0("D",1:fit$ndim); round(z, 2) }

# ---------------------------------- geometric typicality Z per CATEGORY x axis
# Same test applied to each category's subcloud (individuals sharing the category):
# Z = mbar / sqrt((N - n_k)/(n_k (N-1)) * lambda_d).  |Z| > 1.96 = atypical.
mca_category_typicality <- function(fit, dims = seq_len(fit$ndim)) {
  Fi <- fit$row_coords; Z <- fit$Z; N <- fit$n; lam <- fit$lam; cats <- colnames(Z)
  z <- vapply(dims, function(d) vapply(seq_along(cats), function(k) {
    ix <- which(Z[, k] == 1); nk <- length(ix)
    mean(Fi[ix, d]) / sqrt((N - nk) / (nk * (N - 1)) * lam[d]) }, numeric(1)), numeric(length(cats)))
  rownames(z) <- cats; colnames(z) <- paste0("D", dims); round(z, 2) }

# --------------------------------------------- correlation ratio eta^2 + F + p
mca_eta <- function(fit, B_perm = 9999, seed = 2026) { stopifnot(!is.null(fit$group))
  gg <- factor(fit$group); Gn <- nlevels(gg); n <- fit$n; W <- fit$row_coords[,1:fit$ndim,drop=FALSE]
  eta2 <- function(M){ M<-as.matrix(M); mm<-colMeans(M); tt<-sum(sweep(M,2,mm)^2)
    bw <- sum(sapply(split(1:nrow(M), gg), function(ix){ mk<-colMeans(M[ix,,drop=FALSE]); length(ix)*sum((mk-mm)^2) })); bw/tt }
  pp <- function(M){ set.seed(seed); ob<-eta2(M); nu<-replicate(B_perm,{ gg<<-sample(gg); e<-eta2(M); gg<<-factor(fit$group); e }); (1+sum(nu>=ob))/(B_perm+1) }
  sp <- c("D1-Dn", paste0("D",1:fit$ndim))
  Ms <- c(list(W), lapply(1:fit$ndim, function(d) W[,d,drop=FALSE]))
  tab <- data.frame(space=sp, eta2=round(sapply(Ms, eta2),4))
  tab$F <- round((tab$eta2/(Gn-1))/((1-tab$eta2)/(n-Gn)),2)
  tab$perm_p <- sapply(Ms, pp); tab }

# ------------------------------------------- bootstrap group confidence ellipses
#' Bootstrap ellipse (centroid or top-k signature) params + pairwise overlap, per dim-pair.
mca_ellipses <- function(fit, dims = c(1,2), topk = NULL, B = 2000, seed = 2026) {
  stopifnot(!is.null(fit$group)); gv <- fit$group; Fc <- fit$row_coords; G <- fit$coords
  Z <- fit$Z; nk <- fit$count; n <- fit$n; rad <- sqrt(qchisq(.95, 2))
  bc <- function(p){ ix<-which(gv==p); m<-length(ix); set.seed(seed)
    locs <- t(replicate(B, { s<-sample(ix,m,TRUE)
      if (is.null(topk)) colMeans(Fc[s,dims]) else {
        O<-colSums(Z[s,]); E<-m*nk/n; rr<-ifelse(E>0,(O-E)/sqrt(E),0); tp<-order(rr,decreasing=TRUE)[1:topk]
        w<-pmax(rr[tp],0); if(!sum(w)) w<-rep(1,topk); colSums(G[tp,dims]*w)/sum(w) } }))
    list(mu=colMeans(locs), S=cov(locs)) }
  bd  <- function(e){ th<-seq(0,2*pi,len=200); ev<-eigen(e$S)
    sweep(cbind(cos(th),sin(th)) %*% diag(rad*sqrt(ev$values)) %*% t(ev$vectors), 2, e$mu, "+") }
  ovl <- function(a,b) any(mahalanobis(bd(a),b$mu,b$S)<=rad^2) || any(mahalanobis(bd(b),a$mu,a$S)<=rad^2)
  gs  <- sort(unique(gv)); es <- setNames(lapply(gs, bc), gs)
  params <- do.call(rbind, lapply(gs, function(p){ e<-es[[p]]; ev<-eigen(e$S)
    data.frame(group=p, cx=e$mu[1], cy=e$mu[2], semi1=rad*sqrt(ev$values[1]), semi2=rad*sqrt(ev$values[2]),
      angle=atan2(ev$vectors[2,1], ev$vectors[1,1])*180/pi, row.names=NULL) }))
  prs <- combn(gs, 2, simplify = FALSE)
  overlap <- do.call(rbind, lapply(prs, function(pr) data.frame(g1=pr[1], g2=pr[2], overlap=ovl(es[[pr[1]]], es[[pr[2]]]))))
  list(params = params, overlap = overlap, ellipses = es, dims = dims)
}

# ------------------------------------------------------------- enriched masters
# project EXCLUDED (non-active) variables as SUPPLEMENTARY points: their categories
# get coordinates on the retained axes (barycenter of the individuals possessing them,
# G_sup = mean(F)/sqrt(lambda)) WITHOUT shaping the axes, plus a typicality Z.
# vars = NULL auto-detects character columns in fit$data that are not active/metadata.
mca_supplementary <- function(fit, vars = NULL) {
  Fi <- fit$row_coords; N <- fit$n; lam <- fit$lam; nd <- fit$ndim
  meta <- c("_sheet", "SEGMENT", "Publication Year", "period", "cluster", "cluster_label", "seg_id")
  if (is.null(vars)) {
    cand <- setdiff(names(fit$data), c(fit$active, meta, fit$call$group))
    vars <- cand[vapply(cand, function(v) is.character(fit$data[[v]]) || is.factor(fit$data[[v]]), logical(1))]
  }
  if (!length(vars)) return(NULL)
  do.call(rbind, lapply(vars, function(v) {
    vals <- as.character(fit$data[[v]]); lv <- sort(unique(vals[!is.na(vals) & vals != ""]))
    do.call(rbind, lapply(lv, function(L) {
      ix <- which(vals == L); nk <- length(ix)
      co <- vapply(seq_len(nd), function(d) mean(Fi[ix, d]) / sqrt(lam[d]), numeric(1))
      z  <- vapply(seq_len(nd), function(d) mean(Fi[ix, d]) / sqrt((N - nk) / (nk * (N - 1)) * lam[d]), numeric(1))
      out <- data.frame(variable = v, category = L, n = nk, check.names = FALSE, row.names = NULL)
      out[paste0("coord_D", seq_len(nd))] <- round(co, 3)
      out[paste0("Z_D",     seq_len(nd))] <- round(z, 2)
      out })) }))
}

# the final analytical dataset actually fitted (post clean/dedup/specific-MCA),
# one row per segment, with its cluster assignment appended.
mca_dataset <- function(fit) {
  df <- as.data.frame(fit$data, check.names = FALSE)
  df$cluster <- as.character(fit$clusters)
  if (!is.null(fit$cluster_pretty))
    df$cluster_label <- unname(fit$cluster_pretty[as.character(fit$clusters)])
  df
}

mca_master <- function(fit) {
  m <- fit$master; Z <- fit$Z; cats <- m$category; variable <- sub("=.*", "", cats)
  vtot <- ave(m$count, variable, FUN = sum)
  out <- data.frame(variable = variable, m,
    pct_of_total = round(100*m$count/fit$n, 1),
    pct_within_variable = round(100*m$count/vtot, 1), check.names = FALSE, row.names = NULL)
  nC <- sapply(levels(fit$clusters), function(C) colSums(Z[fit$clusters==C,,drop=FALSE]))
  csz <- as.integer(table(fit$clusters))
  shC <- round(100*sweep(nC,2,csz,"/"),1); ofC <- round(100*nC/m$count,1)
  colnames(nC) <- paste0("n_",levels(fit$clusters)); colnames(shC) <- paste0("shareOf_",levels(fit$clusters)); colnames(ofC) <- paste0("catIn_",levels(fit$clusters))
  out <- cbind(out, nC, shC, ofC)
  if (!is.null(fit$group)) {
    gs <- sort(unique(fit$group)); nG <- sapply(gs, function(p) colSums(Z[fit$group==p,,drop=FALSE]))
    gsz <- as.integer(table(fit$group)[gs]); shG <- round(100*sweep(nG,2,gsz,"/"),1); ofG <- round(100*nG/m$count,1)
    colnames(nG) <- paste0("n_",gs); colnames(shG) <- paste0("shareOf_",gs); colnames(ofG) <- paste0("catIn_",gs)
    out <- cbind(out, nG, shG, ofG) }
  out
}

mca_master_rows <- function(fit) {
  Fk <- fit$row_coords[,1:fit$ndim,drop=FALSE]; d2 <- rowSums(fit$row_coords^2)
  ctr <- round(100*sweep(Fk^2,2,fit$lam[1:fit$ndim],"/")*fit$rmass,3); colnames(ctr)<-paste0("ctr_D",1:fit$ndim)
  cos2 <- round(Fk^2/d2,3); colnames(cos2)<-paste0("cos2_D",1:fit$ndim)
  coord <- round(Fk,3); colnames(coord)<-paste0("coord_D",1:fit$ndim)
  base <- data.frame(row_id = seq_len(fit$n), cluster = as.character(fit$clusters), row_mass = round(fit$rmass,6))
  if (!is.null(fit$group)) base$group <- fit$group
  cbind(base, coord, ctr, cos2, dist2 = round(d2,3), fit$data[, fit$active, drop=FALSE], row.names=NULL)
}

# ------------------------------------------------------------------- reporting
mca_report <- function(fit) {
  H <- function(t, note) cat("\n", strrep("=",76), "\n  ", t, "\n", strrep("-",76), "\n  ", note, "\n\n", sep="")
  H(sprintf("MCA + HCPC SUMMARY (N=%d, %d categories, k=%d, min_n=%d)", fit$n, fit$n_cats, fit$call$k, fit$call$min_n),
    "Specific MCA on the indicator matrix; HCPC = Ward.D2 + k-means consolidation. Seeded.")
  print(fit$inertia[1:min(6,nrow(fit$inertia)),], row.names=FALSE)
  H("TOP-5 CATEGORIES PER DIMENSION (contribution)", "coord=position; ctr_pct=% of axis inertia; cos2=fit.")
  print(mca_top_dim(fit), row.names=FALSE)
  H("TOP-5 CHARACTERISTIC CATEGORIES PER CLUSTER (v-test)", "vtest>1.96 = over-represented.")
  print(mca_top_cluster(fit), row.names=FALSE)
  if (!is.null(fit$group)) { H("TOP-5 OVER-REPRESENTED CATEGORIES PER GROUP (adjusted residual)", "adj_resid ~ N(0,1).")
    print(mca_top_group(fit), row.names=FALSE) }
  invisible(fit)
}

#' Adjusted Rand Index (base R, chance-corrected clustering agreement).
mca_ari <- function(a, b) { t <- table(a,b); nn <- sum(t)
  s <- sum(choose(t,2)); ai <- sum(choose(rowSums(t),2)); bj <- sum(choose(colSums(t),2))
  ex <- ai*bj/choose(nn,2); (s-ex)/(.5*(ai+bj)-ex) }

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
                     label_offset = 0.7, legend = TRUE,
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
    dy <- strheight("Ag", cex = cex_lab) * label_offset                 # vertical nudge above the '+'
    if (repel) {                                                        # spread labels, leaders back to the '+'
      pos <- .repel(lx, ly + dy, lb, cex_lab); segments(lx, ly, pos[,1], pos[,2], col="grey70", lwd=.5)
      if (label_mode == "boxed") { wd <- strwidth(lb,cex=cex_lab)*1.1; ht <- strheight(lb,cex=cex_lab)*1.5
        rect(pos[,1]-wd/2, pos[,2]-ht/2, pos[,1]+wd/2, pos[,2]+ht/2, col="white", border="grey60", lwd=.5) }
      text(pos[,1], pos[,2], lb, cex=cex_lab, col=if (label_mode=="boxed") "black" else "grey10")
    } else if (label_mode == "boxed") {                                # boxed label sitting above the '+'
      wd <- strwidth(lb,cex=cex_lab)*1.1; ht <- strheight(lb,cex=cex_lab)*1.5; yb <- ly + dy + ht/2
      rect(lx-wd/2, yb-ht/2, lx+wd/2, yb+ht/2, col="white", border="grey60", lwd=.5)
      text(lx, yb, lb, cex=cex_lab, col="black")
    } else {                                                           # plain label centred ABOVE the '+'
      text(lx, ly + dy, lb, cex=cex_lab, col="grey10", adj=c(0.5,0))
    }
  }
  # group barycentre path
  if (group_path && !is.null(fit$group)) { gs <- sort(unique(fit$group))
    cen <- t(sapply(gs, function(p) colMeans(Fc[fit$group == p, , drop = FALSE])))
    lines(cen, lwd = 2); points(cen, pch = 21, bg = "white", cex = 2.2, lwd = 1.5); text(cen, gs, cex = .7, font = 2) }
  if (legend)
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
  segdf <- data.frame(d1=fit$row_coords[,1], d2=fit$row_coords[,2], d3=fit$row_coords[,3], clu=as.integer(fit$clusters)-1L)
  if (!is.null(fit$group)) segdf$group <- fit$group
  write.csv(segdf, file.path(dir,"fig3d_seg.csv"), row.names=FALSE)
  write.csv(data.frame(name=catnm, d1=G[,1], d2=G[,2], d3=G[,3], clu=defclu, is_top=is_top),
    file.path(dir,"fig3d_cat.csv"), row.names=FALSE)
  write.csv(data.frame(dim=1:3, benz=fit$inertia$pct_benzecri[1:3]), file.path(dir,"fig3d_meta.csv"), row.names=FALSE)
  write.csv(data.frame(clu=seq_along(levels(fit$clusters))-1L, cluster=levels(fit$clusters),
    label=unname(fit$cluster_pretty[levels(fit$clusters)])), file.path(dir,"fig3d_labels.csv"), row.names=FALSE)
  files <- c("fig3d_seg.csv","fig3d_cat.csv","fig3d_meta.csv","fig3d_labels.csv")
  if (!is.null(fit$group)) {                                   # period/signature files only when a grouping exists
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
    files <- c(files, "fig3d_period.csv", "fig3d_cov.csv")
  }
  invisible(file.path(dir, files))
}

#' Frequency of coded categories by the supplementary grouping (e.g. period).
#'
#' For each active variable, the distribution of each group's segments across that
#' variable's categories -- i.e. column percentages that sum to 100 within each group
#' (the "frequency trends" descriptives). Returns a named list of category-by-group
#' tables, or one long data.frame (Variable, Category, one column per group) if long=TRUE.
#'
#' @param fit   an mca_fit with a `group`.
#' @param vars  active variables to tabulate (default all).
#' @param pct   percentages (default) or raw counts.
#' @param digits rounding for percentages.
#' @param long  TRUE -> a single tidy data.frame suitable for a table.
#' @export
mca_frequencies <- function(fit, vars = fit$active, pct = TRUE, digits = 0, long = FALSE) {
  if (is.null(fit$group)) stop("no grouping available (fit was built without `group`)")
  g <- factor(fit$group)
  tabs <- lapply(vars, function(v) {
    tb <- table(category = as.character(fit$data[[v]]), group = g)
    if (pct) tb <- sweep(tb, 2, colSums(tb), "/") * 100
    round(tb, digits)
  })
  names(tabs) <- vars
  if (!long) return(tabs)
  do.call(rbind, lapply(vars, function(v) {
    tb <- tabs[[v]]
    data.frame(Variable = v, Category = rownames(tb), as.data.frame.matrix(tb),
               check.names = FALSE, row.names = NULL)
  }))
}

#' Row-level hierarchical distribution: segments across group x cluster.
#'
#' The nested breakdown of how the segments (rows) distribute across the
#' supplementary grouping and the clusters, with all three share denominators:
#' within-group (rows sum 100 per group), within-cluster (per cluster), of-total.
#'
#' @param fit an mca_fit with a `group`.
#' @return a long data.frame (group, cluster, n, pct_within_group, pct_within_cluster, pct_of_total).
#' @export
mca_distribution <- function(fit) {
  if (is.null(fit$group)) stop("no grouping available (fit was built without `group`)")
  tb <- table(group = fit$group, cluster = fit$clusters); N <- sum(tb)
  long <- expand.grid(group = rownames(tb), cluster = colnames(tb), stringsAsFactors = FALSE)
  long$n                  <- as.vector(tb)
  long$pct_within_group   <- as.vector(round(100 * prop.table(tb, 1), 1))
  long$pct_within_cluster <- as.vector(round(100 * prop.table(tb, 2), 1))
  long$pct_of_total       <- as.vector(round(100 * tb / N, 1))
  long[order(long$group, long$cluster), ]
}

#' Ward (HCPC) dendrogram of the segments, with the k-cluster cut drawn.
#'
#' Recomputes the Ward.D2 tree on the retained MCA dimensions (deterministic,
#' identical to the clustering in `mca_run`) and boxes the k clusters.
#'
#' @param fit an mca_fit; @param k clusters to box (default the fitted k);
#' @param palette cluster colours; @param main title.
#' @return (invisibly) the hclust object.
#' @export
plot_dendrogram <- function(fit, k = fit$call$k, palette = NULL, bw = FALSE, main = "") {
  W  <- fit$row_coords[, 1:fit$ndim, drop = FALSE]
  hc <- hclust(dist(W), "ward.D2")
  K  <- nlevels(fit$clusters)
  if (bw) palette <- grDevices::grey.colors(K, start = 0.2, end = 0.65)      # B&W: distinct greys
  else if (is.null(palette)) palette <- c("#1b9e77","#2c7fb8","#d95f02","#7570b3","#e7298a","#66a61e")[1:K]
  op <- par(mar = c(1, 4.2, 2, 1), lwd = if (bw) 1 else 1); on.exit(par(op))
  plot(hc, labels = FALSE, hang = -1, main = main, sub = "", xlab = "",
       ylab = "Height (Ward D2)")
  stats::rect.hclust(hc, k = k, border = palette)
  invisible(hc)
}
