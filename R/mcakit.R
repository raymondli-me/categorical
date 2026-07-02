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
                    vocab = NULL, cluster_labels = NULL, anchors = NULL, dedup = FALSE, seed = 2026) {
  stopifnot(is.data.frame(data), all(active %in% names(data)))
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
