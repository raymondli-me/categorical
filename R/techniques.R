# ============================================================================
# Methodology registry: a generic knowledge base of the techniques implemented
# here. Each entry carries math (formula), algorithm, inputs/outputs, a symbol
# glossary, the actual package function used, and ASPECT-LEVEL citations (which
# source, which page/chapter, grounding which specific claim) -> a shared
# bibliography (APA7 + BibTeX). Pure data; no project-specific content.
# ============================================================================

# annotated citation: which source, where (page/chapter), grounding what aspect
rf <- function(key, loc = "", aspect = "") list(key = key, loc = loc, aspect = aspect)

.MCA_BIB <- list(
  abdi2007   = list(cite="Abdi & Valentin, 2007", apa="Abdi, H., & Valentin, D. (2007). Multiple correspondence analysis. In N. Salkind (Ed.), Encyclopedia of measurement and statistics. Sage.",
                    bib="@incollection{abdi2007,author={Abdi, Herv\\'e and Valentin, Dominique},title={Multiple Correspondence Analysis},booktitle={Encyclopedia of Measurement and Statistics},editor={Salkind, Neil},publisher={Sage},year={2007}}"),
  leroux2010 = list(cite="Le Roux & Rouanet, 2010", apa="Le Roux, B., & Rouanet, H. (2010). Multiple correspondence analysis. SAGE. https://doi.org/10.4135/9781412993906",
                    bib="@book{leroux2010,author={Le Roux, Brigitte and Rouanet, Henry},title={Multiple Correspondence Analysis},publisher={SAGE},year={2010},doi={10.4135/9781412993906}}", url="https://doi.org/10.4135/9781412993906"),
  greenacre2006 = list(cite="Greenacre & Blasius, 2006", apa="Greenacre, M., & Blasius, J. (Eds.). (2006). Multiple correspondence analysis and related methods. Chapman & Hall/CRC.",
                    bib="@book{greenacre2006,editor={Greenacre, Michael and Blasius, J\\\"org},title={Multiple Correspondence Analysis and Related Methods},publisher={Chapman \\& Hall/CRC},year={2006}}"),
  husson2017 = list(cite="Husson et al., 2017", apa="Husson, F., Lê, S., & Pagès, J. (2017). Exploratory multivariate analysis by example using R (2nd ed.). CRC Press.",
                    bib="@book{husson2017,author={Husson, Fran\\c{c}ois and L\\^e, S\\'ebastien and Pag\\`es, J\\'er\\^ome},title={Exploratory Multivariate Analysis by Example Using R},edition={2},publisher={CRC Press},year={2017}}"),
  le2008     = list(cite="Lê et al., 2008", apa="Lê, S., Josse, J., & Husson, F. (2008). FactoMineR: An R package for multivariate analysis. Journal of Statistical Software, 25(1), 1–18.",
                    bib="@article{le2008,author={L\\^e, S\\'ebastien and Josse, Julie and Husson, Fran\\c{c}ois},title={{FactoMineR}: An {R} Package for Multivariate Analysis},journal={Journal of Statistical Software},volume={25},number={1},pages={1--18},year={2008}}", url="https://doi.org/10.18637/jss.v025.i01"),
  ward1963   = list(cite="Ward, 1963", apa="Ward, J. H. (1963). Hierarchical grouping to optimize an objective function. Journal of the American Statistical Association, 58(301), 236–244.",
                    bib="@article{ward1963,author={Ward, Joe H.},title={Hierarchical Grouping to Optimize an Objective Function},journal={Journal of the American Statistical Association},volume={58},number={301},pages={236--244},year={1963}}", url="https://doi.org/10.1080/01621459.1963.10500845"),
  murtagh2014= list(cite="Murtagh & Legendre, 2014", apa="Murtagh, F., & Legendre, P. (2014). Ward’s hierarchical agglomerative clustering method: Which algorithms implement Ward’s criterion? Journal of Classification, 31(3), 274–295.",
                    bib="@article{murtagh2014,author={Murtagh, Fionn and Legendre, Pierre},title={Ward's Hierarchical Agglomerative Clustering Method},journal={Journal of Classification},volume={31},number={3},pages={274--295},year={2014}}", url="https://doi.org/10.1007/s00357-014-9161-z"),
  macqueen1967=list(cite="MacQueen, 1967", apa="MacQueen, J. (1967). Some methods for classification and analysis of multivariate observations. Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability, 1, 281–297.",
                    bib="@inproceedings{macqueen1967,author={MacQueen, James},title={Some Methods for Classification and Analysis of Multivariate Observations},booktitle={Proc. Fifth Berkeley Symp. on Math. Statist. and Prob.},volume={1},pages={281--297},year={1967}}"),
  hartigan1979=list(cite="Hartigan & Wong, 1979", apa="Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means clustering algorithm. Journal of the Royal Statistical Society C, 28(1), 100–108.",
                    bib="@article{hartigan1979,author={Hartigan, J. A. and Wong, M. A.},title={Algorithm {AS} 136: A k-Means Clustering Algorithm},journal={Journal of the Royal Statistical Society. Series C},volume={28},number={1},pages={100--108},year={1979}}", url="https://doi.org/10.2307/2346830"),
  lebart1984 = list(cite="Lebart et al., 1984", apa="Lebart, L., Morineau, A., & Warwick, K. M. (1984). Multivariate descriptive statistical analysis. Wiley.",
                    bib="@book{lebart1984,author={Lebart, Ludovic and Morineau, Alain and Warwick, Kenneth M.},title={Multivariate Descriptive Statistical Analysis},publisher={Wiley},year={1984}}"),
  haberman1973=list(cite="Haberman, 1973", apa="Haberman, S. J. (1973). The analysis of residuals in cross-classified tables. Biometrics, 29(1), 205–220.",
                    bib="@article{haberman1973,author={Haberman, Shelby J.},title={The Analysis of Residuals in Cross-Classified Tables},journal={Biometrics},volume={29},number={1},pages={205--220},year={1973}}", url="https://doi.org/10.2307/2529686"),
  hubert1985 = list(cite="Hubert & Arabie, 1985", apa="Hubert, L., & Arabie, P. (1985). Comparing partitions. Journal of Classification, 2(1), 193–218.",
                    bib="@article{hubert1985,author={Hubert, Lawrence and Arabie, Phipps},title={Comparing Partitions},journal={Journal of Classification},volume={2},number={1},pages={193--218},year={1985}}", url="https://doi.org/10.1007/BF01908075"),
  rand1971   = list(cite="Rand, 1971", apa="Rand, W. M. (1971). Objective criteria for the evaluation of clustering methods. Journal of the American Statistical Association, 66(336), 846–850.",
                    bib="@article{rand1971,author={Rand, William M.},title={Objective Criteria for the Evaluation of Clustering Methods},journal={Journal of the American Statistical Association},volume={66},number={336},pages={846--850},year={1971}}", url="https://doi.org/10.1080/01621459.1971.10482356"),
  efron1993  = list(cite="Efron & Tibshirani, 1993", apa="Efron, B., & Tibshirani, R. J. (1993). An introduction to the bootstrap. Chapman & Hall.",
                    bib="@book{efron1993,author={Efron, Bradley and Tibshirani, Robert J.},title={An Introduction to the Bootstrap},publisher={Chapman \\& Hall},year={1993}}", url="https://doi.org/10.1201/9780429246593")
)

.MCA_TECH <- list(
  mca = list(
    name="Multiple Correspondence Analysis (MCA)",
    brief="SVD of the standardized residual matrix of the indicator (disjunctive) matrix; positions categories and individuals by co-occurrence.",
    interpretation="Read positions relationally, not absolutely: categories near each other co-occur across segments; opposite quadrants mark mutually-exclusive profiles; a right angle from the origin suggests independence. Distance from the origin marks how distinctive a category is, and axes only mean something once read through their contributing poles.",
    formula="S = D_r^{-1/2} (P - r c^T) D_c^{-1/2};\\ \\ SVD: S = U \\Lambda^{1/2} V^T;\\ \\ F = D_r^{-1/2} U \\Lambda^{1/2},\\ G = D_c^{-1/2} V \\Lambda^{1/2}",
    algorithm=c("Build indicator matrix Z (n x J), one-hot of Q variables.","Correspondence matrix P = Z/N, row masses r, column masses c.","Standardized residuals S = D_r^{-1/2}(P - r c^T) D_c^{-1/2}.","SVD of S; eigenvalues = principal inertias.","Principal coordinates F (rows), G (columns)."),
    inputs="A data.frame of Q categorical variables.",
    optional_inputs="min_n (specific MCA), supplementary grouping, sign anchors, vocab recode.",
    outputs="Eigenvalues/inertia, category coordinates, individual coordinates.",
    optional_outputs="Contributions, squared cosines, Benzecri-adjusted inertia.",
    glossary=c("Z"="n x J indicator (disjunctive) matrix (0/1)","N"="grand total = n*Q","P"="correspondence matrix Z/N","r_i"="row mass = 1/n (constant)","c_j"="column mass = n_j/N","S"="standardized residual matrix","lambda_k"="eigenvalue (principal inertia) of axis k","F"="individual principal coordinates","G"="category principal coordinates"),
    code_fn="mca_run", code="fit <- mca_run(data, active = ACTIVE, group = \"period\", min_n = 10, k = 4)",
    cites=list(rf("abdi2007","","step-by-step derivation of MCA via SVD"),
               rf("leroux2010","chs. 1-3","MCA framework, chi-square metric, cloud geometry"),
               rf("greenacre2006","p. 12","principal inertias, factor loadings, communalities"))),

  benzecri = list(
    name="Benzecri-adjusted inertia",
    brief="Corrects MCA's pessimistic eigenvalues for the block-diagonal inflation of the indicator matrix, giving interpretable variance percentages.",
    interpretation="The raw MCA percentages badly understate the structure because the indicator coding inflates the denominator; report the Benzecri-adjusted percentages. Treat them as the share of interpretable inertia per axis, and use their cumulative value to justify how many dimensions to retain.",
    formula="\\lambda_k^{adj} = \\left(\\tfrac{Q}{Q-1}\\right)^2 \\left(\\lambda_k - \\tfrac{1}{Q}\\right)^2,\\quad \\lambda_k > 1/Q;\\ \\text{else } 0",
    algorithm=c("Keep eigenvalues above 1/Q.","Apply (Q/(Q-1))^2 (lambda - 1/Q)^2.","Re-express as % of the adjusted total."),
    inputs="Raw MCA eigenvalues, number of active variables Q.", optional_inputs=NA,
    outputs="Adjusted % inertia per dimension.", optional_outputs=NA,
    glossary=c("Q"="number of active variables","lambda_k"="raw eigenvalue","lambda_k^{adj}"="Benzecri-adjusted eigenvalue"),
    code_fn="mca_run", code="fit$inertia$pct_benzecri",
    cites=list(rf("greenacre2006","ch. 3","rationale and formula for the Benzecri/Greenacre adjustment"),
               rf("leroux2010","","threshold 1/Q for discarding trivial axes"))),

  contrib = list(
    name="Contributions (CTR)",
    brief="How much a category contributes to an axis's inertia; contributions sum to 1 within each dimension.",
    interpretation="Use contributions to see what BUILDS an axis: a category with CTR well above the average 1/J is a defining pole of that dimension. Because contributions sum to 1 down each axis, read them column-wise to answer 'which categories make this axis?'.",
    formula="\\mathrm{CTR}_{jk} = \\frac{m_j\\,G_{jk}^2}{\\lambda_k},\\qquad \\sum_j \\mathrm{CTR}_{jk} = 1",
    algorithm=c("Square each category coordinate on axis k.","Weight by category mass.","Divide by the axis eigenvalue."),
    inputs="Category coordinates, masses, eigenvalues.", optional_inputs=NA,
    outputs="Contribution of each category to each axis.", optional_outputs=NA,
    glossary=c("m_j"="category mass","G_{jk}"="coordinate of category j on axis k","lambda_k"="eigenvalue of axis k"),
    code_fn="mca_master", code="mca_master(fit)[, c(\"category\",\"ctr_D1\",\"ctr_D2\",\"ctr_D3\")]",
    cites=list(rf("greenacre2006","p. 12","contributions as communalities / share of axis inertia"),
               rf("abdi2007","","contribution formula m*F^2/lambda"))),

  cos2 = list(
    name="Squared cosine (cos2, quality of representation)",
    brief="Proportion of a point's total inertia captured by an axis (an R^2-type fit); squared cosines sum to 1 across axes.",
    interpretation="Use cos2 to see how WELL a point is shown on an axis (its quality of representation): near 1 means the axis captures almost all of that category's variance so its position is trustworthy; a low value means the category really lives on other dimensions. Read row-wise (sums to 1 across axes).",
    formula="\\cos^2_{jk} = \\frac{G_{jk}^2}{\\sum_m G_{jm}^2} = \\frac{G_{jk}^2}{d_j^2}",
    algorithm=c("Squared distance of the point to the origin.","Divide the squared coordinate on axis k by that total."),
    inputs="Category (or individual) coordinates.", optional_inputs=NA,
    outputs="Quality of representation on each axis.", optional_outputs=NA,
    glossary=c("G_{jk}"="coordinate on axis k","d_j^2"="squared distance of point j to origin"),
    code_fn="mca_master", code="mca_master(fit)[, c(\"category\",\"cos2_D1\",\"cos2_D2\",\"cos2_D3\")]",
    cites=list(rf("abdi2007","","squared cosine as quality of representation"),
               rf("greenacre2006","","cos2 relation to the chi-square distance decomposition"))),

  hcpc = list(
    name="Hierarchical Clustering on Principal Components (HCPC): Ward + k-means",
    brief="Ward agglomerative clustering on retained coordinates, consolidated by k-means; minimizes within-cluster inertia.",
    interpretation="Clusters are the configurations actually occupied in the space; the between/within split (Huygens) says how much structure the partition captures, so a high between-inertia percentage means well-separated clusters. Dendrogram height at the cut shows how distinct the groups are.",
    formula="\\Delta(A,B) = \\frac{m_A m_B}{m_A + m_B}\\lVert \\bar{x}_A - \\bar{x}_B \\rVert^2;\\quad I_{total} = I_{between} + I_{within}",
    algorithm=c("Ward.D2 on the retained principal coordinates.","Cut the dendrogram at k.","k-means consolidation initialized at the Ward centroids.","Choose k by the relative inertia-gain."),
    inputs="Retained individual coordinates, k.", optional_inputs="ndim, inertia-gain k selection.",
    outputs="Cluster assignment; between/within inertia; dendrogram.", optional_outputs="Inertia-gain table; v-test characterization.",
    glossary=c("m_A"="mass/size of cluster A","\\bar{x}_A"="centroid of cluster A","I_{between}"="between-cluster inertia","I_{within}"="within-cluster inertia"),
    code_fn=c("mca_run","plot_dendrogram"), code="fit <- mca_run(data, ACTIVE, k = 4, ndim = 3); plot_dendrogram(fit)",
    cites=list(rf("ward1963","","the minimum-variance agglomeration criterion"),
               rf("murtagh2014","","which linkage (ward.D2) actually implements Ward's 1963 criterion"),
               rf("husson2017","","the HCPC procedure: cluster on components, consolidate by k-means"),
               rf("lebart1984","","tradition of clustering on MCA factor coordinates"))),

  kmeans = list(
    name="k-means consolidation",
    brief="Minimizes within-cluster sum of squares by alternating assignment and centroid updates; polishes the Ward partition.",
    interpretation="Consolidation only REFINES the Ward partition (same objective via Huygens), so expect small movements at cluster boundaries, not a different solution; large movement signals an unstable Ward cut.",
    formula="\\min \\sum_{g} \\sum_{i \\in C_g} \\lVert x_i - \\mu_g \\rVert^2,\\qquad \\mu_g = \\tfrac{1}{|C_g|}\\sum_{i \\in C_g} x_i",
    algorithm=c("Initialize centroids at the Ward means.","Assign each point to its nearest centroid.","Recompute centroids as member means.","Repeat to convergence."),
    inputs="Individual coordinates, initial centroids.", optional_inputs=NA,
    outputs="Consolidated cluster assignment.", optional_outputs=NA,
    glossary=c("C_g"="cluster g","\\mu_g"="centroid of cluster g","x_i"="coordinates of individual i"),
    code_fn="mca_run", code="# inside mca_run(): kmeans(W, centers = ward_centroids)",
    cites=list(rf("macqueen1967","","the k-means objective and iterative relocation"),
               rf("hartigan1979","","the AS 136 algorithm used by R's kmeans() default"))),

  vtest = list(
    name="v-test (cluster characterization)",
    brief="Standardized gap between a category's within-cluster and overall proportion (finite-population corrected); |v|>1.96 = characteristic.",
    interpretation="Rank categories within a cluster by v: v > 1.96 marks a category the cluster over-uses and v < -1.96 one it avoids, far more than chance. These top-v categories are the cluster's signature and how you name it.",
    formula="v = \\frac{p_{kg} - p_k}{\\sqrt{\\tfrac{p_k(1-p_k)}{n_g}\\cdot\\tfrac{N - n_g}{N - 1}}}",
    algorithm=c("Compare in-cluster to overall proportion for each category.","Standardize by the hypergeometric SE.","Rank categories by v per cluster."),
    inputs="Cluster assignment, indicator matrix.", optional_inputs=NA,
    outputs="v-test per category per cluster; top characteristic categories.", optional_outputs=NA,
    glossary=c("p_k"="overall proportion of category k","p_{kg}"="proportion of k inside cluster g","n_g"="cluster size","N"="total individuals"),
    code_fn=c("mca_top_cluster","mca_residuals"), code="mca_top_cluster(fit)",
    cites=list(rf("husson2017","","v-test definition and use to describe clusters (catdes)"),
               rf("lebart1984","","test-value (valeur-test) for category description"))),

  haberman = list(
    name="Haberman adjusted standardized residuals",
    brief="Chi-square cell residuals rescaled by their asymptotic variance to be ~N(0,1); tests which categories a group over-/under-uses.",
    interpretation="Read each cell as a z-score of group change: d > +1.96 = the period over-uses that category, d < -1.96 = under-uses it, relative to independence. Because the adjustment inflates the raw residual, some cells cross significance that a naive Pearson residual would miss.",
    formula="d_{ij} = \\frac{O_{ij} - E_{ij}}{\\sqrt{E_{ij}(1 - n_{i\\cdot}/N)(1 - n_{\\cdot j}/N)}},\\quad E_{ij} = \\frac{n_{i\\cdot} n_{\\cdot j}}{N}",
    algorithm=c("Cross-tabulate group x category.","Expected counts under independence.","Pearson residual / sqrt(Haberman variance).","Compare |d| to 1.96."),
    inputs="Group labels, category indicator.", optional_inputs="Pearson (unadjusted) form.",
    outputs="Adjusted residual per category per group.", optional_outputs=NA,
    glossary=c("O_{ij}"="observed count","E_{ij}"="expected under independence","n_{i\\cdot}"="group total","n_{\\cdot j}"="category total","N"="grand total"),
    code_fn=c("mca_residuals","mca_top_group"), code="mca_top_group(fit)",
    cites=list(rf("haberman1973","p. 207, eqs. 4 & 6","the asymptotic variance (1 - n_i/N)(1 - n_j/N) that defines the adjusted residual"))),

  typicality = list(
    name="Geometric typicality test",
    brief="Tests whether a subgroup's mean position on an axis is atypical vs the whole cloud, referenced to the axis inertia; |Z|>1.96 = atypical.",
    interpretation="Ask whether a subgroup sits atypically far along an axis relative to the whole cloud: |Z| > 1.96 means its mean on that dimension is not chance. It localizes WHERE (which axis) a group is distinctive.",
    formula="Z = \\frac{\\bar{y}_p}{\\sqrt{V}},\\qquad V = \\frac{N - n_p}{n_p (N - 1)}\\,\\lambda_d,\\qquad Z \\sim N(0,1)",
    algorithm=c("Subgroup mean coordinate on axis d.","Typicality variance from eigenvalue and subgroup size.","Standardize."),
    inputs="Individual coordinates, subgroup labels, eigenvalues.", optional_inputs=NA,
    outputs="Typicality Z per subgroup per axis.", optional_outputs=NA,
    glossary=c("\\bar{y}_p"="subgroup mean on the axis","n_p"="subgroup size","N"="total","\\lambda_d"="eigenvalue of axis d"),
    code_fn="mca_typicality", code="mca_typicality(fit)",
    cites=list(rf("leroux2010","ch. 5.1","the typicality problem and the Z = ybar/sqrt(V) test for a subcloud mean"))),

  eta_perm = list(
    name="Correlation ratio (eta^2) with permutation test",
    brief="Inertia explained by a grouping (between/total), tested by permutation since MCA coordinates are non-normal; reports eta^2, F, permutation p.",
    interpretation="eta^2 is the share of the cloud's inertia explained by the grouping, an effect size not just a p-value. A small eta^2 with a significant permutation p (common at large n) means the grouping matters but only mildly reshapes the space: reweighting within a stable structure, not reorganization of it.",
    formula="\\eta^2 = \\frac{\\sum_g n_g \\lVert \\bar{x}_g - \\bar{x}\\rVert^2}{\\sum_i \\lVert x_i - \\bar{x}\\rVert^2};\\ F = \\frac{\\eta^2/(G-1)}{(1-\\eta^2)/(N-G)};\\ p = \\frac{1 + \\#\\{\\eta^{2*}_b \\ge \\eta^2\\}}{B+1}",
    algorithm=c("Between/total inertia -> eta^2.","Form F.","Permute labels B times; count exceedances for p."),
    inputs="Individual coordinates, grouping.", optional_inputs="Number of permutations B.",
    outputs="eta^2, F, permutation p (overall and per axis).", optional_outputs=NA,
    glossary=c("\\bar{x}_g"="group centroid","\\bar{x}"="grand mean","n_g"="group size","G"="number of groups","B"="permutations"),
    code_fn="mca_eta", code="mca_eta(fit, B_perm = 9999)",
    cites=list(rf("leroux2010","ch. 5.2","homogeneity test (N-1)eta^2 ~ chi^2 and its permutation version for non-normal coordinates"))),

  ellipses = list(
    name="Bootstrap confidence ellipses",
    brief="Nonparametric 95% regions for each group's mean, from the covariance of bootstrapped centroids; overlap by Mahalanobis boundary test.",
    interpretation="Overlap is the decision rule: if two groups' 95% ellipses overlap, their mean positions are not distinguishable; if one ellipse contains the other's centre, they clearly differ. Ellipse size reflects sampling uncertainty in the centroid, not the spread of the group.",
    formula="\\hat{\\mu} = \\tfrac{1}{B}\\sum_b \\bar{x}^{*b},\\ \\hat{S} = \\mathrm{cov}(\\bar{x}^{*b});\\ (y-\\hat\\mu)^T \\hat{S}^{-1} (y-\\hat\\mu) \\le \\chi^2_{2,0.95}",
    algorithm=c("Resample each group with replacement B times.","Take the bootstrap centroid.","Mean and covariance of centroids.","Draw the 95% ellipse; test overlap via Mahalanobis distance."),
    inputs="Individual coordinates, grouping, dimension pair.", optional_inputs="topk (signature ellipse), B.",
    outputs="Ellipse parameters (centre, semi-axes, angle); pairwise overlap.", optional_outputs=NA,
    glossary=c("\\bar{x}^{*b}"="bootstrap centroid b","\\hat\\mu"="mean of bootstrap centroids","\\hat S"="covariance of centroids","\\chi^2_{2,0.95}"="chi-square 95% quantile, 2 df"),
    code_fn="mca_ellipses", code="mca_ellipses(fit, dims = c(1,2), topk = NULL)",
    cites=list(rf("leroux2010","ch. 5.3","confidence ellipses for subcloud mean points in the principal plane"),
               rf("efron1993","","the bootstrap resampling that produces the centroid distribution"))),

  ari = list(
    name="Adjusted Rand Index (ARI)",
    brief="Chance-corrected agreement between two partitions; 1 = identical, ~0 = chance, < 0 = worse than chance.",
    interpretation="Read ARI as how far from chance toward identical two clusterings are: 1 identical, 0 chance, and e.g. 0.80 = 80 percent of the achievable-above-chance agreement. Use it to show a partition is robust to an analytic choice.",
    formula="\\mathrm{ARI} = \\frac{\\sum_{ij}\\binom{n_{ij}}{2} - \\frac{[\\sum_i\\binom{a_i}{2}][\\sum_j\\binom{b_j}{2}]}{\\binom{n}{2}}}{\\tfrac{1}{2}[\\sum_i\\binom{a_i}{2} + \\sum_j\\binom{b_j}{2}] - \\frac{[\\sum_i\\binom{a_i}{2}][\\sum_j\\binom{b_j}{2}]}{\\binom{n}{2}}}",
    algorithm=c("Cross-tabulate the two partitions.","Observed agreeing pairs minus expected-by-chance.","Normalize by max-minus-chance."),
    inputs="Two equal-length cluster-label vectors.", optional_inputs=NA,
    outputs="Agreement value in [-0.5, 1].", optional_outputs=NA,
    glossary=c("n_{ij}"="objects in cluster i of A and j of B","a_i"="row total","b_j"="column total","n"="total objects"),
    code_fn="mca_ari", code="mca_ari(fitA$clusters, fitB$clusters)",
    cites=list(rf("hubert1985","","the chance-corrected adjustment that defines the ARI"),
               rf("rand1971","","the underlying (unadjusted) Rand index of pairwise agreement"))),

  specific_mca = list(
    name="Specific MCA",
    brief="Excludes rare/junk categories from the axis computation (full margins kept) so they don't warp the map; they can be projected as supplementary.",
    interpretation="Compare a specific-MCA solution to the full one to show rare categories were not distorting the map; near-identical eigenvalues/coordinates and a high ARI mean the threshold choice is inconsequential. Excluded categories can still be read as supplementary points.",
    formula="d^2(x,y) = \\sum_j \\tfrac{1}{c_j}(x_j - y_j)^2\\ \\Rightarrow\\ \\text{drop passive } j;\\quad F_{is} = \\tfrac{1}{\\sqrt{\\lambda_s}}\\tfrac{1}{Q}\\sum_{j\\in J_i} G_{js}",
    algorithm=c("Flag categories below a frequency threshold (n<10 or 5%).","SVD only the active columns, using full margins.","Optionally project excluded categories via the transition formulas."),
    inputs="Indicator matrix, exclusion threshold min_n.", optional_inputs="Supplementary projection.",
    outputs="MCA restricted to active categories.", optional_outputs="Supplementary coordinates.",
    glossary=c("c_j"="column mass of category j","J_i"="categories chosen by individual i","lambda_s"="eigenvalue of axis s"),
    code_fn="mca_run", code="mca_run(data, ACTIVE, min_n = 10)",
    cites=list(rf("leroux2010","ch. 3.3, p. 41","the 5% rule, passive categories, and the transition formulas for supplementary projection"),
               rf("greenacre2006","p. 60","how a low-mass category exaggerates chi-square distance and warps the map"))))

#' Names of the documented techniques (keyed).
#' @export
mca_techniques <- function() vapply(.MCA_TECH, `[[`, character(1), "name")

#' Full registry entry for one technique.
#' @export
mca_technique <- function(key) .MCA_TECH[[key]]

.tech_keys <- function(t) vapply(t$cites, `[[`, character(1), "key")

#' Brief specification table (one row per technique).
#' @export
mca_techniques_table <- function(keys = names(.MCA_TECH)) {
  do.call(rbind, lapply(keys, function(k) { t <- .MCA_TECH[[k]]
    data.frame(Technique = t$name, Brief = t$brief, Formula = t$formula, Inputs = t$inputs,
               Outputs = t$outputs, Code = t$code,
               Citations = paste(vapply(.tech_keys(t), function(r) .MCA_BIB[[r]]$cite, character(1)), collapse = "; "),
               check.names = FALSE, row.names = NULL) }))
}

#' Aspect-level citations: which source (page) grounds which part, with a reading pointer and link.
#' @export
mca_citations <- function(keys = names(.MCA_TECH)) {
  do.call(rbind, lapply(keys, function(k) { t <- .MCA_TECH[[k]]
    do.call(rbind, lapply(t$cites, function(c) {
      src <- .MCA_BIB[[c$key]]$cite; loc <- c$loc
      data.frame(Technique = t$name, Aspect = c$aspect, Source = src, Locator = loc,
                 Reading = paste0("See ", src, if (nzchar(loc)) paste0(", ", loc) else "", " for ", c$aspect, "."),
                 Link = .cite_url(c$key), check.names = FALSE, row.names = NULL) })) }))
}

#' Symbol glossary aggregated across techniques.
#' @export
mca_glossary <- function(keys = names(.MCA_TECH)) {
  g <- do.call(rbind, lapply(keys, function(k) { t <- .MCA_TECH[[k]]
    if (is.null(t$glossary)) return(NULL)
    data.frame(Symbol = names(t$glossary), Meaning = unname(t$glossary),
               Technique = t$name, check.names = FALSE, row.names = NULL) }))
  g[!duplicated(g$Symbol), ]
}

#' Code table: package function + example snippet per technique.
#' @export
mca_code_table <- function(keys = names(.MCA_TECH)) {
  do.call(rbind, lapply(keys, function(k) { t <- .MCA_TECH[[k]]
    data.frame(Technique = t$name, Function = paste(t$code_fn, collapse = ", "),
               Example = t$code, check.names = FALSE, row.names = NULL) }))
}

#' Resolve a citation link: stored DOI/URL, else a Google Scholar search.
.cite_url <- function(key) { u <- .MCA_BIB[[key]]$url
  if (is.null(u) || !nzchar(u)) paste0("https://scholar.google.com/scholar?q=",
    utils::URLencode(.MCA_BIB[[key]]$cite, reserved = TRUE)) else u }

#' Browsable/selectable reference list: key, short cite, APA7, link.
#' @export
mca_reflist <- function() {
  r <- names(.MCA_BIB)
  data.frame(Key = r,
             Citation = vapply(r, function(x) .MCA_BIB[[x]]$cite, character(1)),
             APA = vapply(r, function(x) .MCA_BIB[[x]]$apa, character(1)),
             Link = vapply(r, .cite_url, character(1)), check.names = FALSE, row.names = NULL)
}

#' Bibliography (APA7 or BibTeX). Select by technique `keys` OR directly by reference `refs`.
#' @param keys technique keys; @param refs explicit reference keys (overrides keys);
#' @param format "apa" or "bibtex"; @param include_url append the link to APA entries.
#' @export
mca_bibliography <- function(keys = names(.MCA_TECH), refs = NULL, format = c("apa", "bibtex"), include_url = TRUE) {
  format <- match.arg(format)
  if (is.null(refs)) refs <- unique(unlist(lapply(.MCA_TECH[keys], .tech_keys)))
  refs <- intersect(refs, names(.MCA_BIB))
  if (format == "bibtex") return(unname(vapply(refs, function(r) .MCA_BIB[[r]]$bib, character(1))))
  out <- vapply(refs, function(r) { a <- .MCA_BIB[[r]]$apa
    if (include_url && !grepl("http", a, fixed = TRUE)) a <- paste0(a, " ", .cite_url(r)); a }, character(1))
  sort(unname(out))
}

# ---- additional bibliography ----
.MCA_BIB <- c(.MCA_BIB, list(
  dillon1984 = list(cite="Dillon & Goldstein, 1984", apa="Dillon, W. R., & Goldstein, M. (1984). Multivariate analysis: Methods and applications. Wiley.",
                    bib="@book{dillon1984,author={Dillon, William R. and Goldstein, Matthew},title={Multivariate Analysis: Methods and Applications},publisher={Wiley},year={1984}}"),
  agresti2013= list(cite="Agresti, 2013", apa="Agresti, A. (2013). Categorical data analysis (3rd ed.). Wiley.",
                    bib="@book{agresti2013,author={Agresti, Alan},title={Categorical Data Analysis},edition={3},publisher={Wiley},year={2013}}"),
  benzecri1979=list(cite="Benzecri, 1979", apa="Benzécri, J.-P. (1979). Sur le calcul des taux d'inertie dans l'analyse d'un questionnaire. Cahiers de l'Analyse des Données, 4(3), 377–378.",
                    bib="@article{benzecri1979,author={Benz\\'ecri, Jean-Paul},title={Sur le calcul des taux d'inertie dans l'analyse d'un questionnaire},journal={Cahiers de l'Analyse des Donn\\'ees},volume={4},number={3},pages={377--378},year={1979}}")))

# ---- additional techniques ----
.MCA_TECH <- c(.MCA_TECH, list(
  indicator = list(
    name="Indicator (disjunctive) matrix and the Burt table",
    brief="The 0/1 one-hot encoding of Q categorical variables that MCA decomposes; the Burt matrix B = Z^T Z is the table of all two-way cross-tabs.",
    interpretation="Everything downstream is a transformation of Z: masses are its margins and inertia is its departure from independence. Its block-diagonal structure (each variable perfectly predicts itself) is exactly what inflates the raw eigenvalues and motivates the Benzecri correction.",
    formula="Z \\in \\{0,1\\}^{n\\times J},\\ \\sum_j Z_{ij}=Q;\\quad B = Z^T Z",
    algorithm=c("One-hot each of the Q variables.","Concatenate to the n x J indicator matrix Z.","Optionally form the Burt matrix B = Z^T Z."),
    inputs="Q categorical variables.", optional_inputs=NA,
    outputs="Indicator matrix Z (and Burt matrix B).", optional_outputs=NA,
    glossary=c("Z"="indicator (disjunctive) matrix","Q"="active variables","J"="total categories","B"="Burt matrix Z^T Z"),
    code_fn="mca_run", code="fit$Z   # the active indicator matrix",
    cites=list(rf("leroux2010","ch. 2","indicator vs Burt coding of the questionnaire"), rf("greenacre2006","","disjunctive coding and its consequences"))),

  chisq_distance = list(
    name="Chi-square distance",
    brief="The metric MCA uses between profiles: squared category-wise differences weighted by the inverse of the column mass, so rare categories weigh heavily.",
    interpretation="This weighting is why rare categories can dominate the map: a category present in 2 percent of segments contributes about 1/0.02 = 50x more to a distance than one at 50 percent. That sensitivity is the reason for the 5 percent / n<10 rule and specific MCA.",
    formula="d^2(x,y) = \\sum_{j=1}^{J} \\frac{1}{c_j}(x_j - y_j)^2",
    algorithm=c("Take two profiles.","Square their category-wise differences.","Weight each by 1/c_j and sum."),
    inputs="Two profiles, column masses.", optional_inputs=NA,
    outputs="Squared distance between profiles.", optional_outputs=NA,
    glossary=c("c_j"="column mass of category j","x_j"="profile x entry for j","y_j"="profile y entry for j"),
    code_fn="mca_run", code="# implicit in the SVD of the standardized residual matrix",
    cites=list(rf("greenacre2006","p. 60","how a low-mass category exaggerates the chi-square distance"), rf("leroux2010","","chi-square metric on profiles"))),

  total_inertia = list(
    name="Total inertia",
    brief="The total variance of the table: the sum of eigenvalues, equal to chi-square / N; measures overall departure from independence.",
    interpretation="Inertia here IS variance-as-deviation-from-independence: if rows and columns were unrelated all points would collapse to the origin and inertia would be 0. Each eigenvalue is the share carried by one axis, and percentages are shares of this total.",
    formula="\\mathrm{Inertia} = \\sum_k \\lambda_k = \\sum_{ij} s_{ij}^2 = \\chi^2/N",
    algorithm=c("Sum the squared standardized residuals (trace of S S^T).","Equivalently, sum the eigenvalues."),
    inputs="Standardized residual matrix / eigenvalues.", optional_inputs=NA,
    outputs="Total inertia; per-axis shares.", optional_outputs=NA,
    glossary=c("s_{ij}"="standardized residual","lambda_k"="eigenvalue","chi^2"="Pearson chi-square of the table","N"="grand total"),
    code_fn="mca_run", code="sum(fit$lam)   # total inertia",
    cites=list(rf("greenacre2006","p. 12","principal inertias as the variance decomposition"), rf("leroux2010","","total inertia = chi-square / N"))),

  huygens = list(
    name="Inertia decomposition (Huygens' theorem)",
    brief="Total inertia splits exactly into between-cluster and within-cluster inertia; the basis for why clustering explains anything.",
    interpretation="Because total inertia is fixed, maximizing between-cluster inertia is identical to minimizing within-cluster inertia, the shared objective of Ward and k-means. Report between-inertia percentage as the share of structure the partition captures (higher = tighter, better-separated clusters).",
    formula="I_{total} = I_{between} + I_{within};\\quad \\%_{between} = 100\\,I_{between}/I_{total}",
    algorithm=c("Grand centroid and total inertia.","Between-inertia = sum of size-weighted squared centroid-to-grand distances.","Within = total - between."),
    inputs="Coordinates, cluster assignment.", optional_inputs=NA,
    outputs="Between/within inertia and the between percentage.", optional_outputs=NA,
    glossary=c("I_{total}"="total inertia","I_{between}"="between-cluster inertia","I_{within}"="within-cluster inertia"),
    code_fn="mca_run", code="fit$between_inertia   # % of total inertia between clusters",
    cites=list(rf("dillon1984","","the Huygens inertia identity underlying cluster analysis"), rf("husson2017","","between/within decomposition in HCPC"))),

  inertia_gain = list(
    name="Number of clusters by inertia gain",
    brief="Chooses k where the relative gain in between-cluster inertia from adding a cluster drops off (an elbow on the dendrogram).",
    interpretation="Pick k at the largest gain before diminishing returns: a jump of, say, 15 percent from 3 to 4 clusters versus 5 percent from 4 to 5 argues for four. It is the clustering analogue of the scree elbow.",
    formula="\\mathrm{gain}(k) = \\%_{between}(k) - \\%_{between}(k-1)",
    algorithm=c("Cut the Ward tree at successive k.","Record between-inertia percentage at each k.","Choose k at the last large gain."),
    inputs="Ward hierarchy, coordinates.", optional_inputs=NA,
    outputs="Between-percentage and gain per k.", optional_outputs=NA,
    glossary=c("gain(k)"="rise in between-inertia % when going from k-1 to k clusters"),
    code_fn="mca_run", code="fit$gain_tab   # between-% and gain per k",
    cites=list(rf("husson2017","","the relative inertia-gain criterion for choosing k"))),

  signature = list(
    name="Group signature (residual-weighted top-k centroid)",
    brief="A descriptive locator placing a group at the residual-weighted mean of the categories it most over-uses; complements the plain centroid.",
    interpretation="Where the centroid is the group's literal average position (often near the origin), the signature exaggerates toward what makes the group distinctive, so the two together separate 'where a period sits' from 'what a period is about'. It is a descriptive summary, not an inferential test.",
    formula="\\mathbf{s}_p = \\frac{\\sum_{k\\in \\mathrm{Top}_p} e_{pk}\\,\\mathbf{g}_k}{\\sum_{k\\in \\mathrm{Top}_p} e_{pk}},\\quad e_{pk} = \\frac{O_{pk}-E_{pk}}{\\sqrt{E_{pk}}}",
    algorithm=c("Rank a group's categories by residual e_pk.","Take the top-k (e.g. 5).","Average their coordinates weighted by the positive residuals."),
    inputs="Indicator matrix, category coordinates, grouping, k.", optional_inputs="Bootstrap for a signature ellipse.",
    outputs="Signature position per group.", optional_outputs="Bootstrap covariance for a signature ellipse.",
    glossary=c("e_{pk}"="Pearson residual (weight)","g_k"="category coordinate","Top_p"="top-k over-used categories in group p"),
    code_fn=c("mca_ellipses","mca_export_fig3d"), code="mca_ellipses(fit, dims = c(1,2), topk = 5)   # signature-based ellipse",
    cites=list(rf("haberman1973","","the residual used as the over-representation weight"), rf("leroux2010","","mean points of subclouds in the cloud of individuals"))),

  sqrt_lambda = list(
    name="Category-scale rescaling and transition formulas",
    brief="Rescaling principal coordinates by 1/sqrt(lambda) gives standard (category-scale) coordinates; the transition formulas project rows from columns and vice versa, enabling supplementary points.",
    interpretation="Principal (sqrt-lambda-weighted) coordinates are for reading distances; standard (÷ sqrt-lambda) coordinates put rows and categories on a common footing so a supplementary point can be placed as the average of the active categories it carries, pushed outward by 1/sqrt(lambda). Use it to project excluded/rare categories without letting them shape the axes.",
    formula="F^{std} = F/\\sqrt{\\lambda};\\quad F_{is}=\\tfrac{1}{\\sqrt{\\lambda_s}}\\tfrac{1}{Q}\\sum_{j\\in J_i} G_{js};\\quad G_{js}=\\tfrac{1}{\\sqrt{\\lambda_s}}\\tfrac{1}{n_j}\\sum_{i\\in I_j} F_{is}",
    algorithm=c("Divide principal coordinates by sqrt(eigenvalue) per axis.","Place a supplementary point as the rescaled average of its active categories/individuals."),
    inputs="Principal coordinates, eigenvalues.", optional_inputs=NA,
    outputs="Standard coordinates; supplementary projections.", optional_outputs=NA,
    glossary=c("lambda_s"="eigenvalue of axis s","F_{is}"="individual coordinate","G_{js}"="category coordinate","J_i"="categories chosen by i","I_j"="individuals choosing j"),
    code_fn="mca_run", code="sweep(fit$coords, 2, sqrt(fit$lam), \"/\")   # standard (category-scale) coordinates",
    cites=list(rf("leroux2010","ch. 3.3","the transition formulas and supplementary elements"))),

  pearson_residual = list(
    name="Pearson standardized residual",
    brief="The unadjusted chi-square cell residual (observed - expected)/sqrt(expected); the building block of the adjusted residual and the signature weights.",
    interpretation="A quick over/under-representation z, but conservative (its true variance is below 1), so it under-detects; prefer Haberman's adjusted residual for significance and use the Pearson form mainly as a weight or descriptive quantity.",
    formula="e_{ij} = \\frac{O_{ij}-E_{ij}}{\\sqrt{E_{ij}}},\\quad E_{ij}=\\frac{n_{i\\cdot}n_{\\cdot j}}{N}",
    algorithm=c("Cross-tabulate group x category.","Expected counts under independence.","(observed - expected)/sqrt(expected)."),
    inputs="Group x category table.", optional_inputs=NA,
    outputs="Pearson residual per cell.", optional_outputs=NA,
    glossary=c("O_{ij}"="observed count","E_{ij}"="expected count","e_{ij}"="Pearson residual"),
    code_fn="mca_residuals", code="mca_residuals(fit, by = \"group\", type = \"pearson\")",
    cites=list(rf("agresti2013","","standardized residuals for contingency tables"), rf("haberman1973","","why the Pearson form is conservative")))))

#' Curated one-row-per-technique overview: what it answers, the function, key citation.
#' @export
mca_overview <- function(keys = names(.MCA_TECH)) {
  do.call(rbind, lapply(keys, function(k) { t <- .MCA_TECH[[k]]
    data.frame(Technique = t$name, "What it answers" = t$brief,
               Function = paste(t$code_fn, collapse = ", "),
               Citation = .MCA_BIB[[.tech_keys(t)[1]]]$cite,
               check.names = FALSE, row.names = NULL) }))
}
