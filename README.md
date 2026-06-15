# SensoGrappe

**SensoGrappe** is an R package providing functions for sensory data analysis,
developed in the [GRAPPE laboratory](https://www.groupe-esa.com/recherche/grappe/)
(ESA, Angers, France). It covers sensory ANOVA on mixed models, HRATA/HCATA
hierarchical free-sorting analysis, free-comment analysis (mrCA), and
publication-ready visualisation.

---

## Installation

The package is not yet on CRAN. Install the development version directly from
GitHub using the `remotes` (or `devtools`) package:

```r
# Install remotes if needed
install.packages("remotes")

# Install SensoGrappe from GitHub
remotes::install_github("ronysym/SensoGrappe")
```

Once installed, load it as usual:

```r
library(SensoGrappe)
```

> **R ≥ 4.1.0** is required.

---

## Main features

### Sensory ANOVA (mixed models)

| Function | Description |
|---|---|
| `Var.Grappe()` | Check and encode a sensory dataset |
| `AOV.Grappe()` | ANOVA and post-hoc tests on linear mixed models |
| `STAR.Grappe()` | Encode p-values as significance stars |
| `graph.AOV()` | Plot means from an `AOV.Grappe` result |
| `export.AOV()` | Export ANOVA results to a file |
| `Descri.Quanti()` | Boxplots and summary graphs for numeric variables |
| `canonisation()` | Reshape a multi-variable table into a single column |

### HRATA / HCATA (hierarchical free-sorting)

| Function | Description |
|---|---|
| `hrata.codage()` | Prepare the coding file from a hierarchical CSV structure |
| `hrata.agregation()` | Aggregate raw HRATA/HCATA data |
| `hrata.table()` | Compute the average table for multidimensional analysis |
| `hrata.multidim()` | Multidimensional PCA of HRATA/HCATA data |
| `hrata.signi()` | Regression analysis for significance testing |

### Free-comment analysis (mrCA)

| Function | Description |
|---|---|
| `data.preprocess.fc()` | Full preprocessing pipeline for free-comment data |
| `get.binary()` | Binarise free-comment data (subject × product × descriptor) |
| `mr.clust()` | Cluster and aggregate binary descriptors |
| `transfert.MV.BM()` | Decompose a hierarchical descriptor (ConsoTextPlorer format) |
| `preprocess.mrca()` | Prepare a binary matrix for mrCA analysis |
| `plot.sensory.mrCA.grappe()` | Plot an mrCA result |
| `mr.sig.cell.table()` | HTML table of citation percentages with significance colouring |

### Visualisation

| Function | Description |
|---|---|
| `plot.PCA.grappe()` | PCA biplots (adapted from FactoMineR), with ggplot2 output |

---

## Datasets

| Dataset | Description |
|---|---|
| `wine` | Sensory evaluation of wines |
| `rose` | HRATA data on rose flowers |
| `rose.attribute` | Hierarchical attribute structure for the rose dataset |
| `data_fc_not_manual` | Free-comment data from ConsoTextPlorer (raw, not manually annotated) |

---

## Quick example

```r
library(SensoGrappe)

# --- Sensory ANOVA ---
data(wine)
dta <- Var.Grappe(wine, col.product = 1, col.judge = 2, col.note = 3:ncol(wine))
res <- AOV.Grappe(dta)
graph.AOV(res)

# --- HRATA ---
data(rose)
data(rose.attribute)
agg  <- hrata.agregation(rose, rose.attribute)
tab  <- hrata.table(agg)
mult <- hrata.multidim(tab)
```

---

## Dependencies

SensoGrappe imports: `agricolae`, `car`, `dplyr`, `ellipse`, `factoextra`,
`FactoMineR`, `ggplot2`, `ggrepel`, `graphics`, `grDevices`, `grid`,
`gridExtra`, `kableExtra`, `knitr`, `lme4`, `lmerTest`, `plyr`, `qpdf`,
`randomcoloR`, `RColorBrewer`, `stats`, `stringr`, `tidyr`, `utils`.

All dependencies are available on CRAN and will be installed automatically by
`remotes::install_github()`.

---

## Author

**Ronan Symoneaux** — GRAPPE, ESA Angers  
<r.symoneaux@groupe-esa.com>  
ORCID: [0000-0001-6792-8629](https://orcid.org/0000-0001-6792-8629)

## License

GPL-3 — see [LICENSE](LICENSE) for details.
