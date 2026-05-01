# orisma <img src="man/figures/logo.png" align="right" height="120" alt="orisma logo"/>

**Occupational Risk Integrated Systematic Mapping and Analysis**

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/orisma)](https://CRAN.R-project.org/package=orisma)
[![R-CMD-check](https://github.com/Aguilar-Elena/orisma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Aguilar-Elena/orisma/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`orisma` is an R package that automates the complete pipeline for **systematic bibliometric mapping of occupational risk evidence** in any domain — emerging technologies, established hazards, or specific industrial sectors.

---

## What does ORISMA do?

Starting from reference files exported from any major bibliographic database (Web of Science, Scopus, PubMed, Dimensions, EBSCO, and others), `orisma` runs a fully automated pipeline:

1. **Ingestion** — reads RIS, BibTeX and CSV files from multiple databases simultaneously
2. **Deduplication** — three-step pipeline (exact DOI → normalised title → fuzzy match)
3. **Risk extraction** — scans abstracts and titles against a normative dictionary (ISO 45001 / INSST / NIOSH)
4. **Bibliometric analysis** — co-occurrence matrices, temporal trends, hierarchical clustering
5. **Original indicators** — WRDI, RCS, MGP (see below)
6. **Reports** — bilingual HTML report (EN/ES), CSV files, visualisations, and a reproducibility certificate

---

## Three original indicators

| Indicator | Full name | What it measures |
|---|---|---|
| **WRDI** | Worker-Risk Disconnection Index | Proportion of studies characterising a risk without measuring real worker exposure |
| **RCS** | Risk Category Saturation Index | Relative dominance of each risk category vs. a uniform baseline |
| **MGP** | Material-Gap Profile | Ratio between a material's known hazard potential and its coverage in the literature |

---

## Installation

```r
# From CRAN (once published)
install.packages("orisma")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("Aguilar-Elena/orisma")
```

---

## Minimal usage

```r
library(orisma)

# Step 1: Export references from WoS, Scopus, PubMed... in RIS format
# Step 2: Put all .ris files in a folder, e.g. "my_references/"

# Three lines to a complete analysis:
refs   <- orm_load("my_references/")
result <- orm_run(refs)
orm_report(result)
```

For **Spanish output**:

```r
options(orisma.lang = "es")
refs   <- orm_load("mis_referencias/")
result <- orm_run(refs)
orm_report(result, out_dir = "resultados_orisma/")
```

---

## Modular workflow (full control)

```r
library(orisma)

# Load
refs <- orm_load("my_references/")

# Deduplicate (3-step pipeline)
deduped <- orm_dedup(refs)

# Customise the dictionary
dict <- orm_dict()
dict <- orm_dict_add_terms(dict, "nanoparticles", c("nano-aerosol", "UFP"))
dict <- orm_dict_add_category(dict,
  key      = "vibration",
  label_en = "Vibration exposure",
  label_es = "Exposición a vibraciones",
  terms    = c("hand-arm vibration", "whole-body vibration", "HAV")
)

# Extract risk categories
mx <- orm_extract(deduped, dict = dict)

# Compute indicators
result <- orm_analyse(mx, material_col = "keywords")

# Generate reports
orm_report(result, lang = "es", out_dir = "results/")
```

---

## Outputs

After running `orm_report()`, the output directory contains:

| File | Description |
|---|---|
| `orisma_report.html` | Interactive bilingual executive report |
| `orisma_corpus.csv` | All records after deduplication |
| `orisma_matrix.csv` | Binary risk category matrix |
| `orisma_indicators.csv` | WRDI, RCS, and MGP per category |
| `prisma_log.csv` | PRISMA-compatible flow log |
| `analysis.orisma` | Reproducibility certificate (JSON with MD5 hashes) |
| `plots/` | PNG visualisations (heatmap, gap map, temporal trends, etc.) |

---

## Supported databases

| Database | Recommended format | Batch limit |
|---|---|---|
| Web of Science | RIS (Plain text) | 1 000 |
| Scopus | RIS or CSV | 2 000 |
| PubMed | RIS | No limit |
| Dimensions | CSV or RIS | 2 500 |
| EBSCO (CINAHL, BSC) | RIS | 25 000 |
| ProQuest | RIS or BibTeX | 100 |
| Cochrane Library | RIS | No limit |
| Ovid / MEDLINE | RIS | 1 000 |
| ScienceDirect | RIS | No limit |
| The Lens | RIS or CSV | No limit |

---

## Citation

If you use `orisma` in your research, please cite:

```
Aguilar-Elena, R. (2025). orisma: Occupational Risk Integrated Systematic
Mapping and Analysis. R package version 0.1.0. Universidad Internacional de
Valencia (VIU). https://github.com/raguilarelena/orisma
```

---

## Author

**Dr. Raúl Aguilar-Elena**  
Occupational Risk Prevention and Occupational Health Research Group (GPRL)  
Universidad Internacional de Valencia (VIU), Valencia, Spain  
📧 raguilar@universidadviu.com

---

## License

MIT © 2025 Raúl Aguilar-Elena, Universidad Internacional de Valencia (VIU)
