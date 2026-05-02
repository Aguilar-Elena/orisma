# orisma <img src="man/figures/logo.png" align="right" height="120" alt="orisma logo"/>

**Occupational Risk Integrated Systematic Mapping and Analysis**

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/orisma)](https://CRAN.R-project.org/package=orisma)
[![R-CMD-check](https://github.com/Aguilar-Elena/orisma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Aguilar-Elena/orisma/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

`orisma` is an R package that automates the complete pipeline for **systematic bibliometric mapping of occupational risk evidence** in any domain — emerging technologies, established hazards, or specific industrial sectors. It produces both **academic outputs** (indicators, visualisations, reproducibility certificates) and **practitioner outputs** (risk sheets, priority rankings, guided extraction matrices).

---

## What does ORISMA do?

Starting from reference files exported from any major bibliographic database (Web of Science, Scopus, PubMed, Dimensions, EBSCO, and others), `orisma` runs a fully automated pipeline in **2-3 seconds**:

1. **Ingestion** — reads RIS, BibTeX and CSV files from multiple databases simultaneously
2. **Deduplication** — three-step pipeline (exact DOI → normalised title → fuzzy match)
3. **Risk extraction** — scans abstracts against a 58-category normative dictionary (ISO 45001 / INSST / NIOSH / EU-OSHA)
4. **Bibliometric analysis** — co-occurrence matrices, temporal trends, hierarchical clustering
5. **Original indicators** — WRDI, RCS, MGP (see below)
6. **Dimension detection** — automatic discovery of corpus dimensions (normative blocks or free text)
7. **Abstract Sufficiency Score** — 0-5 index of preventive usefulness per abstract
8. **Bridge article detection** — identifies articles connecting technical science with applied prevention
9. **Priority ranking** — top articles for practitioner reading
10. **Reports** — bilingual HTML reports (EN/ES), risk sheets, guided extraction matrices, reproducibility certificates

---

## Three original bibliometric indicators

| Indicator | Full name | What it measures |
|---|---|---|
| **WRDI** | Worker-Risk Disconnection Index | Proportion of studies characterising a risk without measuring real worker exposure |
| **RCS** | Risk Category Saturation Index | Relative dominance of each risk category vs. a uniform baseline |
| **MGP** | Material-Gap Profile | Ratio between a material's known hazard potential and its coverage in the literature |

---

## Two new preventive intelligence indicators

| Indicator | Full name | What it measures |
|---|---|---|
| **ASS** | Abstract Sufficiency Score (0-5) | How much preventively useful information an abstract contains for an OHS practitioner |
| **Bridge score** | Bridge Article Score (0-5) | Degree to which a study connects technical science with applied OHS prevention |

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

## Minimal usage — 3 lines

```r
library(orisma)

refs   <- orm_load("my_references/")   # load RIS/BibTeX/CSV files
result <- orm_run(refs)                 # full pipeline (2-3 sec)
orm_report(result, lang = "en")         # generate all outputs
```

For **Spanish output**:

```r
options(orisma.lang = "es")
refs   <- orm_load("mis_referencias/")
result <- orm_run(refs)
orm_report(result, lang = "es", out_dir = "resultados/")
```

---

## Complete function reference

| Function | For whom | What it does |
|---|---|---|
| `orm_load()` | Everyone | Multi-source ingestion (RIS, BibTeX, CSV) with format auto-detection |
| `orm_dedup()` | Everyone | 3-step deduplication (DOI + title + fuzzy) |
| `orm_extract()` | Researcher | Risk category extraction via normative dictionary |
| `orm_analyse()` | Researcher | Compute WRDI, RCS, MGP indicators |
| `orm_autodim()` | Researcher | Automatic dimension discovery (blocks or free text) |
| `orm_dim_matrix()` | Researcher | Risk x dimension cross-heatmap |
| `orm_ass()` | Both | Abstract Sufficiency Score (0-5) per record |
| `orm_bridge()` | Both | Bridge article detection and classification |
| `orm_ranking()` | Both | Priority reading list by combined score |
| `orm_priority()` | Both | Traffic light classification RED/AMBER/GREEN/GREY |
| `orm_run()` | Everyone | **Complete pipeline in one call** |
| `orm_report()` | Researcher | Full HTML report with 7 visualisations |
| `orm_risk_sheet()` | OHS practitioner | Actionable risk sheet (regulation-neutral) |
| `orm_extraction_matrix()` | Both | Guided extraction template for PDF review |
| `orm_validate()` | Researcher | Manual validation with Cohen's Kappa |
| `orm_dict()` | Everyone | Load/customise the 58-category risk dictionary |

---

## Outputs generated automatically

After running `orm_report()` and `orm_risk_sheet()`:

### For researchers
| File | Description |
|---|---|
| `orisma_report.html` | Interactive bilingual executive report with 7 plots |
| `orisma_corpus.csv` | All records after deduplication |
| `orisma_matrix.csv` | Binary risk category matrix (records x categories) |
| `orisma_indicators.csv` | WRDI, RCS, MGP per category |
| `prisma_log.csv` | PRISMA-compatible selection flow |
| `analysis.orisma` | Reproducibility certificate (JSON with MD5 hashes) |
| `plots/` | 7 publication-ready PNG plots |

### For OHS practitioners
| File | Description |
|---|---|
| `orisma_risk_sheet.html` | Actionable risk sheet with RED/AMBER/GREEN traffic light |
| `orisma_extraction_matrix.csv` | Pre-filled extraction template for PDF review |
| `orisma_priority_ranking.csv` | Top-20 priority articles by bridge + ASS score |

---

## Risk dictionary

The built-in dictionary covers **58 risk categories** in 6 normative blocks:

| Block | Categories | Standards |
|---|---|---|
| A — Safety at work | 18 | INSST / ISO 45001 |
| B — Industrial hygiene | 8 | INSST / NIOSH |
| C — Ergonomics | 8 | INSST / ISO 45001 |
| D — Psychosociology | 11 | INSST / ISO 45001 |
| E — Biological hazards | 5 | EU-OSHA / NIOSH |
| F — Emerging technologies | 8 | EU-OSHA 2024-2026 |

The dictionary can be extended for any domain:

```r
dict <- orm_dict()

# Add terms to an existing category
dict <- orm_dict_add_terms(dict, "nanomaterials", c("nano-aerosol", "NOAA"))

# Add a completely new category
dict <- orm_dict_add_category(dict,
  key      = "exoskeleton_risk",
  label_en = "Exoskeleton-related musculoskeletal risk",
  label_es = "Riesgo musculoesqueletico por exoesqueleto",
  terms    = c("exoskeleton", "powered exosuit", "wearable robot")
)
```

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
| The Lens (free) | RIS or CSV | No limit |

Export all databases in **RIS format**, place files in a folder, and run `orm_load("folder/")`. ORISMA detects the source database automatically from the filename.

---

## Abstract Sufficiency Score (ASS)

The ASS is a cumulative 0-5 index measuring how much preventively useful information an abstract contains:

| Score | Meaning |
|---|---|
| 0 | Non-informative for OHS purposes |
| 1 | Mentions a hazard but no occupational context |
| 2 | Mentions occupational/workplace context |
| 3 | Mentions exposure measurement or quantification |
| 4 | Mentions exposure in workers with a result |
| 5 | Complete: exposure + worker population + method + prevention |

---

## Bridge articles

A **bridge article** connects technical science with applied OHS prevention. It simultaneously addresses:

1. Technology or process
2. Hazardous agent
3. Workers (real workplace population)
4. Exposure measurement
5. Preventive recommendation

Articles meeting 4-5 criteria = **Strong bridge** (highest priority for reading).
Articles meeting 3 criteria (must include workers + measurement) = **Partial bridge**.

---

## Methodological note

ORISMA uses dictionary-based automatic classification. This may produce false positives. Manual validation of a representative sample is recommended using `orm_validate()`, which computes Cohen's Kappa between automatic and manual classification. A Kappa >= 0.7 is acceptable for high-impact journal publication.

ORISMA does not include country-specific regulations or limit values, as these vary by jurisdiction. The practitioner applies the relevant national/regional regulation based on the risk categories identified.

---

## Citation

If you use `orisma` in your research, please cite:

```
Aguilar-Elena, R. & Delgado-Garcia, A. (2025). orisma: Occupational Risk
Integrated Systematic Mapping and Analysis. R package version 0.1.0.
Universidad Internacional de Valencia (VIU) & Universidad de Salamanca (USAL).
https://github.com/Aguilar-Elena/orisma
```

---

## Authors

**PhD. Raul Aguilar-Elena** &middot; raguilar@universidadviu.com  
Occupational Risk Prevention and Occupational Health Research Group (GPRL)  
Universidad Internacional de Valencia (VIU), Valencia, Spain

**Ana Delgado-Garcia** &middot; a.delgado@usal.es  
Universidad de Salamanca (USAL), Salamanca, Spain

---

## License

MIT © 2025 Raul Aguilar-Elena & Ana Delgado-Garcia
