# orisma 0.1.0

* Initial CRAN submission.
* Complete pipeline with `orm_load()`, `orm_run()` and `orm_report()`.
* Added `orm_run_guarded()` for running the complete ORISMA pipeline with a relevance-control layer.
* Added `orm_relevance_guard()` to flag or exclude records with weak topic or occupational relevance.
* Added three relevance guard modes:
  * `flag`: excludes only clearly off-topic records and flags uncertain records for review.
  * `conservative`: default balanced mode; excludes clearly off-topic and likely non-occupational biomedical/clinical records while retaining uncertain records for review.
  * `strict`: excludes records with weak occupational context for rapid practitioner-oriented screening.
* Three original bibliometric indicators: WRDI, RCS and MGP.
* Two preventive intelligence indicators: ASS and Bridge Article Score.
* Built-in occupational risk dictionary with 58 categories organised in six blocks: Safety, Industrial Hygiene, Ergonomics, Psychosociology, Biological Hazards and Emerging Technologies.
* Bilingual outputs (EN/ES): HTML reports, occupational risk sheets, extraction matrices and priority rankings.
* Automatic dimension detection via normative blocks.
* Manual validation support with `orm_validate()`.
* Sample dataset `orisma_sample` included for examples and testing.
