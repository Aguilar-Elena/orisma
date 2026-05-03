#' ORISMA bibliometric indicators
#'
#' @name orisma_indicators
#' @aliases WRDI RCS MGP ASS bridge_score
#'
#' @description
#' ORISMA implements five original bibliometric indicators designed
#' specifically for occupational health and safety (OHS) evidence mapping.
#' Three are corpus-level indicators (WRDI, RCS, MGP) and two are
#' record-level indicators (ASS, Bridge Score).
#'
#'
#'
#' ## 1. Worker-Risk Disconnection Index (WRDI)
#'
#' ### Definition
#' The WRDI measures the proportion of studies in a corpus that characterise
#' an occupational risk **without** including direct worker exposure data.
#' A study is considered to have worker exposure data if its abstract
#' contains terms indicating real measurement of exposure in actual workers
#' under real working conditions (e.g. "worker exposure", "occupational
#' exposure", "breathing zone", "personal sampling", "field study",
#' "workplace measurement").
#'
#' ### Formula
#' For a given risk category \eqn{c}:
#' \deqn{WRDI_c = 1 - \frac{N_{workers,c}}{N_{total,c}}}
#' where \eqn{N_{workers,c}} is the number of studies in category \eqn{c}
#' that include worker exposure data, and \eqn{N_{total,c}} is the total
#' number of studies in that category.
#'
#' The **global WRDI** is computed across all records:
#' \deqn{WRDI_{global} = 1 - \frac{N_{workers}}{N_{total}}}
#'
#' ### Interpretation
#' - **WRDI = 0**: All studies include direct worker exposure data.
#'   The body of evidence is fully connected to real workplace conditions.
#' - **WRDI = 1**: No study includes worker exposure data. The entire
#'   literature characterises the risk technically (e.g. in simulated
#'   environments, chambers, or in vitro) without measuring real exposure
#'   in workers.
#' - **WRDI >= 0.8**: Critical alert. The evidence has very low direct
#'   preventive transferability. On-site risk assessment is essential.
#' - **WRDI 0.5-0.8**: Attention required. More than half the evidence
#'   lacks worker data.
#' - **WRDI < 0.3**: Reasonable coverage. Most studies include worker data.
#'
#' ### Important limitation
#' WRDI detection is based on abstract text, not full text. Studies that
#' measured worker exposure but did not mention it in the abstract may
#' be misclassified. Manual validation via [orm_validate()] is recommended.
#'
#'
#'
#' ## 2. Risk Category Saturation Index (RCS)
#'
#' ### Definition
#' The RCS measures the **relative dominance** of a risk category in the
#' corpus compared to a hypothetical uniform distribution across all
#' categories. It identifies which categories are over-represented
#' (saturated) and which are under-represented (gaps) in the literature.
#'
#' ### Formula
#' \deqn{RCS_c = \frac{pct_c}{pct_{uniform}}}
#' where \eqn{pct_c} is the percentage of records assigned to category
#' \eqn{c}, and \eqn{pct_{uniform} = 100 / K} is the percentage each
#' category would have under a uniform distribution across all \eqn{K}
#' categories.
#'
#' Equivalently:
#' \deqn{RCS_c = \frac{N_c \cdot K}{N_{total}}}
#' where \eqn{N_c} is the number of records in category \eqn{c},
#' \eqn{K} is the total number of categories, and \eqn{N_{total}} is
#' the total number of records.
#'
#' ### Interpretation
#' - **RCS > 1**: The category is over-represented relative to a uniform
#'   baseline. The literature has concentrated disproportionately on this
#'   risk type.
#' - **RCS = 1**: The category has exactly the representation expected
#'   under a uniform distribution.
#' - **RCS < 1**: The category is under-represented. This risk type has
#'   received less attention than a balanced literature would suggest.
#' - **RCS = 0**: No studies address this category. Complete evidence gap.
#'
#' ### Note
#' RCS is a relative measure. A category can have RCS > 1 with very few
#' absolute studies if the corpus is small or highly specialised. Always
#' interpret RCS together with the absolute number of records (N).
#'
#'
#'
#' ## 3. Material-Gap Profile (MGP)
#'
#' ### Definition
#' The MGP is a domain-specific indicator designed for corpora where the
#' corpus can be stratified by material, substance, or agent. It measures
#' the ratio between a material's known hazard potential and its coverage
#' in the occupational health literature, identifying materials that are
#' dangerous but understudied.
#'
#' ### Formula
#' \deqn{MGP_m = \frac{hazard\_proxy_m}{coverage_m}}
#' where \eqn{hazard\_proxy_m} is an estimate of the material's hazard
#' potential (based on the number of distinct risk categories detected in
#' studies involving that material), and \eqn{coverage_m} is the proportion
#' of corpus records that address that material.
#'
#' ### Interpretation
#' - **High MGP**: The material is associated with multiple risk categories
#'   but appears in few studies. Priority material for future research and
#'   on-site risk assessment.
#' - **Low MGP**: The material is well-covered in the literature relative
#'   to its known hazard profile.
#' - **MGP requires a material column**: The `material_col` parameter in
#'   [orm_analyse()] must point to a column classifying each record by
#'   material or agent. If not available, MGP is not computed.
#'
#'
#'
#' ## 4. Abstract Sufficiency Score (ASS)
#'
#' ### Definition
#' The ASS is a **cumulative hierarchical index** (0-5) measuring how much
#' preventively useful information an abstract contains for an occupational
#' health practitioner. It is not a measure of study quality, but of
#' abstract informativeness for preventive purposes.
#'
#' The score is strictly cumulative: a record cannot reach level N without
#' satisfying all previous levels.
#'
#' ### Levels
#' \describe{
#'   \item{0 - Non-informative}{The abstract contains no hazard or risk
#'     terms relevant to OHS. No useful preventive information.}
#'   \item{1 - Hazard without context}{The abstract mentions a hazard or
#'     risk agent (e.g. nanoparticles, noise, vibration) but provides no
#'     occupational or workplace context. Could be an environmental or
#'     laboratory study.}
#'   \item{2 - Occupational context}{The abstract mentions workers,
#'     employees, operators, or workplace/occupational setting. The study
#'     is clearly situated in a work context.}
#'   \item{3 - Exposure measurement}{The abstract reports quantitative
#'     exposure data: concentrations, levels, measurements, or monitoring
#'     results. Implies some form of exposure quantification.}
#'   \item{4 - Worker exposure with result}{The abstract explicitly
#'     reports exposure in workers (not just in the environment) with a
#'     result (e.g. exceeded a limit, found significant association,
#'     detected at breathing zone).}
#'   \item{5 - Complete preventive abstract}{The abstract addresses all
#'     four dimensions: worker population + exposure measurement + study
#'     method/design + preventive recommendation or control measure. This
#'     is the highest OHS informative level.}
#' }
#'
#' ### Computation
#' Each level is detected via regular expression patterns applied to the
#' abstract text. Detection is strictly cumulative: the algorithm tests
#' each level in sequence and stops at the first level not satisfied.
#'
#' ### Interpretation
#' - **Mean ASS < 2**: The corpus is predominantly technical with very
#'   little preventive context. High priority for on-site investigation.
#' - **Mean ASS 2-3**: Mixed corpus. Some workplace context but limited
#'   quantitative exposure data.
#' - **Mean ASS > 3**: Good preventive evidence base. Substantial
#'   proportion of studies report actual worker exposure data.
#' - **ASS = 5 articles**: These are the most valuable abstracts for
#'   practitioners and should be read in full first.
#'
#'
#'
#' ## 5. Bridge Article Score
#'
#' ### Definition
#' A bridge article is a study that **connects technical science with
#' applied OHS prevention**. It simultaneously addresses five dimensions
#' that are rarely all present in a single study:
#'
#' \describe{
#'   \item{Criterion 1 - Technology/process}{The study involves a specific
#'     technology, industrial process, or work task (e.g. additive
#'     manufacturing, welding, construction, healthcare).}
#'   \item{Criterion 2 - Hazardous agent}{The study characterises a
#'     specific hazardous agent (chemical, physical, biological, or
#'     psychosocial).}
#'   \item{Criterion 3 - Workers (MANDATORY)}{The study involves a real
#'     worker population in a real workplace setting. This criterion is
#'     mandatory for bridge classification.}
#'   \item{Criterion 4 - Exposure measurement (MANDATORY)}{The study
#'     quantitatively measures exposure (air sampling, biological
#'     monitoring, dosimetry, etc.). This criterion is mandatory for
#'     bridge classification.}
#'   \item{Criterion 5 - Prevention/recommendation}{The study includes
#'     preventive recommendations, control measures, or intervention
#'     results.}
#' }
#'
#' ### Classification
#' \describe{
#'   \item{Strong bridge (score 4-5)}{Meets criteria 3+4 (mandatory) plus
#'     2 or 3 additional criteria. Highest priority for full-text reading.
#'     These articles have already done the translation from laboratory
#'     science to workplace prevention.}
#'   \item{Partial bridge (score 3)}{Meets criteria 3+4 (mandatory) plus
#'     1 additional criterion. Valuable but incomplete bridge.}
#'   \item{Technical study (score 0-2, or missing C3/C4)}{Does not meet
#'     the mandatory criteria. Contributes technical knowledge but lacks
#'     direct preventive applicability.}
#' }
#'
#' ### Priority score
#' The overall **priority reading score** used in [orm_ranking()] combines
#' all record-level indicators:
#' \deqn{Priority = (Bridge \times 2) + (ASS \times 1.5) + (N_{cats} \times 0.5)}
#' where \eqn{N_{cats}} is the number of risk categories detected in the
#' record. Bridge score is weighted highest because it reflects the most
#' direct preventive relevance.
#'
#'
#'
#' ## References
#'
#' The WRDI, RCS, and MGP indicators were first described in:
#'
#' Aguilar-Elena, R. & Delgado-Garcia, A. (2025). *Mapping the Safety
#' Landscape of Emerging Technologies: A Bibliometric Analysis of
#' Occupational Risks in Metal Additive Manufacturing*. (Under review)
#'
#' The ORISMA methodological framework is described in:
#'
#' Aguilar-Elena, R. & Delgado-Garcia, A. (2025). *orisma: A Framework
#' for Occupational Risk Integrated Systematic Mapping and Analysis*.
#' R package version 0.1.0. Universidad Internacional de Valencia (VIU) &
#' Universidad de Salamanca (USAL).
#'
#' @seealso
#' [orm_analyse()] to compute WRDI, RCS, and MGP.
#' [orm_ass()] to compute the Abstract Sufficiency Score.
#' [orm_bridge()] to detect bridge articles.
#' [orm_ranking()] to generate a priority reading list.
#' [orm_validate()] to validate automatic classification with Cohen's Kappa.
#'
NULL
