## R CMD check results

0 errors | 0 warnings | 1 note

* checking data for non-ASCII characters ... NOTE
  Note: found 5 marked UTF-8 strings

These UTF-8 strings are intentional and occur in the package example dataset
(`orisma_sample`). They correspond to scientific abstract text containing
standard typographic and scientific characters, including the micro symbol,
typographic quotation marks, en dashes, and ellipses. The package declares
Encoding: UTF-8 in DESCRIPTION.

## Test environments

* Local macOS, R 4.4.0
* win-builder, R-devel, Windows Server 2022
* GitHub Actions:
  - macOS-latest, R release
  - windows-latest, R release
  - ubuntu-latest, R release
  - ubuntu-latest, R devel

## GitHub Actions

R CMD check completed successfully on GitHub Actions.

## Submission

This is the initial CRAN submission.
