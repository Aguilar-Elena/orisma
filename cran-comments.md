## Test environments

* Local: macOS 26.3 (aarch64-apple-darwin20), R 4.4.0
* GitHub Actions: macOS (release), Ubuntu (release + devel), Windows (release)
* win-builder: R-devel

## R CMD check results

0 errors | 0 warnings | 2 notes

### Notes

1. "Found the following hidden files and directories: .github"
   This is the GitHub Actions CI directory. It is intentionally included
   for continuous integration and is not part of the installed package.

2. "unable to verify current time"
   Network-related note from the check environment. Not a package issue.

## New submission

This is a new submission to CRAN.

ORISMA provides a complete pipeline for systematic bibliometric mapping
of occupational health and safety evidence. It implements three original
bibliometric indicators (WRDI, RCS, MGP) and two preventive intelligence
indicators (ASS, Bridge Score). The package is domain-agnostic and
regulation-neutral for global applicability.

The package has been tested on macOS, Ubuntu, and Windows via GitHub
Actions (see https://github.com/Aguilar-Elena/orisma/actions).

