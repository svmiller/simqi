## Test environment

- Pop! OS 22.04, R 4.1.2

## Initial Comments to CRAN

R CMD check done via `devtools::check()`, resulting in 0 errors, 0 warnings and 0 notes.

`devtools::spell_check()` results in a lot of typos, all of which are false positives.

## Feedback Upon Initial Rejection

I had neglected to fully suppress the vignettes from the package. I intend them for online use only and not as a package bundle. That got fumbled in DESCRIPTION.
