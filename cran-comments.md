## Test environment

- Pop! OS 22.04, R 4.1.2

## Initial Comments to CRAN

R CMD check done via `devtools::check()`, resulting in 0 errors, 0 warnings and 0 notes.

`devtools::spell_check()` results in a lot of typos, all of which are false positives.

This is a new package of mine that I want as an initial development offering.

## Feedback Upon Initial Rejection

I neglected to notice that I was exporting internal functions, which would require me to properly document them. I can attribute that to me leaning on Roxygen to handle documentation for me, but forgetting some of the specifics of what I was doing for other packages that did not lean on Roxygen.

`devtools::check_win_devel()` wants to say `cran-comments.md` is an impermissible, non-standard file, but I cannot reproduce this through any other check. It's included as it should be in `.Rbuildignore`.
