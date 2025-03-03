#' Get simulations from a model object
#'
#' @description \code{sim_qi()} is a function to simulate quantities of interest
#' from a regression model
#'
#' @param mod a model object
#' @param nsim number of simulations to be run, defaults to 1,000
#' @param newdata A data frame with a hypothetical prediction grid.
#' If absent, defaults to the model frame.
#' @param original_scale logical, defaults to TRUE. If TRUE, the ensuing
#' simulations are returned on their original scale. If FALSE, the ensuing
#' simulations are transformed to a more practical/intuitive quantity that is
#' potentially more intuitive for the user (e.g. a probability for a logistic
#' regression). This argument is ignored in the context of
#' simulations on the linear model.
#' @param return_newdata logical, defaults to FALSE. If TRUE, the output returns
#' additional columns corresponding with the inputs provided to \code{newdata}.
#' This may facilitate easier transformation along with greater clarity as to
#' what the simulations correspond.
#' @param vcov a manual variance-covariance matrix to supply to the simulation
#' process. Use with caution, and if you did some kind of on-the-fly standard
#' error adjustment in your regression model to make your results "robust". If
#' nothing is supplied, the model's default variance-covariance matrix is used.
#'
#' @return
#'
#' \code{sim_qi()} returns a data frame (as a \code{tibble}) with the
#' quantities of interest and identifying information about the particular
#' simulation number. For linear models, or simple generalized linear models
#' where the dependent variable is either "there" or "not there", the quantity
#' of interest returned is a single column (called `y`). For models where the
#' underlying estimation of the dependent variable is, for lack of a better
#' term, "multiple" (e.g. ordinal models with the basic proportional odds
#' assumption), the columns returned correspond with the number of distinct
#' values of the outcome. For example, an ordinal model where there are five
#' unique values of the dependent variable will return columns `y1`, `y2`, `y3`,
#' `y4`, and `y5`.
#'
#' @details
#'
#' # Supported Models
#'
#' 1. Linear models produced by `lm()` in base R.
#' 2. Generalized linear models produced by `glm()`. Families (links) include:
#'      - Binomial (logit, probit)
#'      - Poisson (log)
#' 3. Cumulative link models produced by \pkg{ordinal} package.
#'      - Links: logit, probit
#'
#' ## What `original_scale` Does in This Function
#'
#' `original_scale` defaults to TRUE in this function. When TRUE, the simulated
#' quantity that's returned is a quantity on its "original scale." Understanding
#' what exactly that means requires some knowledge about the model in question
#' and what exactly the model is estimating on your behalf. In the simple linear
#' model produced by the `lm()` function in base R, this is straightforward (and,
#' thus, this argument does nothing for that model). The quantity returned is
#' the estimated value of the dependent variable. However, models with some kind
#' of link function return fitted values on some particular scale that might not
#' be so user-friendly (e.g. a probit index, or a natural logged odds). However,
#' that is the "original scale" on which the fitted values are returned. This
#' summary table may help you better understand what this argument does with
#' respect to what you want.
#'
#' | *Model Function* | *Family (Link)* | *original_scale = TRUE* | *original_scale = FALSE* |
#' |:----------------:|:------:|:-----------------------:|:------------------------:|
#' | `lm()` | NA | NA (Estimated value of *y*) | NA (Estimated value of *y*) |
#' | `glm()`| binomial(link='logit') | natural logged odds of *y* = 1 | probability of *y* = 1 |
#' | `glm()`| binomial(link='probit') | probit index of *y* = 1 | probability of *y* = 1 |
#' | `glm()`| poisson(link='log') | logged lambda | lambda |
#' | `clm()`| link = 'logit' | natural logged odds of *y* = value *j* | probability of *y* = value *j* |
#' | `clm()`| link = 'probit' | probit index of *y* = value *j* | probability of *y* = value *j* |
#' | `logistf()`| NA | natural logged odds of *y* = 1 | probability of *y* = 1 |
#'
#' For ordinal models, I recommend setting `original_scale` to be FALSE. The
#' function, underneath the hood, is actually calculating things on the level of
#' the probability. It's just transforming back to a natural logged odds or a
#' probit index, if that is what you say you want.
#'
#' ## Other Details
#'
#' Specifying a variable in `newdata` with the exact same name as the
#' dependent variable (e.g. `mpg` in the simple example provided in this
#' documentation file) is necessary for matrix multiplication purposes. The
#' function will do that for you if you have not done it yourself. I recommend
#' letting this function do that for you. For matrix multiplication purposes,
#' this column this function creates will have a default of 0. It does not
#' (should not?) matter for the simulations.
#'
#' If nothing is supplied in `newdata`, `model.frame()` is called and the
#' simulations are run on the data that inform the model itself. I don't
#' recommend this, but it works for debugging purposes.
#'
#' This function builds in an implicit assumption that your dependent variable
#' in the regression model is not called `y`. Nothing about this function will
#' misbehave (as far as I know) if your dependent variable is called `y` in the
#' model, but it may lead to some confusion in how you interpret the results of
#' the simulations. The simulated values are always returned as a column called
#' `y`.
#'
#' Factors (so-called "fixed effects") behave curiously in this function. For now,
#' this function will politely assume your factors are *all* present in the
#' `newdata` you create (even if you don't want them). Future updates
#' will try to understand this behavior better. The only loss here is the
#' efficiency of the simulation procedure, especially if you are not interested
#' in simulated values of the dependent variable for particular combinations of
#' the factor variable.
#'
#' @examples
#'
#' set.seed(8675309)
#'
#' M1 <- lm(mpg ~ hp + am, mtcars)
#'
#' newdat <- data.frame(am = c(0,1), hp = 123)
#'
#' sim_qi(M1, nsim = 100, newdat, return_newdata = TRUE)
#'
#' @importFrom MASS mvrnorm
#' @importFrom methods is
#' @importFrom tibble as_tibble tibble
#' @importFrom stats coef model.frame model.matrix plogis pnorm qnorm rchisq terms vcov
#' @export
#'

# sim_qi <- function(mod, nsim = 1000, newdata, original_scale = TRUE, return_newdata = FALSE, ...) {
#     UseMethod("sim_qi")
# }
# I may want to do it this way down the road if I can do even more with this.


sim_qi <- function(mod, nsim = 1000, newdata, original_scale = TRUE, return_newdata = FALSE, vcov = NULL) {
    # if (!identical(class(mod), "lm")) {
    #     stop("Hold on a second.")
    # }

    # various stops and warnings ----

    if((missing(newdata) & return_newdata == TRUE)) {
        stop("`return_newdata` cannot be TRUE with no data frame supplied to the `newdata` argument.")
    }

    if(!missing(newdata)) {
        if(!is(newdata, "data.frame")) {
        stop("The object supplied to the `newdata` argument must be a data frame.")
        }
    } else {

    }


    # class: lm -----
    if(identical(class(mod), "lm")) {

        the_sims <- .sim_qi.lm(mod, nsim, newdata, original_scale, vcov)

    }


    # class: glm -----
    if(is(mod, "glm")) {

        the_sims <- .sim_qi.glm(mod, nsim, newdata, original_scale, vcov)
    }


    # class: clm -----
    if(is(mod, "clm")) {

        the_sims <- .sim_qi.clm(mod, nsim, newdata, original_scale, vcov)

    }


    # class: logistf -----
    if(is(mod, "logistf")) {

        the_sims <- .sim_qi.logistf(mod, nsim, newdata, original_scale, vcov)

    }


    if(missing(newdata)) {
        newdata <- model.frame(mod)
    }

    if(return_newdata == TRUE) {

        nnn <- do.call(rbind, replicate(n = nsim, expr = newdata,
                                        simplify = FALSE))
        the_sims <- as_tibble(cbind(the_sims, nnn))

    }


    return(the_sims)
}
