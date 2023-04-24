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
#' simulations are transformed to a more practical/intuitive quantity that for
#' now is the simulated probability. This argument is ignored in the context of
#' simulations on the linear model.
#'
#' @return
#'
#' \code{sim_qi()()} returns a data frame (as a \code{tibble}) with the
#' quantities of interest and identifying information about the particular
#' simulation number. For linear models, or simple generalized linear models
#' where the dependent variable is either "there" or "not there", the quantity
#' of interest returned is a single column (called `y`). For models where the
#' underlying estimation of the dependent variable is, for lack of a better
#' term, "multiple" (e.g. ordinal models with the basic proportional odds)
#' assumption), the columns returned correspond with the number of distinct
#' values of the outcome. For example, an ordinal model where there are five
#' unique values of the dependent variable will return columns `y1`, `y2`, `y3`,
#' `y4`, and `y5`.
#'
#' @details
#'
#' For ordinal models, I recommend setting `original_scale` to be FALSE. The
#' function, underneath the hood, is actually calculating things on the level of
#' the probability. It's just transforming back to a logit or a probit, if that
#' is what you say you want.
#'
#'
#' @examples
#'
#' set.seed(8675309)
#'
#' M1 <- lm(mpg ~ cyl + wt, mtcars)
#'
#' sim_qi(M1, 10)
#'
#'
#' @export
#'

sim_qi <- function(mod, nsim = 1000, newdata, original_scale = TRUE) {
    # if (!identical(class(mod), "lm")) {
    #     stop("Hold on a second.")
    # }

    if(identical(class(mod), "lm")) {

        if(original_scale == FALSE) {
            warning("There is no conversion of a link function to note in a linear model. This function ignores this argument for models of class 'lm'.")
        }

        siggies <- summary(mod)$sigma
        nobs <- summary(mod)$df[1] + summary(mod)$df[2]
        k <- summary(mod)$df[1]

        siggies*sqrt((nobs-k)/rchisq(nsim,nobs-k))
        list(a = smvrnorm(nsim, coef(mod), vcov(mod)),
             b = siggies*sqrt((nobs-k)/rchisq(nsim,nobs-k))) -> sim_params

        if(missing(newdata)) {
            newdata <- model.frame(mod)
        }

        modmat <- model.matrix(terms(mod), newdata)

        the_sims <- tibble(y = numeric(), sim = numeric())

        for (i in (1:nsim)) {

            yi <- modmat %*% sim_params$a[i,]

            simval <- rep(i, length(yi))
            hold_me <- data.frame(y = yi,
                                  sim = i)

            the_sims <- rbind(the_sims, hold_me)
            the_sims <- as_tibble(the_sims)

        }
    }


    if(is(mod, "glm")) {

        the_link <- mod$family$link


        if(!(the_link %in% c("logit", "probit"))) {

            stop("For now, this function works on just GLMs with a link of either probit or logit.")
        }

        smvrnorm(nsim, coef(mod), vcov(mod)) -> sim_params

        if(missing(newdata)) {
            newdata <- model.frame(mod)
        }

        modmat <- model.matrix(terms(mod), newdata)

        the_sims <- tibble(y = numeric(), sim = numeric())

        for (i in (1:nsim)) {

            yi <- modmat %*% sim_params[i,]

            simval <- rep(i, length(yi))
            hold_me <- data.frame(y = yi,
                                  sim = i)

            the_sims <- rbind(the_sims, hold_me)
            the_sims <- as_tibble(the_sims)

        }

        if(original_scale == FALSE && the_link == "logit") {
            the_sims$y <- plogis(the_sims$y)
        } else if(original_scale == FALSE && the_link == "probit") {

            the_sims$y <- pnorm(the_sims$y)
        }
    }

    if(is(mod, "clm")) {

        if(missing(newdata)) {
            newdata <- model.frame(mod)
        }

        the_link <- mod$info$link

        if(!(the_link %in% c("logit", "probit"))) {

            stop("For now, this function works on just ordinal models with a link of either probit or logit.")
        }


        smvrnorm(nsim, coef(mod), vcov(mod)) -> simmies
        simmies <- as.data.frame(simmies)

        simmies$sim <- seq(1:nrow(simmies))

        # This is going to 1) replicate the simmies the number of times corresponding
        #  with the number of rows there are in newdata. The do.call() just rbinds them.

        sss <- do.call(rbind, replicate(n = nrow(newdata), expr = simmies,
                                        simplify = FALSE))

        # This just orders them for my own sanity.
        sss[order(sss$sim),] -> sss

        # Now, we're going to do the same that we did with simmies, this time with the
        #  newdata. We're just going to replicate them the number of times corresponding
        #  with the number of simulations we asked for (nsim)
        nnn <- do.call(rbind, replicate(n = nsim, expr = newdata, simplify = FALSE))

        # This is a fairly complicated operation to do a simple thing. To clarify:
        #  - Using sss, we're going to grab the column names with a "|" in them, and
        #    split by the "|".
        #  - Then, we're going to collapse them to a character vector (unlist), get just
        #    the unique values (unique), and convert them to a numeric.
        #  - Then, we'll just add an element to it that is the max of it, plus one.
        #    This identifies all unique values of the DV.
        ## x <- as.numeric(unique(unlist(strsplit(grep("\\|", colnames(sss), value=TRUE), split="\\|"))))
        ## x <- c(x, max(x)+1)
        # Alternatively...
        y_levs <- mod$y.levels

        # find intersection (i.e. just the stuff that we'd want for xb)
        col_intersection <- intersect(colnames(nnn), colnames(sss))
        # col_intersection

        # sss$xb <- rowSums(mapply(`*`, nnn[, col_intersection],
        #                          simmies[, col_intersection]))

        # create an xb that is the simple multiplication of the intersections.
        # This has the effect of multiplying our "set" values in newdata by the
        # simulated coefficients.
        xb <- rowSums(mapply(`*`, nnn[, col_intersection],
                             simmies[, col_intersection]))


        # Here's what this is going to do, which is the base R equivalent of a
        #  mutate_at(). We're going to create a custom function (funcy) that's just
        #  going to subtract the xbs from the thresholds. "Subbies" is going to take
        #  some of the code that I commented out above, simplifying identifying which
        #  columns in simmies have those thresholds. Then, apply() is going to take
        #  the sss column (matching the dimensions that will ultimately come) and apply
        #  the function. The 2 you see applies to the margin argument, indicating you
        #  you should apply this to the column.

        if(the_link == "logit") {

            funcy <- function(x) {plogis(x - xb)}

        } else if (the_link == "probit"){

            funcy <- function(x) {pnorm(x - xb)}
        }


        subbies <- grep("\\|", colnames(simmies), value=TRUE)
        data.frame(apply(sss[, subbies], 2, funcy)) -> aaa


        # Now, we're going to create our quantities of interest. This is going to create
        #  a blank data frame (ys) that is as many rows as sss with as many columns as
        #  there are unique values of y_levs.

        ylabs <- paste0("y", y_levs)
        data.frame(matrix(ncol= length(ylabs),
                          nrow=nrow(sss),
                          dimnames=list(NULL, ylabs))) -> ys

        # This is kind of fun. To populate ys, the first/lowest values of ys is going to
        # just be assigned from the first threshold we calculated. The second through
        # the length(ylabs)-1 value is going to be the value from aaa minus the one
        # before it. The last one is simply going to be 1 - whatever that is.

        ys[1] <- aaa[1]
        ncol_min1 <- ncol(aaa)

        for(i in 2:ncol_min1) {
            ys[i] <- aaa[i] - aaa[i-1]

        }

        ys[ncol(ys)] <- 1 - aaa[ncol(aaa)]

        if(original_scale == TRUE & the_link == "probit") {

           ys <- apply(ys, 2, function(x) {qnorm(x)})
        } else if(original_scale == TRUE & the_link == "logit") {

            ys <- apply(ys, 2, function(x) {log(x / (1 - x))})
        }

        as_tibble(data.frame(sim = sss$sim, ys)) -> the_sims

    }


    return(the_sims)
}
