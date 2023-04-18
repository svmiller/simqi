#' Get simulations from a model object
#'
#' @description \code{sim_qi()} is a function to simulate quantities of interest
#' from a regression model
#'
#' @param mod a model object
#' @param nsim number of simulations to be run, defaults to 1,000
#' @param newdata A data frame with a hypothetical prediction grid.
#' If absent, defaults to the model frame.
#'
#' @return \code{sim_qi()()} returns a data frame (as a \code{tibble}) with the
#' quantities of interest and identifying information about the particular
#' simulation number.
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

sim_qi <- function(mod, nsim = 1000, newdata) {
    # if (!identical(class(mod), "lm")) {
    #     stop("Hold on a second.")
    # }

    if(identical(class(mod), "lm")) {

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
    }


    return(the_sims)
}
