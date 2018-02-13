#' Linear Regression with PIAAC data
#'
#' @param datapath Path to the PIAAC data. A string.
#' @param countries Vector with three-letter country codes (ISO3166-1 alpha-3).
#' @param variables Vector with variable names.
#' @param formula Formula for the linear model.
#' @param pv One of "pvlit", "pvnum", "pvpsl" or none.
#' @param FUN Function name for data tidying.
#' @param ... Other Arguments passed to FUN or survey::svyglm.
#'
#' @return List of regression results. Individual list elements are to be handled with mitools.
#' @export
#'
#' @examples
#' results <- piaac_glm("~/data/", c("deu", "usa"), c("gender"), pvlit ~ gender)
#' results <- piaac_glm("~/data/", c("deu", "usa"), c("gender"), pvlit ~ gender, FUN = tidy_data)
piaac_glm <- function(datapath, countries, variables, formula,
                      pv = NULL, FUN = piaac_tidy, ...) {

    include_pv <- ifelse (is.null(pv) == TRUE, FALSE, TRUE)
    cat(include_pv)

    pb <- txtProgressBar(min = 0, max = length(countries), style = 3)

    results <- list()

    for (i in seq_along(countries)) {

        d <- piaac_read(datapath, countries[[i]])
        names(d) <- tolower(names(d))

        if (include_pv == TRUE) {
            pv_exp <- paste0("^", pv, "[0-9]+")
            d <- dplyr::select(d, dplyr::one_of(variables), cntryid, vemethod, dplyr::matches(pv_exp), dplyr::contains("spfwt"))
        }
        else {
            d <- dplyr::select(d, dplyr::one_of(variables), cntryid, vemethod, dplyr::contains("spfwt"))
        }

        d <- FUN(d, variables, ...) # call function for data tidying

        # Methode für replicate weights herausfinden
        jk.method <- unique(d$vemethod)                     # is there only one resampling technique?
        stopifnot(length(jk.method) == 1)                   # if not, stop
        jk.method <- trimws(as.character(jk.method[1]))     # getting resampling method
        if (jk.method == 'JK2' ) {jk.method <- "JKn"}       # setting JK2 = JKn for survey package

        if (include_pv == FALSE) {
            sd <- survey::svrepdesign(
                weights = ~ spfwt0,
                repweights = "spfwt[1-9]",
                rscales = rep(1, 80),
                scale = ifelse(jk.method == 'JKn', 1, 79/80 ), # see technical report
                type = jk.method,
                data = d)

            # Regress and save results in list
            results[[countries[[i]]]] <- survey::svyglm(formula, sd,...)
        }
        else if (include_pv == TRUE) {
            # Vorbereitungen für survey und mitools
            lit <- grep(pv_exp, colnames(d), value=TRUE)# literacy by variable name
            pv.non <- names(d)[!(names(d) %in% lit)]
            d.temp <- list()

            # build a new list containing data.frames. Each data frame contains ONE plausible value
            for (j in seq_along(lit)){

                # create temporary (temp) data.frame with one pv
                # and all non-pv variables
                temp <- d[, c(pv.non , lit[j])]

                # delete number at the end of pvs and levels
                pv.temp <- names(temp)==paste0(pv, j)
                names(temp)[pv.temp] <- pv

                # save the temp data.frame with nicer name
                d.temp[[j]] <- temp

                # remove temporary objects
                rm(temp, pv.temp)
            }

            # survey design object
            sd <- survey::svrepdesign(
                weights = ~spfwt0,
                repweights = "spfwt[1-9]",
                rscales = rep(1, 80),
                scale = ifelse(jk.method == 'JKn', 1, 79/80 ), # see technical report
                type = jk.method,
                data = mitools::imputationList(d.temp))

            # Regress and save results in list
            results[[countries[[i]]]] <- with(sd, survey::svyglm(formula, ...))
            rm(d, d.temp, j, pv.non, lit)
        }

        setTxtProgressBar(pb, i)
        rm(jk.method, i, sd)
    }
    close(pb)
    rm(pb)

    return(results)
}
