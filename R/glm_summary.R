#' Summary statistics for Linear Models
#'
#' @param object svyglm result object.
#' @param ... Any further parameters.
#' @param print Logical, if summary table should be printed.
#'
#' @return Dataframe with summary staistics
#' @export
#'
#' @examples
#' glm_summary(mitools::MIcombine(results$deu))   # Results for German dataset
#' glm_summary(mitools::MIcombine(results[[2]]))  # Results for second dataset
glm_summary <- function(object, ..., print = TRUE){
    out             <-data.frame(results=coef(object), se=sqrt(diag(vcov(object))))
    t <- coef(object)/(sqrt(diag(vcov(object))))
    out$"t value"   <- t
    p <- 2*pt(-abs(t), object$df)
    out$"Pr(>|t|)"  <- signif(p, 3)
    out$"Signif."   <- ifelse(p <= 0.001, "***",
                              ifelse(p <= 0.01, "**",
                                     ifelse(p <= 0.05, "*",
                                            ifelse(p <= 0.01, ".", "ns"))))

    if (print == TRUE) {
        cat("Multiple imputation results:\n")
        lapply(object$call, function(a) {cat("      ");print(a)})
        print(out,...)
        cat("---\nSignif. codes:  ns: P > 0.05, *: P <= 0.05, **: P <= 0.01, ***: P <= 0.001")
    }
    else {
        return(out)
    }
}
