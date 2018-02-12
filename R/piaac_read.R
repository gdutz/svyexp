#' Reads PIAAC data
#'
#' @param datapath Path to the PIAAC data. A string.
#' @param cntry Three-letter country code (ISO3166-1 alpha-3). A string.
#'
#' @return A tibble, imported by the haven package.
#' @export
#'
#' @examples
#' d <- piaac_read("~/data/", "usa")
piaac_read <- function(datapath, cntry) {
    p <- paste0(datapath, "prg", cntry, "p1.sav")
    d <- haven::read_spss(p, user_na = TRUE)
    return(d)
}
