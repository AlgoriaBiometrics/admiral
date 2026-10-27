#' Compute Factor for Value Imputations When Character Value Contains < or >
#'
#' Function to compute factor for value imputation when character
#' value contains < or >.  The factor is calculated using the number of decimals.
#' If there are no decimals, the factor is 1, otherwise the factor = 1/10^decimal
#' place.  For example, the factor for 100 = 1, the factor for 5.4 = 1/10^1,
#' the factor for 5.44 = 1/10^2.  This results in no additional false precision
#' added to the value.  This is an intermediate function.
#'
#' Derive an imputed value
#'
#' @param character_value_decimal Character value to determine decimal precision
#'
#' @return Decimal precision value to add or subtract
#'
#' @export
#'
#' @details This is a vector-oriented helper and is not usually called directly on a data
#' frame with `%>%`.
#'
#' @keywords com_bds_findings
#' @family com_bds_findings
#'
#' @examples
#' compute_qual_imputation_dec("<40.1")
#' compute_qual_imputation_dec(c("0.35", "1"))
compute_qual_imputation_dec <- function(character_value_decimal) {
  x <- str_trim(character_value_decimal)

  decimal <- case_when(
    str_detect(x, "\\.") ~ 1 / (10^(str_length(x) - str_locate(x, "\\.")[, 1])),
    TRUE ~ 1
  )

  unname(decimal)
}

#' Impute Numeric Values from Character Results with Qualifiers
#'
#' Converts character results to numeric values and optionally shifts results
#' with strict inequality qualifiers.
#'
#' @param character_value Character vector of numeric results, optionally prefixed
#' with a qualifier, for example, `c("<40", ">=40.1", "3")`.
#'
#' @param imputation_type Method used to derive numeric results:
#'
#' - `1`: Remove qualifiers and use `factor` as the additive offset for strict
#'   inequalities.
#' - `2`: Remove qualifiers and use the decimal precision of each original
#'   character result as the additive offset. See Details.
#'
#' @permitted `1`, `2`
#'
#' @param factor Numeric offset used when `imputation_type = 1`. It is added for
#' results with `>` and subtracted for results with `<`. It is ignored when
#' `imputation_type = 2`.
#'
#' @details
#' The characters `<`, `>`, and `=` are removed before conversion to numeric.
#' For results with a strict `>` qualifier, the offset is added; for results with
#' a strict `<` qualifier, it is subtracted. Results with `>=` or `<=`, and results
#' without an inequality qualifier, are converted without a shift.
#'
#' With `imputation_type = 2`, the offset is \eqn{10^{-d}}, where \eqn{d} is the
#' number of decimal places in the original character result, including trailing
#' zeros. For integer results, the offset is 1. For example, `"<40"` becomes 39,
#' `">40.1"` becomes 40.2, and `">40.10"` becomes 40.11.
#'
#' The appropriate imputation rule depends on the analysis specification.
#'
#' @return A numeric vector of imputed results.
#'
#' @export
#'
#'
#' @keywords com_bds_findings
#' @family com_bds_findings
#'
#' @examples
#' values <- c("<40", ">40.1", "<=40", ">=40.1", "3")
#' compute_qual_imputation(values)
#' compute_qual_imputation(values, factor = 0.5)
#' compute_qual_imputation(values, imputation_type = 2)
#'
#' # Trailing zeros retain the reported precision
#' compute_qual_imputation(c(">40.1", ">40.10"), imputation_type = 2)
compute_qual_imputation <- function(character_value, imputation_type = 1, factor = 0) {
  clean_value <- character_value

  # Identify strings with letters and set to NA immediately to avoid warning with
  # as.numeric() later down
  has_letters <- str_detect(clean_value, "[A-Za-z]")
  clean_value[has_letters] <- NA_character_

  # Remove symbols from the remaining valid strings
  clean_value <- gsub("=|>|<", "", clean_value)

  numeric_value <- as.numeric(clean_value)

  if (imputation_type == 1) {
    numeric_value <-
      replace_when(
        numeric_value,
        str_detect(character_value, ">") & !str_detect(character_value, "=") ~
          numeric_value + factor,
        str_detect(character_value, "<") & !str_detect(character_value, "=") ~
          numeric_value - factor
      )
  }

  if (imputation_type == 2) {
    numeric_value <-
      replace_when(
        numeric_value,
        str_detect(character_value, ">") & !str_detect(character_value, "=") ~
          numeric_value + compute_qual_imputation_dec(character_value),
        str_detect(character_value, "<") & !str_detect(character_value, "=") ~
          numeric_value - compute_qual_imputation_dec(character_value)
      )
  }

  numeric_value
}
