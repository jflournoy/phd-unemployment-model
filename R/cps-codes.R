# CPS Code Constants and Lookup Functions
#
# Centralized, tested definitions of IPUMS CPS codes to prevent bugs from
# hardcoded magic numbers scattered across the codebase.
#
# All codes sourced from IPUMS CPS documentation:
# https://cps.ipums.org/cps-action/variables/EMPSTAT
# https://cps.ipums.org/cps-action/variables/EDUC

# ==============================================================================
# Employment Status Codes (EMPSTAT)
# ==============================================================================

#' Get EMPSTAT Codes for Employed Persons
#'
#' Returns the IPUMS CPS EMPSTAT codes that indicate employment.
#'
#' @return Integer vector of EMPSTAT codes for employed persons
#'
#' @details
#' Employed codes:
#' - 10: At work
#' - 12: Has job, not at work last week
#'
#' @examples
#' employed_codes <- get_employed_codes()
#' cps_data[cps_data$EMPSTAT %in% employed_codes, ]
#'
#' @export
get_employed_codes <- function() {
  c(10L, 12L)
}

#' Get EMPSTAT Codes for Unemployed Persons
#'
#' Returns the IPUMS CPS EMPSTAT codes that indicate unemployment
#' (in labor force, actively seeking work).
#'
#' @return Integer vector of EMPSTAT codes for unemployed persons
#'
#' @details
#' Unemployed codes:
#' - 20: Unemployed
#' - 21: Unemployed, experienced worker
#' - 22: Unemployed, new worker
#'
#' @examples
#' unemployed_codes <- get_unemployed_codes()
#' cps_data[cps_data$EMPSTAT %in% unemployed_codes, ]
#'
#' @export
get_unemployed_codes <- function() {
  c(20L, 21L, 22L)
}

#' Get EMPSTAT Codes for Labor Force Participants
#'
#' Returns the IPUMS CPS EMPSTAT codes for persons in the labor force
#' (employed + unemployed).
#'
#' @return Integer vector of EMPSTAT codes for labor force participants
#'
#' @details
#' Labor force = employed + unemployed
#'
#' @examples
#' lf_codes <- get_labor_force_codes()
#' labor_force_data <- cps_data[cps_data$EMPSTAT %in% lf_codes, ]
#'
#' @export
get_labor_force_codes <- function() {
  c(get_employed_codes(), get_unemployed_codes())
}

#' Get EMPSTAT Codes for Not in Labor Force
#'
#' Returns the IPUMS CPS EMPSTAT codes for persons not in the labor force.
#'
#' @return Integer vector of EMPSTAT codes for NILF persons
#'
#' @details
#' Not in labor force codes (30+):
#' - 30: Not in labor force
#' - 31: Retired
#' - 32: Disabled
#' - 33: Unable to work
#' - 34: School
#' - 35: Taking care of house or family
#' - 36: Other
#'
#' @export
get_not_in_labor_force_codes <- function() {
  c(30L, 31L, 32L, 33L, 34L, 35L, 36L)
}

#' Check if EMPSTAT Code Indicates Employment
#'
#' @param empstat Integer vector of EMPSTAT codes
#'
#' @return Logical vector indicating whether each code represents employment
#'
#' @examples
#' is_employed(c(10, 20, 30))  # TRUE, FALSE, FALSE
#'
#' @export
is_employed <- function(empstat) {
  empstat %in% get_employed_codes()
}

#' Check if EMPSTAT Code Indicates Unemployment
#'
#' @param empstat Integer vector of EMPSTAT codes
#'
#' @return Logical vector indicating whether each code represents unemployment
#'
#' @examples
#' is_unemployed(c(10, 20, 30))  # FALSE, TRUE, FALSE
#'
#' @export
is_unemployed <- function(empstat) {
  empstat %in% get_unemployed_codes()
}

#' Check if EMPSTAT Code Indicates Labor Force Participation
#'
#' @param empstat Integer vector of EMPSTAT codes
#'
#' @return Logical vector indicating whether each code represents labor force participation
#'
#' @examples
#' is_in_labor_force(c(10, 20, 30))  # TRUE, TRUE, FALSE
#'
#' @export
is_in_labor_force <- function(empstat) {
  empstat %in% get_labor_force_codes()
}


# ==============================================================================
# Education Level Codes (EDUC)
# ==============================================================================

#' Get Supported EDUC Codes
#'
#' Returns the IPUMS CPS EDUC codes for the 7 education levels we analyze.
#'
#' @return Integer vector of supported EDUC codes
#'
#' @details
#' Supported education codes:
#' - 2: Less than high school
#' - 73: High school diploma or GED
#' - 81: Some college but no degree
#' - 111: Bachelor's degree
#' - 123: Master's degree
#' - 124: Professional school degree (MD, JD, DVM, etc.)
#' - 125: Doctoral degree (PhD)
#'
#' @export
get_education_codes <- function() {
  c(2L, 73L, 81L, 111L, 123L, 124L, 125L)
}

#' Get Education Code to Label Mapping
#'
#' Returns a named character vector mapping EDUC codes to readable labels.
#'
#' @return Named character vector (names = codes, values = labels)
#'
#' @details
#' Maps IPUMS EDUC codes to our standardized education level labels:
#' - 2 -> "less_than_hs"
#' - 73 -> "high_school"
#' - 81 -> "some_college"
#' - 111 -> "bachelors"
#' - 123 -> "masters"
#' - 124 -> "professional"
#' - 125 -> "phd"
#'
#' @examples
#' educ_map <- get_education_code_map()
#' educ_map["111"]  # "bachelors"
#'
#' @export
get_education_code_map <- function() {
  c(
    "2" = "less_than_hs",
    "73" = "high_school",
    "81" = "some_college",
    "111" = "bachelors",
    "123" = "masters",
    "124" = "professional",
    "125" = "phd"
  )
}

#' Convert EDUC Code to Label
#'
#' @param code Integer vector of EDUC codes
#'
#' @return Character vector of education labels (NA for unknown codes)
#'
#' @examples
#' get_education_label(c(111, 123, 125))
#' # "bachelors" "masters" "phd"
#'
#' @export
get_education_label <- function(code) {
  educ_map <- get_education_code_map()
  unname(educ_map[as.character(code)])
}

#' Convert Education Label to EDUC Code
#'
#' @param label Character vector of education labels
#'
#' @return Integer vector of EDUC codes (NA for unknown labels)
#'
#' @examples
#' get_education_code(c("bachelors", "masters", "phd"))
#' # 111 123 125
#'
#' @export
get_education_code <- function(label) {
  educ_map <- get_education_code_map()
  # Reverse lookup
  code_lookup <- stats::setNames(as.integer(names(educ_map)), educ_map)
  unname(code_lookup[label])
}

#' Get Ordered Education Levels
#'
#' Returns education levels in order from least to most educated.
#'
#' @return Character vector of education levels in order
#'
#' @details
#' Order (least to most educated):
#' 1. less_than_hs
#' 2. high_school
#' 3. some_college
#' 4. bachelors
#' 5. masters
#' 6. professional
#' 7. phd
#'
#' Use this to create ordered factors:
#' `factor(education, levels = get_education_levels(), ordered = TRUE)`
#'
#' @export
get_education_levels <- function() {
  c(
    "less_than_hs",
    "high_school",
    "some_college",
    "bachelors",
    "masters",
    "professional",
    "phd"
  )
}

#' Check if EDUC Code Represents Advanced Degree
#'
#' @param code Integer vector of EDUC codes
#'
#' @return Logical vector indicating whether each code is an advanced degree
#'
#' @details
#' Advanced degrees: masters (123), professional (124), phd (125)
#'
#' @examples
#' is_advanced_degree(c(111, 123, 125))  # FALSE, TRUE, TRUE
#'
#' @export
is_advanced_degree <- function(code) {
  code %in% c(123L, 124L, 125L)
}

#' Check if EDUC Code Represents College Degree
#'
#' @param code Integer vector of EDUC codes
#'
#' @return Logical vector indicating whether each code is a college degree
#'
#' @details
#' College degrees: bachelors (111) and all advanced degrees (123, 124, 125)
#'
#' @examples
#' is_college_degree(c(81, 111, 125))  # FALSE, TRUE, TRUE
#'
#' @export
is_college_degree <- function(code) {
  code %in% c(111L, 123L, 124L, 125L)
}


# ==============================================================================
# Documentation and Descriptions
# ==============================================================================

#' Get Human-Readable Description for EMPSTAT Code
#'
#' @param code Integer EMPSTAT code
#'
#' @return Character string describing the employment status
#'
#' @examples
#' get_empstat_description(10)  # "At work"
#'
#' @export
get_empstat_description <- function(code) {
  descriptions <- c(
    "0" = "NIU (Not in universe)",
    "1" = "Armed Forces",
    "10" = "At work",
    "12" = "Has job, not at work last week",
    "20" = "Unemployed",
    "21" = "Unemployed, experienced worker",
    "22" = "Unemployed, new worker",
    "30" = "Not in labor force",
    "31" = "Retired",
    "32" = "Disabled",
    "33" = "Unable to work",
    "34" = "School",
    "35" = "Taking care of house or family",
    "36" = "Other"
  )

  descriptions[as.character(code)]
}

#' Get Human-Readable Description for EDUC Code
#'
#' @param code Integer EDUC code
#'
#' @return Character string describing the education level
#'
#' @examples
#' get_education_description(124)  # "Professional school degree (MD, JD, DVM, etc.)"
#'
#' @export
get_education_description <- function(code) {
  descriptions <- c(
    "2" = "Less than high school (None or preschool through Grade 11)",
    "73" = "High school diploma or GED",
    "81" = "Some college but no degree",
    "111" = "Bachelor's degree",
    "123" = "Master's degree (MA, MS, MEng, MEd, MSW, MBA, etc.)",
    "124" = "Professional school degree (MD, JD, DDS, DVM, LLB, etc.)",
    "125" = "Doctoral degree (PhD, EdD, etc.)"
  )

  descriptions[as.character(code)]
}


# ==============================================================================
# Validation Helpers
# ==============================================================================

#' Validate EMPSTAT Codes
#'
#' Checks that all EMPSTAT codes are valid IPUMS values.
#'
#' @param empstat Integer vector of EMPSTAT codes
#'
#' @return TRUE if all valid, warns about invalid codes
#'
#' @examples
#' validate_empstat_codes(c(10, 20, 30))  # TRUE
#'
#' @export
validate_empstat_codes <- function(empstat) {
  # Known valid EMPSTAT codes
  valid_codes <- c(0L, 1L, 10L, 12L, 20L, 21L, 22L, 30L, 31L, 32L, 33L, 34L, 35L, 36L)

  invalid <- empstat[!empstat %in% valid_codes]

  if (length(invalid) > 0) {
    warning(sprintf(
      "Found %d invalid EMPSTAT codes: %s",
      length(invalid),
      paste(unique(invalid), collapse = ", ")
    ))
    return(FALSE)
  }

  TRUE
}

#' Validate Education Codes
#'
#' Checks that all EDUC codes are in our supported set.
#'
#' @param educ Integer vector of EDUC codes
#'
#' @return TRUE if all valid, warns about unmapped codes
#'
#' @examples
#' validate_education_codes(c(111, 123, 125))  # TRUE
#'
#' @export
validate_education_codes <- function(educ) {
  valid_codes <- get_education_codes()

  unmapped <- educ[!educ %in% valid_codes]

  if (length(unmapped) > 0) {
    warning(sprintf(
      "Found %d unmapped EDUC codes: %s\nSupported codes: %s",
      length(unmapped),
      paste(unique(unmapped), collapse = ", "),
      paste(valid_codes, collapse = ", ")
    ))
    return(FALSE)
  }

  TRUE
}
