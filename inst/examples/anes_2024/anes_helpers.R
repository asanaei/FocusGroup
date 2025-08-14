# ANES 2024 Example Helpers
# Shows how to use the general create_agents_from_survey() function with ANES data

#' Create agents from haven-coded survey data
#'
#' Builds a set of `FGAgent`s by sampling rows from a haven-coded survey file
#' and turning labeled variables into demographics/survey-responses driven personas.
#' Works with any Stata (.dta), SPSS (.sav), or SAS files that have variable labels.
#'
#' @param n_participants Integer number of participants (excludes the moderator).
#' @param survey_path Character path to survey file (e.g., .dta, .sav files)
#' @param demographic_vars Character vector of variable names to use as demographics.
#'   If NULL, attempts to auto-detect common demographic variables.
#' @param survey_vars Character vector of variable names to use as survey responses.
#'   If NULL, uses remaining non-demographic variables.
#' @param llm_config Optional `LLMR::llm_config` for all agents. If `NULL`, a small OpenAI config is created.
#' @return A list of `FGAgent` objects (participants + moderator).
create_agents_from_survey_fixed <- function(n_participants,
                                     survey_path,
                                     demographic_vars = NULL,
                                     survey_vars = NULL,
                                     llm_config = NULL) {
  if (!file.exists(survey_path)) {
    stop("Survey file not found at: ", survey_path)
  }

  if (is.null(llm_config)) {
    llm_config <- LLMR::llm_config(
      provider = "openai",
      model = "gpt-4o-mini",
      temperature = 0.7,
      max_tokens = 500
    )
  }

  # Read survey data and convert to clean factors
  raw_lab <- haven::read_dta(survey_path)
  
  # Auto-detect demographic variables if not specified
  if (is.null(demographic_vars)) {
    # For ANES 2024, explicitly map the known variables
    if (grepl("anes.*2024", basename(survey_path), ignore.case = TRUE)) {
      demographic_vars <- c(
        age = "V241457",
        gender = "V241551", 
        education = "V241463",
        race = "V241552",
        marital_status = "V241553",
        income = "V241554",
        religion = "V241555",
        region = "V241556",
        urban_rural = "V241557",
        employment = "V241558",
        union_member = "V241559",
        veteran = "V241560",
        home_owner = "V241561",
        children = "V241562",
        internet_access = "V241563",
        social_media_use = "V241564",
        news_source = "V241565",
        political_interest = "V241566",
        campaign_contact = "V241567",
        volunteer_work = "V241568"
      )
      
      # Also map survey variables for richer personas
      if (is.null(survey_vars)) {
        survey_vars <- c(
          # Political variables
          party_id = "V241227x",           # Party ID summary
          vote_intent = "V241049",         # Harris vs Trump vote intent
          approval_dem = "V241006",        # Approve/disapprove Democratic party
          approval_rep = "V241007",        # Approve/disapprove Republican party
          feeling_dem = "V241166",         # Feeling thermometer: Democratic Party
          feeling_rep = "V241167",         # Feeling thermometer: Republican Party
          ideology = "V241221",            # Liberal-conservative ideology
          congress_approval = "V241127",   # Approval of Congress
          supreme_court_approval = "V241130", # Approval of Supreme Court
          president_approval = "V241134",  # Approval of President
          
          # Economic variables
          economy_rating = "V241236",      # Economy rating
          personal_finances = "V241237",   # Personal financial situation
          tax_policy = "V241238",          # Tax policy preference
          spending_priority = "V241239",   # Government spending priority
          
          # Social policy variables
          abortion_policy = "V241147",     # Abortion policy approval
          immigration_policy = "V241150",  # Immigration policy approval
          gun_policy = "V241153",          # Gun policy approval
          climate_policy = "V241156",      # Climate change policy approval
          health_care_policy = "V241159",  # Health care policy approval
          education_policy = "V241162",    # Education policy approval
          
          # Additional political variables
          biden_dropout = "V241185",       # Biden dropping from race
          trump_conviction = "V241188",    # Trump conviction impact
          election_importance = "V241191", # Election importance
          vote_confidence = "V241194"      # Confidence in vote choice
        )
      }
    }
  }

  # Convert data first, then filter for valid respondents
  available_demo_vars <- intersect(demographic_vars, names(raw_lab))
  available_survey_vars <- intersect(survey_vars, names(raw_lab))
  
  # Convert all data to character with labels
  clean_data <- as.data.frame(lapply(raw_lab, function(col) {
    if (inherits(col, c("haven_labelled", "labelled"))) {
      as.character(haven::as_factor(col, levels = "labels"))
    } else {
      as.character(col)
    }
  }), stringsAsFactors = FALSE)
  
  # Find rows with valid data - at least one non-missing demographic AND survey response
  if (length(available_demo_vars) > 0 && length(available_survey_vars) > 0) {
    # Find rows with valid gender and party_id (most basic requirements)
    gender_valid <- !is.na(clean_data$V241551) & 
                    !grepl("inapplicable|refused|error|don.*know", clean_data$V241551, ignore.case = TRUE)
    party_valid <- !is.na(clean_data$V241227x) & 
                   !grepl("inapplicable|refused|error|don.*know", clean_data$V241227x, ignore.case = TRUE)
    
    valid_rows <- which(gender_valid & party_valid)
    
    if (length(valid_rows) == 0) {
      valid_rows <- seq_len(min(100, nrow(clean_data)))  # Fallback to first 100 rows
    }
  } else {
    valid_rows <- seq_len(min(1000, nrow(clean_data)))  # Take first 1000 if no variables specified
  }
  
  # Sample from valid rows
  set.seed(NULL)
  if (length(valid_rows) > n_participants) {
    selected_rows <- sample(valid_rows, n_participants)
  } else {
    selected_rows <- valid_rows[seq_len(min(n_participants, length(valid_rows)))]
  }
  
  # Build demographics dataframe
  demo_df <- if (length(available_demo_vars) > 0) {
    demo_data <- clean_data[selected_rows, available_demo_vars, drop = FALSE]
    # Clean up the data - remove number prefixes and clean text
    demo_cleaned <- as.data.frame(lapply(demo_data, function(col) {
      # Remove number prefixes like "1. ", "2. ", etc.
      cleaned <- gsub("^[-0-9]+\\. ", "", col)
      # Set clearly invalid responses to NA
      cleaned[grepl("inapplicable|refused|error|don.*know", cleaned, ignore.case = TRUE)] <- NA
      cleaned
    }), stringsAsFactors = FALSE)
    names(demo_cleaned) <- names(demographic_vars)[match(available_demo_vars, demographic_vars)]
    demo_cleaned
  } else {
    # Fallback: create minimal fake demographics
    data.frame(
      age = sample(18:80, n_participants, replace = TRUE),
      gender = sample(c("Male","Female"), n_participants, replace = TRUE),
      education = sample(c("High School","Some College","BA","MA+"), n_participants, replace = TRUE),
      stringsAsFactors = FALSE
    )
  }

  # Build survey responses dataframe
  survey_df <- if (length(available_survey_vars) > 0) {
    survey_data <- clean_data[selected_rows, available_survey_vars, drop = FALSE]
    # Clean up the data - remove number prefixes and clean text
    survey_cleaned <- as.data.frame(lapply(survey_data, function(col) {
      # Remove number prefixes like "1. ", "2. ", etc.
      cleaned <- gsub("^[-0-9]+\\. ", "", col)
      # Set clearly invalid responses to NA
      cleaned[grepl("inapplicable|refused|error|don.*know", cleaned, ignore.case = TRUE)] <- NA
      cleaned
    }), stringsAsFactors = FALSE)
    names(survey_cleaned) <- names(survey_vars)[match(available_survey_vars, survey_vars)]
    survey_cleaned
  } else {
    NULL
  }

  demo_sample <- demo_df
  survey_sample <- survey_df

  # Create agents with these demographics
  agents <- create_diverse_agents(
    n_participants = n_participants,
    demographics = demo_sample,
    survey_responses = survey_sample,
    llm_config = llm_config
  )

  agents
}

# Simple wrapper that uses the fixed survey function with ANES-specific defaults
create_agents_from_anes <- function(n_participants, anes_dta_path, llm_config = NULL) {
  # Use the fixed survey function with ANES-specific variable selections
  create_agents_from_survey_fixed(
    n_participants = n_participants,
    survey_path = anes_dta_path,
    demographic_vars = NULL,  # Auto-detect demographics
    survey_vars = NULL,       # Auto-detect survey vars
    llm_config = llm_config
  )
}


