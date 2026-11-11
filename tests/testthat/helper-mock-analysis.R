# A mock `analysis_data` with known effects (ANALYSIS_PLAN.md, Step 1 task 8),
# for testing the analysis engine without /data. Six fictional datasets cover
# the shapes the engine must handle: two panels (the same participants at two
# waves), two repeated cross-sections (new participants at each wave) and two
# single occasions (wave NA), one of them in two countries. Columns use real
# dataschema rows and values, and the table is factorised as the pipeline does.
#
# One outcome per score type, each with a known per-dataset effect drawn
# around a known mean (the scale is in `truth`):
#
# - continuous `mh_dep_self`: SD per hour of `st_game`
# - ordinal `wb_selfratedhealth_self`: proportional-odds log odds per hour of
#   `st_watch`, towards later levels (worse health)
# - binary `beh_alcohol_self`: log odds per hour of `st_socmedia`, which only
#   four datasets measure
# - diagnosis `mh_anx_diagnosed`: log odds of "Currently diagnosed" per hour of
#   `st_game`
# - nominal `wb_bodyimage`: no effect
make_mock_analysis_data <- function(n_per_wave = 400, seed = 20261109) {
  withr::local_seed(seed)

  designs <- tibble::tibble(
    dataset_id = as.character(9991:9996),
    dataset_name = paste("Mock Study", LETTERS[1:6]),
    design = c("panel", "panel", "repeated", "repeated", "single", "single"),
    countries = list(
      "Australia",
      "England",
      "Australia",
      "Canada",
      "New Zealand",
      c("Ireland", "Scotland")
    ),
    has_socmedia = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  )
  means <- c(
    mh_dep_self = 0.10,
    wb_selfratedhealth_self = 0.15,
    beh_alcohol_self = 0.20,
    mh_anx_diagnosed = 0.10
  )
  truth <- tidyr::expand_grid(
    dataset_id = designs$dataset_id,
    outcome = names(means)
  )
  truth$exposure <- unname(c(
    mh_dep_self = "st_game",
    wb_selfratedhealth_self = "st_watch",
    beh_alcohol_self = "st_socmedia",
    mh_anx_diagnosed = "st_game"
  )[truth$outcome])
  truth$scale <- unname(c(
    mh_dep_self = "sd_per_hour",
    wb_selfratedhealth_self = "log_odds_per_hour",
    beh_alcohol_self = "log_odds_per_hour",
    mh_anx_diagnosed = "log_odds_per_hour"
  )[truth$outcome])
  truth$mean_effect <- unname(means[truth$outcome])
  truth$effect <- truth$mean_effect + stats::rnorm(nrow(truth), 0, 0.03)
  no_socmedia <- designs$dataset_id[!designs$has_socmedia]
  truth <- truth[
    !(truth$outcome == "beh_alcohol_self" & truth$dataset_id %in% no_socmedia),
  ]

  effect_of <- function(id, outcome) {
    truth$effect[truth$dataset_id == id & truth$outcome == outcome]
  }

  one_dataset <- function(i) {
    d <- designs[i, ]
    waves <- if (d$design == "single") NA_character_ else c("1", "2")
    n_people <- if (d$design == "panel") {
      n_per_wave
    } else {
      n_per_wave * length(waves)
    }
    people <- tibble::tibble(
      participant_id = sprintf("%s-%04d", d$dataset_id, seq_len(n_people)),
      sex = sample(c("Female", "Male"), n_people, replace = TRUE),
      base_age = stats::runif(n_people, 9, 15),
      country = sample(d$countries[[1]], n_people, replace = TRUE)
    )
    rows <- if (d$design == "panel") {
      tidyr::expand_grid(people, wave = waves)
    } else {
      people$wave <- rep(waves, length.out = n_people)
      people
    }
    n <- nrow(rows)
    wave_index <- ifelse(is.na(rows$wave), 0, as.numeric(rows$wave) - 1)
    rows$age_years <- rows$base_age + 2 * wave_index
    rows$data_year <- 2015 + 2 * wave_index

    rows$st_watch <- pmin(stats::rgamma(n, shape = 2, scale = 1), 24)
    rows$st_game <- pmin(stats::rgamma(n, shape = 1.5, scale = 1), 24)
    rows$st_socmedia <- if (d$has_socmedia) {
      pmin(stats::rgamma(n, shape = 1.5, scale = 1.2), 24)
    } else {
      NA_real_
    }

    rows$mh_dep_self <- effect_of(d$dataset_id, "mh_dep_self") *
      rows$st_game +
      0.05 * (rows$age_years - 12) +
      stats::rnorm(n)

    latent <- effect_of(d$dataset_id, "wb_selfratedhealth_self") *
      rows$st_watch +
      stats::rlogis(n)
    health_levels <- c("Excellent", "Very good", "Good", "Fair", "Poor")
    rows$wb_selfratedhealth_self <- health_levels[
      findInterval(latent, c(-Inf, -0.5, 0.8, 2, 3.2, Inf))
    ]

    rows$beh_alcohol_self <- if (d$has_socmedia) {
      p <- stats::plogis(
        -1.5 + effect_of(d$dataset_id, "beh_alcohol_self") * rows$st_socmedia
      )
      ifelse(stats::runif(n) < p, "TRUE", "FALSE")
    } else {
      NA_character_
    }

    p_dx <- stats::plogis(
      -3 + effect_of(d$dataset_id, "mh_anx_diagnosed") * rows$st_game
    )
    rows$mh_anx_diagnosed <- ifelse(
      stats::runif(n) < p_dx,
      "Currently diagnosed",
      sample(
        c("Never diagnosed", "Previously diagnosed"),
        n,
        replace = TRUE,
        prob = c(0.95, 0.05)
      )
    )
    rows$wb_bodyimage <- sample(
      c(
        "Much too thin",
        "A bit too thin",
        "About right",
        "A bit too fat",
        "Much too fat"
      ),
      n,
      replace = TRUE,
      prob = c(0.05, 0.15, 0.55, 0.2, 0.05)
    )

    rows$survey_weight <- if (i %% 2 == 1) stats::runif(n, 0.5, 2) else NA_real_
    rows$survey_cluster <- sprintf(
      "%s-s%02d",
      d$dataset_id,
      sample(1:20, n, TRUE)
    )
    rows$rct_arm <- "Not a randomised trial"

    tibble::tibble(
      dataset_id = d$dataset_id,
      dataset_name = d$dataset_name,
      participant_id = rows$participant_id,
      wave = rows$wave,
      st_measure_id = "primary",
      data_year = rows$data_year,
      age_years = rows$age_years,
      sex = rows$sex,
      country = rows$country,
      survey_weight = rows$survey_weight,
      survey_cluster = rows$survey_cluster,
      rct_arm = rows$rct_arm,
      st_watch = rows$st_watch,
      st_game = rows$st_game,
      st_socmedia = rows$st_socmedia,
      mh_dep_self = rows$mh_dep_self,
      wb_selfratedhealth_self = rows$wb_selfratedhealth_self,
      beh_alcohol_self = rows$beh_alcohol_self,
      mh_anx_diagnosed = rows$mh_anx_diagnosed,
      wb_bodyimage = rows$wb_bodyimage
    )
  }

  data <- dplyr::bind_rows(lapply(seq_len(nrow(designs)), one_dataset))
  list(
    data = factorise_analysis_data(data),
    truth = truth,
    designs = designs[, c("dataset_id", "dataset_name", "design")]
  )
}
