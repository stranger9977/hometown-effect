`%||%` <- function(a, b) if (is.null(a)) b else a

espn_athlete_url <- function(espn_id) {
  sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/athletes/%s",
          espn_id)
}

parse_espn_athlete <- function(path) {
  id_from_file <- sub("[.]json$", "", basename(path))
  j <- tryCatch(jsonlite::read_json(path), error = function(e) NULL)
  if (is.null(j) || is.null(j$id)) {
    return(tibble::tibble(
      espn_id = id_from_file, full_name = NA_character_,
      birth_city = NA_character_, birth_state = NA_character_,
      birth_country = NA_character_, espn_dob = NA_character_,
      parse_ok = FALSE))
  }
  bp <- j$birthPlace %||% list()
  dob <- j$dateOfBirth %||% NA_character_
  tibble::tibble(
    espn_id       = as.character(j$id),
    full_name     = j$fullName %||% NA_character_,
    birth_city    = bp$city %||% NA_character_,
    birth_state   = bp$state %||% NA_character_,
    birth_country = bp$country %||% NA_character_,
    espn_dob      = if (is.na(dob)) NA_character_ else substr(dob, 1, 10),
    parse_ok      = TRUE)
}
