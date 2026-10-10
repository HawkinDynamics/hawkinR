# Tests for create_athletes and update_athletes input validation and JSON payload
# These test the data preparation layer without making API calls.

test_that("AddAthleteJSON handles optional columns correctly", {
  # Only required column: name
  df_minimal <- data.frame(name = "Test", stringsAsFactors = FALSE)
  json <- hawkinR:::AddAthleteJSON(df_minimal)
  parsed <- jsonlite::fromJSON(json)
  expect_equal(parsed$name, "Test")

  # With active column

  df_active <- data.frame(
    name = "Test",
    active = TRUE,
    stringsAsFactors = FALSE
  )
  json2 <- hawkinR:::AddAthleteJSON(df_active)
  parsed2 <- jsonlite::fromJSON(json2)
  expect_true(parsed2$active)
})

test_that("AddAthleteJSON handles NA values by excluding them", {
  df <- data.frame(
    name = "Test",
    image = NA_character_,
    stringsAsFactors = FALSE
  )
  json <- hawkinR:::AddAthleteJSON(df)
  parsed <- jsonlite::fromJSON(json)

  # NA image should not be included in the JSON
  expect_null(parsed$image)
})

test_that("UpdateAthleteJSON preserves id field", {
  df <- data.frame(
    id = c("id1", "id2"),
    name = c("Alice", "Bob"),
    active = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  json <- hawkinR:::UpdateAthleteJSON(df)
  parsed <- jsonlite::fromJSON(json)

  expect_equal(parsed$id, c("id1", "id2"))
  expect_equal(parsed$name, c("Alice", "Bob"))
  expect_equal(parsed$active, c(TRUE, FALSE))
})

test_that("UpdateAthleteJSON handles external properties for updates", {
  df <- data.frame(
    id = "athlete-1",
    position = "Forward",
    jersey = "10",
    stringsAsFactors = FALSE
  )
  json <- hawkinR:::UpdateAthleteJSON(df)
  parsed <- jsonlite::fromJSON(json)

  expect_equal(parsed$id, "athlete-1")
  expect_equal(parsed$external$jersey, "10")
  # position is a native field now, never an external property
  expect_equal(parsed$position, "Forward")
  expect_null(parsed$external$position)
})

# --- Native profile fields: position, sport, dob, height (CSE-150) ---

# Parse a payload without simplifying, so each athlete stays a named list and
# an omitted field is visibly absent rather than an NA in a data frame column.
parse_athletes <- function(json) {
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

# The update and create payloads built from the same data frame
both_payloads <- function(df) {
  list(hawkinR:::UpdateAthleteJSON(df), hawkinR:::AddAthleteJSON(df))
}

profile_df <- function(...) {
  data.frame(
    name = c("One", "Two"),
    position = c(" Forward ", "Guard"),
    sport = c("Basketball", "Soccer"),
    dob = c("1998-04-01", "2001-11-23"),
    height = c(190.5, 172),
    jersey = c("10", "22"),
    ...,
    stringsAsFactors = FALSE
  )
}

test_that("profile fields are native on create and never in external", {
  athletes <- parse_athletes(hawkinR:::AddAthleteJSON(profile_df()))

  expect_equal(athletes[[1]]$position, "Forward") # trimmed
  expect_equal(athletes[[1]]$sport, "Basketball")
  expect_equal(athletes[[1]]$dob, "1998-04-01")
  expect_equal(athletes[[1]]$height, 190.5)
  expect_equal(athletes[[2]]$height, 172)
  for (a in athletes) {
    expect_named(a$external, "jersey")
  }
})

test_that("profile fields are native on update and never in external", {
  df <- profile_df(id = c("id1", "id2"))
  athletes <- parse_athletes(hawkinR:::UpdateAthleteJSON(df))

  expect_equal(athletes[[2]]$id, "id2")
  expect_equal(athletes[[2]]$position, "Guard")
  expect_equal(athletes[[2]]$sport, "Soccer")
  expect_equal(athletes[[2]]$dob, "2001-11-23")
  expect_equal(athletes[[2]]$height, 172)
  for (a in athletes) {
    expect_named(a$external, "jersey")
  }
})

test_that("NA and blank profile values are omitted, not sent", {
  df <- data.frame(
    id = c("id1", "id2"),
    position = c(NA, "  "),
    sport = c("", NA),
    dob = c(NA, ""),
    height = c(NA, NA),
    stringsAsFactors = FALSE
  )
  for (json in both_payloads(df)) {
    for (a in parse_athletes(json)) {
      expect_false(any(c("position", "sport", "dob", "height") %in% names(a)))
      expect_length(a$external, 0)
    }
  }
})

test_that("lastTestedOn is never sent, in the body or in external", {
  df <- data.frame(
    id = "id1",
    name = "One",
    lastTestedOn = 1718000000,
    stringsAsFactors = FALSE
  )
  for (json in both_payloads(df)) {
    expect_false(grepl("lastTestedOn", json, fixed = TRUE))
  }
})

test_that("id is not sent on create, in the body or in external", {
  df <- data.frame(id = "id1", name = "One", stringsAsFactors = FALSE)
  athlete <- parse_athletes(hawkinR:::AddAthleteJSON(df))[[1]]
  expect_null(athlete$id)
  expect_length(athlete$external, 0)
})

test_that("a Date dob is sent as YYYY-MM-DD", {
  df <- data.frame(id = "id1", dob = as.Date("2001-12-31"))
  athlete <- parse_athletes(hawkinR:::UpdateAthleteJSON(df))[[1]]
  expect_equal(athlete$dob, "2001-12-31")
})

test_that("a POSIXct dob keeps its own calendar day, with no shift to UTC", {
  # 00:30 on 1 April in Auckland is still 31 March in UTC
  ahead <- as.POSIXct("1998-04-01 00:30:00", tz = "Pacific/Auckland")
  # 23:30 on 1 April in Los Angeles is already 2 April in UTC
  behind <- as.POSIXct("1998-04-01 23:30:00", tz = "America/Los_Angeles")
  expect_equal(format(ahead, format = "%Y-%m-%d", tz = "UTC"), "1998-03-31")
  expect_equal(format(behind, format = "%Y-%m-%d", tz = "UTC"), "1998-04-02")

  # A data frame column carries one time zone, so test each separately
  for (value in list(ahead, behind)) {
    df <- data.frame(id = "id1")
    df$dob <- value
    for (json in both_payloads(df)) {
      expect_equal(parse_athletes(json)[[1]]$dob, "1998-04-01")
    }
  }
})

test_that("a 4-digit year dob passes through as YYYY", {
  df_num <- data.frame(id = c("id1", "id2"), dob = c(1998, 2001))
  athletes <- parse_athletes(hawkinR:::UpdateAthleteJSON(df_num))
  expect_identical(athletes[[1]]$dob, "1998")
  expect_identical(athletes[[2]]$dob, "2001")

  df_chr <- data.frame(id = "id1", dob = "1987", stringsAsFactors = FALSE)
  json_chr <- hawkinR:::AddAthleteJSON(df_chr)
  expect_identical(parse_athletes(json_chr)[[1]]$dob, "1987")

  df_int <- data.frame(id = "id1", dob = 1975L)
  json_int <- hawkinR:::UpdateAthleteJSON(df_int)
  expect_identical(parse_athletes(json_int)[[1]]$dob, "1975")
})

test_that("height is rounded to 1 dp and numeric strings are accepted", {
  df <- data.frame(id = c("id1", "id2"), height = c(180.26, 175.04))
  athletes <- parse_athletes(hawkinR:::UpdateAthleteJSON(df))
  expect_equal(athletes[[1]]$height, 180.3)
  expect_equal(athletes[[2]]$height, 175)

  df_chr <- data.frame(id = "id1", height = "182.5", stringsAsFactors = FALSE)
  json_chr <- hawkinR:::UpdateAthleteJSON(df_chr)
  expect_equal(parse_athletes(json_chr)[[1]]$height, 182.5)

  df_bad <- data.frame(id = "id1", height = "tall", stringsAsFactors = FALSE)
  expect_error(hawkinR:::UpdateAthleteJSON(df_bad), "centimeters")
})

test_that("a height below 100 warns but is still sent", {
  df <- data.frame(
    id = c("id1", "id2"), name = c("One", "Two"), height = c(95, 180)
  )
  expect_warning(
    json <- hawkinR:::UpdateAthleteJSON(df),
    "below 100: One \\(95\\)"
  )
  athletes <- parse_athletes(json)
  expect_equal(athletes[[1]]$height, 95)

  expect_warning(hawkinR:::AddAthleteJSON(df), "241.3")
})

test_that("a height of 0 is not sent and a negative height is sent without the feet/inches warning", {
  df <- data.frame(id = c("id1", "id2"), height = c(0, -5))
  expect_no_warning(json <- hawkinR:::UpdateAthleteJSON(df))
  athletes <- parse_athletes(json)
  expect_false("height" %in% names(athletes[[1]]))
  # Out of range: left for the API to reject, not reported as feet/inches.
  expect_equal(athletes[[2]]$height, -5)
})

test_that("heights of 100 or more do not warn", {
  df <- data.frame(id = c("id1", "id2"), height = c(100, 250))
  expect_no_warning(hawkinR:::UpdateAthleteJSON(df))
})

# --- get_athletes() -> modify -> update_athletes() round trip ---

test_that("get_athletes output round-trips without junk external keys", {
  # Raw API shape: native profile fields, lastTestedOn, and external keys that
  # include a legacy "position" written by older hawkinR versions.
  raw <- data.frame(
    id = c("a1", "a2"),
    name = c("One", "Two"),
    active = c(TRUE, TRUE),
    image = c("https://img/1.png", NA),
    position = c("Forward", NA),
    dob = c("1998-04-01", "1990"),
    sport = c("Basketball", NA),
    height = c(190, 241.3),
    lastTestedOn = c(1718000000, NA),
    stringsAsFactors = FALSE
  )
  raw$teams <- list("t1", "t2")
  raw$groups <- list(NA, NA)
  raw$external <- list(
    list(jersey = "10", position = "Old Guard"),
    list(jersey = "22")
  )

  athletes <- hawkinR:::AthletePrep(raw, prefix = "")
  # The external "position" collides with the native column
  expect_true("position.1" %in% names(athletes))

  athletes$sport[2] <- "Soccer"
  expect_warning(
    json <- hawkinR:::UpdateAthleteJSON(athletes),
    "position.1"
  )
  sent <- parse_athletes(json)

  expect_equal(sent[[1]]$position, "Forward")
  expect_equal(sent[[1]]$dob, "1998-04-01")
  expect_equal(sent[[2]]$dob, "1990")
  expect_equal(sent[[2]]$sport, "Soccer")
  expect_null(sent[[2]]$position)
  for (a in sent) {
    expect_named(a$external, "jersey")
  }
  expect_false(grepl("lastTestedOn", json, fixed = TRUE))
})

# --- update_athletes() sends the payload on the wire ---

test_that("update_athletes sends profile fields natively in the request body", {
  skip_on_cran()

  auth <- HawkinAuth(config = HawkinConfig())
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- auth
  on.exit(.hawkin_env$active_conn <- old_conn)

  sent <- NULL
  mockery::stub(update_athletes, "httr2::req_perform", function(req, ...) {
    sent <<- req
    structure(list(), class = "httr2_response")
  })
  mockery::stub(update_athletes, "httr2::resp_status", 200L)
  mockery::stub(update_athletes, "httr2::resp_body_json", list(
    data = data.frame(name = "One"),
    hasFailures = FALSE
  ))

  df <- data.frame(
    id = "id1",
    name = "One",
    position = "Forward",
    height = 190.5,
    lastTestedOn = 1718000000,
    jersey = "10",
    stringsAsFactors = FALSE
  )
  expect_true(update_athletes(df))

  expect_equal(sent$method, "PUT")
  body <- parse_athletes(sent$body$data)[[1]]
  expect_equal(body$position, "Forward")
  expect_equal(body$height, 190.5)
  expect_null(body$lastTestedOn)
  expect_named(body$external, "jersey")
})

# --- teams, groups and external (CSE-152) ---

test_that("a list column of teams and groups is sent as a flat array", {
  df <- data.frame(id = c("id1", "id2"), name = c("One", "Two"))
  df$teams <- I(list(c("t2", "t3"), "t1"))
  df$groups <- I(list(c("g1", "g2"), "g3"))
  for (json in both_payloads(df)) {
    athletes <- parse_athletes(json)
    expect_identical(athletes[[1]]$teams, list("t2", "t3"))
    expect_identical(athletes[[1]]$groups, list("g1", "g2"))
    expect_identical(athletes[[2]]$teams, list("t1"))
    expect_identical(athletes[[2]]$groups, list("g3"))
    expect_false(grepl("[[", json, fixed = TRUE))
  }
})

test_that("comma-separated teams and groups are split and trimmed", {
  df <- data.frame(
    id = "id1",
    teams = " t2, t3 ,,",
    groups = "g1,g2",
    stringsAsFactors = FALSE
  )
  for (json in both_payloads(df)) {
    athlete <- parse_athletes(json)[[1]]
    expect_identical(athlete$teams, list("t2", "t3"))
    expect_identical(athlete$groups, list("g1", "g2"))
  }
})

test_that("a single team or group is still sent as a JSON array", {
  df <- data.frame(id = "id1", teams = "t1", groups = "g1")
  for (json in both_payloads(df)) {
    compact <- jsonlite::minify(json)
    expect_match(compact, '"teams":["t1"]', fixed = TRUE)
    expect_match(compact, '"groups":["g1"]', fixed = TRUE)
  }
})

test_that("NA, NULL and blank teams or groups are omitted", {
  df <- data.frame(id = c("id1", "id2", "id3"))
  df$teams <- I(list(NA, NULL, character(0)))
  df$groups <- c(NA, "", " , ")
  for (json in both_payloads(df)) {
    for (a in parse_athletes(json)) {
      expect_false(any(c("teams", "groups") %in% names(a)))
    }
  }
})

test_that("external is not sent when there are no custom columns", {
  df <- data.frame(id = "id1", name = "One", teams = "t1")
  for (json in both_payloads(df)) {
    expect_false("external" %in% names(parse_athletes(json)[[1]]))
    expect_false(grepl("external", json, fixed = TRUE))
  }
})

test_that("external is sent as an object when a custom column has a value", {
  df <- data.frame(id = "id1", jersey = "10", stringsAsFactors = FALSE)
  for (json in both_payloads(df)) {
    expect_match(jsonlite::minify(json), '"external":{"jersey":"10"}', fixed = TRUE)
  }
})

test_that("external is not sent for a row whose custom cells are all NA or blank", {
  df <- data.frame(
    id = c("id1", "id2"),
    jersey = c("10", NA),
    nickname = c(" ", ""),
    stringsAsFactors = FALSE
  )
  for (json in both_payloads(df)) {
    athletes <- parse_athletes(json)
    expect_identical(athletes[[1]]$external, list(jersey = "10"))
    expect_false("external" %in% names(athletes[[2]]))
  }
})

test_that("update_athletes sends flat teams and no external without custom columns", {
  skip_on_cran()

  auth <- HawkinAuth(config = HawkinConfig())
  auth@access_token <- "test-token"
  auth@expires_at <- as.POSIXct(Sys.time() + 3600)

  .hawkin_env <- hawkinR:::.hawkin_env
  old_conn <- .hawkin_env$active_conn
  .hawkin_env$active_conn <- auth
  on.exit(.hawkin_env$active_conn <- old_conn)

  sent <- NULL
  mockery::stub(update_athletes, "httr2::req_perform", function(req, ...) {
    sent <<- req
    structure(list(), class = "httr2_response")
  })
  mockery::stub(update_athletes, "httr2::resp_status", 200L)
  mockery::stub(update_athletes, "httr2::resp_body_json", list(
    data = data.frame(name = "Jane"),
    hasFailures = FALSE
  ))

  df <- data.frame(id = "a1", name = "Jane")
  df$teams <- I(list(c("t2", "t3")))
  expect_true(update_athletes(df))

  expect_equal(sent$method, "PUT")
  body <- parse_athletes(sent$body$data)[[1]]
  expect_identical(body$teams, list("t2", "t3"))
  expect_false("external" %in% names(body))
})

test_that("a list-valued custom column sends strings and skips empty elements", {
  df <- data.frame(id = c("a1", "a2", "a3", "a4"), name = c("A", "B", "C", "D"))
  df <- rbind(df, data.frame(id = c("a5", "a6"), name = c("E", "F")))
  df$tags <- I(list(NULL, character(0), "x", c("y", "z"), c("", "w"), c("", " ")))
  json <- hawkinR:::UpdateAthleteJSON(df)
  athletes <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_false("external" %in% names(athletes[[1]]))
  expect_false("external" %in% names(athletes[[2]]))
  expect_identical(athletes[[3]]$external$tags, "x")
  expect_identical(athletes[[4]]$external$tags, "y, z")
  expect_identical(athletes[[5]]$external$tags, "w")
  expect_false("external" %in% names(athletes[[6]]))
})

test_that("custom text values are trimmed and numbers stay numbers", {
  df <- data.frame(id = c("a1", "a2"), name = c("A", "B"),
                   jersey = c(" 10 ", NA), score = c(7, 8))
  athletes <- jsonlite::fromJSON(hawkinR:::UpdateAthleteJSON(df), simplifyVector = FALSE)
  expect_identical(athletes[[1]]$external$jersey, "10")
  expect_identical(athletes[[1]]$external$score, 7L)
  expect_false("jersey" %in% names(athletes[[2]]$external))
  expect_identical(athletes[[2]]$external$score, 8L)
})
