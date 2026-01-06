################################
# hawkinR v1.2.0.1000 Dev Test #
################################

#---------------------------------#
# 1. Set Project Environment -----
#---------------------------------#

devtools::load_all()

initialize_logger(log_output = "both", log_threshold_file = "TRACE")

#-----------------------#
# 2. Authorization -----
#-----------------------#

## | A. Set Auth Parameters -----
region <- "Americas"
profile <- "Greenhouse"
org <- "v1"

## | B. Store API Secret -----
### 1. Store the token for different profiles
hd_auth_store(profile = "ghouse_dev", token = Sys.getenv("gspToken"))
#hd_auth_store(profile = "hawkinR_tester", token = Sys.getenv("hawkin_tester"))
hd_auth_store(profile = "Greenhouse")

### 2. Check Connection
hd_connect(profile = "hawkinR_tester")
### 3. Manually fetch the connection to ensure it exists
my_conn <- get_active_conn()
### 4. Access the profile from the connection
testProfile <- my_conn@config@profile

#-------------------------------#

### 5. Change Connection
hd_connect(profile = "Greenhouse")
### 6. Manually fetch the connection to ensure it exists
my_conn <- get_active_conn()
### 7. Access the profile from the connection
greenhouseProfile <- my_conn@config@profile

#-------------------------------#
### 8. Change Connection
hd_connect(profile = "ghouse_dev")
### 9. Manually fetch the connection to ensure it exists
my_conn <- get_active_conn()
### 10. Access the profile from the connection
activeProfile <- my_conn@config@profile


## | C. Set Configuration -----
hd_connect(profile = "ghouse_dev")

## | D. Test Authorization -----


#-----------------------#
# 4. Test Functions ----
#-----------------------#

## | A. Get Athletes -----

### 1. Get active athletes only
rosterActive <- get_athletes(includeInactive = FALSE)

### 2. Get all athletes, including inactive
rosterAll <- get_athletes(includeInactive = TRUE)

### 3. Check that inactive athletes are included
rosterCheck <- length(rosterActive) < length(rosterAll)

## | B. Get Team Data -----
teams <- get_teams()

## | C. Get Groups -----
groups <- get_groups()

## | D. Get Tags -----
tags <- get_tags()



## | D. Get Tests by athlete -----
lg_tests <- get_tests(
  athleteId = rosterActive$id[rosterActive$name %in% "Lauren Green"],
  from = "2024-01-01",
  to = "2024-12-31")

## | E. Get Tests by team -----
team_tests <- get_tests(
  teamId = teams$id[teams$name %in% "Colorado Prep Basketball"],
  from = "2025-01-01",
  to = "2025-12-31",
  sync = TRUE)

## | F. Get Tests by group -----
group_tests <- get_tests(
  groupId = groups$id[groups$name %in% "Testing"],
  from = "2025-01-01",
  to = "2025-12-31",
  includeInactive = TRUE)

## | G. Get Tests by Type -----

### 1. CMJ Tests
cmj_tests <- get_tests(
  typeId = "CMJ",
  from = 1735449605
  )

### 2. CMJ Tests
sj_tests <- get_tests(typeId = "Isometric Test")

### 3. Rebound Tests
types <- get_testTypes()
rebType <- types$canonicalId[types$name %in% "Drop Jump"]
reb_tests <- get_tests(typeId = "CMJ")

## | G. Get Force time Data -----
ft_test <- get_forcetime(testId = lg_tests$id[1])

## | H. Get Mass Force time Data -----
all_my_ft <- function(testIdList) {
  ft_list <- list()
  for (i in 1:nrow(testIdList)) {
    message("Fetching Force-Time data for test ", i, " of ", nrow(testIdList), ": ", testIdList$id[i])

    tryCatch({
      ft_data <- get_forcetime(testId = testIdList$id[i])
      ft_list[[testIdList$id[i]]] <- ft_data
      message("Completed fetching Force-Time data for test ", i, " of ", nrow(testIdList), ": ", testIdList$id[i])
    }, error = function(e) {
      logger::log_error(paste0("hawkinR/all_my_ft ->",
                              "Error fetching data for test ID ",
                              testIdList$id[i], ": ", e$message))
      stop("Error fetching data for test ID ", call. = FALSE)
    })
  }
  return(ft_list)
}

all_ft_data <- all_my_ft(lg_tests)
