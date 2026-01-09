
# hawkinR v1.2.0.1001 Dev Test -----


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
profile1 <- "ghouse_dev"
profile2 <- "hawkinR_tester"
org <- "v1"

## | B. Store API Secret -----
### 1. Store the token for different profiles
<<<<<<< HEAD
hd_auth_store(profile = profile1, token = Sys.getenv("gspToken"))
hd_auth_store(profile = profile2, token = Sys.getenv("hawkinR_tester"))

### 2. Test Initial Connection -----
hd_connect(profile = profile2)

=======
hd_auth_store(profile = "ghouse_dev", token = Sys.getenv("gspToken"))

### 2. Check Connection
hd_connect(profile = "ghouse_dev")
>>>>>>> d6a868dc825eac8606233c4d833b1991f695033d
### 3. Manually fetch the connection to ensure it exists
my_conn <- get_active_conn()

### 4. Access the profile from the connection
testProfile <- my_conn@config@profile

#-------------------------------#

### 5. Change Connection -----
hd_connect(profile = profile1)

### 6. Manually fetch the connection to ensure it exists
my_conn <- get_active_conn()

### 7. Access the profile from the connection
ghouseProfile <- my_conn@config@profile


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

if (rosterCheck) {
  log_info("hawkinR Dev Test: Inactive athletes successfully included in rosterAll")
} else {
  log_info("hawkinR Dev Test: Inactive athletes NOT included in rosterAll")
}

## | B. Get Team Data -----
teams <- get_teams()

## | C. Get Groups -----
groups <- get_groups()

## | D. Get Tags with alternate profile -----
tags <- get_tags(profile = profile2)


## | E. Get Tests by athlete | filter inactive-----
lg_tests_filtered <- get_tests(
  profile = profile1,
  athleteId = rosterActive$id[rosterActive$name %in% "Lauren Green"],
  from = "2024-01-01",
  to = "2024-12-31"
)

## | E. Get Tests by athlete | All-----
lg_tests_filtered <- get_tests(
  profile = profile1,
  athleteId = rosterActive$id[rosterActive$name %in% "Lauren Green"],
  from = "2024-01-01",
  to = "2024-12-31",
  includeInactive = TRUE
)

## | F. Get Tests by team -----
team_tests <- get_tests(
  teamId = teams$id[teams$name %in% "Colorado Prep Basketball"],
  from = "2025-01-01",
  to = "2025-12-31",
  sync = TRUE,
  includeInactive = TRUE
)

## | G. Get Tests by group -----
group_tests <- get_tests(
  groupId = groups$id[groups$name %in% "Testing"],
  from = "2025-01-01",
  to = "2025-12-31",
  includeInactive = TRUE
)

## | H. Get Tests by Type -----

### 1. CMJ Tests
cmj_tests <- get_tests(
  typeId = "CMJ",
  from = 1735449605
)

### 2. CMJ Tests
iso_tests <- get_tests(typeId = "Isometric Test")

### 3. Rebound Tests
types <- get_testTypes()
djType <- types$canonicalId[types$name %in% "Drop Jump"]
dj_tests <- get_tests(typeId = djType)

### 4. Open Ended Time Frame
allTests <- get_tests(
  includeInactive = TRUE
)

#-------------------------------#
## | G. Get Force time Data -----
ft_test <- get_forcetime(testId = lg_tests_filtered$id[2])

## | I. Get Mass Force time Data -----

# store raw data locally
all_my_data <- get_forcetime_bulk(test_ids = lg_tests_filtered$id)

# save raw data as rds file -----
get_forcetime_bulk(
  test_ids = lg_tests_filtered$id[1:10],
  export = TRUE,
  export_dir = paste0(getwd(),"/dev/sampleDataRDS/"),
  format = "rds",
  file_naming = c("athlete_name","test_id")
)

# save raw data as csv file -----
get_forcetime_bulk(
  test_ids = NULL,
  export = TRUE,
  export_dir = paste0(getwd(),"/dev/sampleDataCSV/"),
  format = "csv",
  file_naming = c("athlete_name","test_date","test_id"),
  athleteId = rosterActive$id[rosterActive$name %in% "Lauren Green"],
  from = "2024-06-01",
  to = "2024-08-31"
)

# save raw data as tsv file -----
get_forcetime_bulk(
  export = TRUE,
  export_dir = paste0(getwd(),"/dev/sampleDataTSV/"),
  format = "tsv",
  file_naming = c("testType_name","date","test_id"),
  typeId = rebType,
  from = "2024-01-01",
  to = "2024-12-31"
)
