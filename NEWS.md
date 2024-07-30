# Change Log

## hawkinR v1.1.1

* fixes (or removes) log suppression:  surface more useful logging to end-users by removing log suppression present in v1.1.0 and earlier

## hawkinR v1.1.0

* Update of `get_tests()` functionality.

* Depreciation of `get_tests_...` functions.

* 'teamId' and 'groupId' parameters now accept lists and atomic vectors to go along with single text strings

* Addition of logs functionality and customization

* Updated documentation

## hawkinR v1.0.5.02

* fix to `DateTimeParam` utility function

* updated test files

## hawkinR v1.0.5.01

* Bug fix to get_tests_teams / groups `to` parameter

## hawkinR v1.0.5.1

* Bug fixes to `get_tests...`. Corrected pad_and_condense function in utils.R

## hawkinR v1.0.5

* Bug fixes to `get_tests_type`. Now handles test trials with and without tags specified

* Changes to `get_tests_type`. 'typeId' parameter now accepts strings containing either canonical test Id, test type name, or test type abbreviation.  See docs for abbreviations.

* Addition of `get_tags` function. Create data frame of tags within your system, including: id, name, description.

* Addition of utils.R file with utility functions to minimize code and increase readability.

## hawkinR v1.0.4

* changes to API endpoint (inclusion of test tag details). All `get_tests_...` function outputs now
include tag Ids, names, and description columns with the prefix 'test_type_tag_'. These can be
found after 'test_type_canonicalId' column.

## hawkinR v1.0.3.2

* bug fixes to all `get_tests_...` functions to handle NULL externalIds

## hawkinR v1.0.3.1

* bug fixes to get_atheletes function to handle NULL externalIds

## hawkinR v1.0.3

* Updates to all `get_tests_...` functions to include filter parameter for 'active' tests. Ability 
to include or exclude disabled trials.

* Updates to all `get_tests_...` functions to include 'sync' parameter. Ability 
to utilize 'syncFrom' and 'syncTo' in all queries. This allows for more efficient queries and 
faster download times when used frequently or to maintain an external database.

* Bug fixes to externalId field

## hawkinR v1.0.2

* updated `get_athlete output` to include externalId

## hawkinR v1.0.1

* bug fixes to `get_tests_team` and `get_tests_group`

* Changes to column name outputs for `get_tests...` calls.

* bug fixes to `get_forcetime`

## hawkinR 1.0.0

* Initial CRAN submission.
