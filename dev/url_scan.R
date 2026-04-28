files <- c(
  "DESCRIPTION",
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  "README.md",
  "NEWS.md"
)
all_text <- unlist(lapply(files, readLines, warn = FALSE))
# Match URL chars but stop at common trailing punctuation/delimiters
m <- regmatches(all_text, gregexpr("https?://[^\\s,;)\"'>\\]}`]+", all_text, perl = TRUE))
urls <- unique(unlist(m))
# Strip any stray trailing punctuation that slipped through
urls <- gsub("[.,;)\"\\]>]+$", "", urls)
urls <- unique(urls[nchar(urls) > 0])
cat(sprintf("Found %d unique URLs\n\n", length(urls)))

for (u in urls) {
  r <- tryCatch({
    req <- httr2::request(u)
    req <- httr2::req_method(req, "HEAD")
    req <- httr2::req_timeout(req, 15)
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)
    httr2::resp_status(resp)
  }, error = function(e) paste0("ERR: ", conditionMessage(e)))
  cat(sprintf("  [%s] %s\n", r, u))
}
