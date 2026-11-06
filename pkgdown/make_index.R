# Creates pkgdown/index.md (home page of the website) from README.md,
# embedding videos and fixing image paths.
#
# Runs automatically in the pkgdown GitHub Actions workflow before building the
# site. To preview the site locally run it first (from the package root):
#   source("pkgdown/make_index.R"); pkgdown::build_site()

# Read the content of the README.md file
file_content <- readLines("README.md", warn = FALSE, encoding = "UTF-8")

# video player template
replacement_template <- '<video src="URL_PLACEHOLDER" controls width="100%"></video>'

# Replace lines with video links (<https://github.com/user-attachments/...>)
is_video <- grepl("^<?https://github\\.com/user-attachments/assets/[^ ]+>?\\s*$", file_content)
urls <- gsub("^<|>?\\s*$", "", file_content[is_video])
file_content[is_video] <- vapply(urls, function(u) sub("URL_PLACEHOLDER", u, replacement_template, fixed = TRUE), character(1))

# GitHub alert boxes ("> [!TIP]" blocks) as Bootstrap alert boxes (pkgdown shows them as plain quotes)
alert_start <- grep("^> \\[!(NOTE|TIP|IMPORTANT|WARNING|CAUTION)\\]\\s*$", file_content)
for (start in rev(alert_start)) {
  end <- start
  while (end < length(file_content) && grepl("^>", file_content[end + 1])) end <- end + 1
  body <- sub("^> ?", "", file_content[(start + 1):end])
  file_content <- c(file_content[seq_len(start - 1)], '<div class="alert alert-info">', "", body, "", "</div>",
                    file_content[seq_along(file_content) > end])
}

# fix image paths
file_content <- gsub("man/figures/", "reference/figures/", file_content)

# Write the modified content to index.md
writeLines(file_content, "pkgdown/index.md", useBytes = TRUE)

message("pkgdown/index.md created from README.md (", sum(is_video), " video(s) embedded)")
