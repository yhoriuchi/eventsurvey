# eventsurvey: check local links and images in the generated pkgdown site.
# Run from the package root after pkgdown::build_site().

library(xml2)

site <- normalizePath("docs")
pages <- list.files(site, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
failures <- character()
checked <- 0L
for (page in pages) {
  html <- read_html(page)
  nodes <- xml_find_all(html, "//*[@href or @src]")
  links <- unique(c(xml_attr(nodes, "href"), xml_attr(nodes, "src")))
  links <- links[!is.na(links) & nzchar(links)]
  links <- sub("^https://yhoriuchi.github.io/eventsurvey/", paste0(site, "/"), links)
  links <- links[!grepl("^([a-zA-Z]+:|//|#)", links)]
  for (link in links) {
    path <- URLdecode(sub("[?#].*$", "", link))
    if (!nzchar(path)) next
    target <- if (startsWith(path, "/")) path else file.path(dirname(page), path)
    checked <- checked + 1L
    if (!file.exists(target)) failures <- c(failures, paste(page, "->", link))
  }
}
cat(length(pages), "HTML pages;", checked, "local references;", length(failures), "broken.\n")
if (length(failures)) stop(paste(failures, collapse = "\n"))
