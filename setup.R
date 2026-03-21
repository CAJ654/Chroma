pkgs <- c("shiny", "httr", "jsonlite", "shinycssloaders")
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org", quiet = TRUE)
  }
}
cat("All packages ready.\n")
