## R Session Information

We will try our best to keep code up to date and use modern versions of R, R-Studio and all packages.  That said, if you run into issues knowing our R Session info may help you troubleshoot.  Here's the specifics of our code runs:

```{r}
#| echo: false
#| message: false
#| warning: false


library(renv)
library(sessioninfo)
library(knitr)

# --- Platform Info ---
si <- suppressWarnings(session_info())
platform_info <- si$platform
platform_info["Library path"] <- "[hidden]"

platform_df <- data.frame(
  Component = names(platform_info),
  Value = unlist(platform_info),
  row.names = NULL
)

cat("### Platform Information\n")
kable(platform_df)

# --- Packages Used Across All .qmd Files ---
deps <- tryCatch(
  suppressWarnings(renv::dependencies()),
  error = function(e) {
    message("Warning: Failed to scan dependencies. Skipping.")
    return(data.frame())
  }
)

pkgs <- unique(deps$Package)

pkg_df <- data.frame(
  Package = pkgs,
  Version = sapply(pkgs, function(p) {
    tryCatch(as.character(packageVersion(p)), error = function(e) NA)
  }),
  stringsAsFactors = FALSE
)

cat("\n### Packages Used Across the Website\n")
kable(pkg_df)

```

