packages <- c(
  "nhanesA",
  "dplyr",
  "haven",
  "here",
  "ggplot2",
  "tidyr",
  "corrplot",
  "dagitty",
  "ggdag",
  "caret",
  "mediation",
  "pROC"
)

missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
