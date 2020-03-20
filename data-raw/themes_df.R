## code to prepare `themes_df` dataset goes here
options(stringsAsFactors = FALSE)
themes_df <- data.frame(
  theme = c("generic", "water", "air", "soil", "vegetation", "species"),
  theme_number = c("0", "1", "2", "3", "4", "5"))

usethis::use_data(themes_df, internal = TRUE, overwrite = TRUE)
