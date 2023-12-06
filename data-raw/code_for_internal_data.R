## code to prepare `themes_df` dataset goes here
options(stringsAsFactors = FALSE)
themes_df <- data.frame(
  theme = c("generic", "water", "soil", "air", "vegetation", "species"),
  theme_number = c("0", "1", "2", "3", "4", "5"))

# code to prepare reserved_codes internal dataset goes here
library(googlesheets4)
library(dplyr)
gs4_auth(
  email = "*@inbo.be",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

inventaris_labo <- "1ClXTqk8bDtWz1wAvWeb5HLCU2UHLx0H50ohi5f8zboM"

sip <- googlesheets4::read_sheet(
  ss = inventaris_labo,
  sheet = "SIP") %>%
  janitor::clean_names()
sap <- googlesheets4::read_sheet(
  ss = inventaris_labo,
  sheet = "SAP") %>%
  janitor::clean_names()
sop <- googlesheets4::read_sheet(
  ss = inventaris_labo,
  sheet = "SOP") %>%
  janitor::clean_names()

inventaris_svp <- "1MJbpX1gLxzcmUDbPSIRPS0uCIBaJsIycVNWL4XVw4uo"
sfp <- googlesheets4::read_sheet(
  ss = inventaris_svp,
  sheet = "SVP") %>%
  janitor::clean_names()

sip2 <- googlesheets4::read_sheet(
  ss = inventaris_svp,
  sheet = "SIP") %>%
  janitor::clean_names()

# genetisch labo
inventaris_gendiv <- "1ItMaxUZDgJJqf8kf1Y_SgNgxahbxxXqWcKLAKshoatw"
sap_gendiv <- googlesheets4::read_sheet(
  ss = inventaris_gendiv,
  sheet = "SAP") %>%
  janitor::clean_names()
sip_gendiv <- googlesheets4::read_sheet(
  ss = inventaris_gendiv,
  sheet = "SIP") %>%
  janitor::clean_names()
sop_gendiv <- googlesheets4::read_sheet(
  ss = inventaris_gendiv,
  sheet = "SOP") %>%
  janitor::clean_names()




#####################################################
# harmoniseren
sip_cleaned <- sip %>%
  filter(!is.na(sip)) %>%
  mutate(description = sprintf("Apparaat: %s; Type model: %s; Producent: %s",
                               apparaat, type_model, producent),
         inventory = "lab") %>%
  select(protocolcode = sip,
         description,
         inventory) %>%
  mutate(protocolcode = paste0(tolower(protocolcode), "-nl")) %>%
  distinct()

all(grepl("s[fioap]p-\\w{3,6}-nl",
          x = sip_cleaned$protocolcode))

sip2_cleaned <- sip2 %>%
  mutate(inventory = "field") %>%
  select(protocolcode = svp_code,
         description = titel,
         inventory) %>%
  mutate(protocolcode = paste0(tolower(protocolcode), "-nl")) %>%
  distinct()

all(grepl("s[fioap]p-\\w{3,6}-nl",
          x = sip2_cleaned$protocolcode))

sfp_cleaned <- sfp %>%
  filter(!is.na(svp_code)) %>%
  select(protocolcode = svp_code,
         description = titel) %>%
  mutate(protocolcode = paste0(tolower(protocolcode), "-nl"),
         protocolcode = gsub("s0p", "sop", protocolcode),
         protocolcode = gsub("svp", "sfp", protocolcode),
         inventory = "field") %>%
  distinct()

all(grepl("s[fioap]p-\\w{3,6}-nl",
          x = sfp_cleaned$protocolcode))

sop_cleaned <- sop %>%
  select(protocolcode = sop_code,
         description = procedure) %>%
  mutate(protocolcode = paste0(tolower(protocolcode), "-nl"),
         inventory = "lab") %>%
  distinct()

all(grepl("s[fioap]p-\\w{3,6}-nl",
          x = sop_cleaned$protocolcode))


sap_cleaned <- sap %>%
  mutate(description = sprintf("Methode: %s; Toestel: %s; Matrix: %s",
                               methode, toestel, matrix)) %>%
  select(protocolcode = sap_code,
         description) %>%
  mutate(protocolcode = paste0(tolower(protocolcode), "-nl"),
         protocolcode = gsub("\\/", "", protocolcode),
         inventory = "lab") %>%
  distinct()

all(grepl("s[fioap]p-\\w{3,6}-nl",
          x = sap_cleaned$protocolcode))

sap_gendiv_cleaned <- sap_gendiv %>%
  filter(!is.na(sap_code)) %>%
  mutate(description = sprintf("Methode: %s; Techniek: %s",
                               methode, techniek)) %>%
  select(protocolcode = sap_code,
         description) %>%
  mutate(protocolcode = paste0(tolower(protocolcode), "-nl"),
         protocolcode = gsub("\\/", "", protocolcode),
         inventory = "gendiv") %>%
  distinct()

all(grepl("s[fioap]p-\\w{3,6}-nl",
          x = sap_gendiv_cleaned$protocolcode))

sip_gendiv_cleaned <- sip_gendiv %>%
  filter(!is.na(sip_code)) %>%
  mutate(description = sprintf("Toestel: %s; Type model: %s; Producent: %s",
                               toestel, type_model, producent)) %>%
  select(protocolcode = sip_code,
         description) %>%
  mutate(protocolcode = paste0(tolower(protocolcode), "-nl"),
         protocolcode = gsub("\\/", "", protocolcode),
         inventory = "gendiv") %>%
  distinct()

all(grepl("s[fioap]p-\\w{3,6}-nl",
          x = sip_gendiv_cleaned$protocolcode))

sop_gendiv_cleaned <- sop_gendiv %>%
  filter(!is.na(sop_code)) %>%
  mutate(description = sprintf("Procedure: %s",
                               procedure)) %>%
  select(protocolcode = sop_code,
         description) %>%
  mutate(protocolcode = paste0(tolower(protocolcode), "-nl"),
         protocolcode = gsub("\\/", "", protocolcode),
         inventory = "gendiv") %>%
  distinct()

all(grepl("s[fioap]p-\\w{3,6}-nl",
          x = sop_gendiv_cleaned$protocolcode))


reserved_codes <- bind_rows(
  sap_cleaned,
  sap_gendiv_cleaned,
  sfp_cleaned,
  sip_cleaned,
  sip2_cleaned,
  sip_gendiv_cleaned,
  sop_cleaned,
  sop_gendiv_cleaned
)

# also add bare number versions in case of 123a type numbers
bare_codes <- reserved_codes %>%
  tidyr::separate(protocolcode,
                  into = c("protocoltype", "protocolnumber_full", "language"),
                  sep = "-",
                  remove = FALSE) %>%
  filter(!grepl("^\\d{3}$", protocolnumber_full)) %>%
  mutate(protocolnumber_bare = stringr::str_sub(protocolnumber_full, 1, 3),
         protocolcode_orig = protocolcode,
         protocolcode = paste(protocoltype, protocolnumber_bare, language,
                              sep = "-")) %>%
  distinct(protocolcode, inventory)

reserved_codes <- bind_rows(reserved_codes, bare_codes) %>%
  distinct() %>%
  arrange(protocolcode) %>%
  tidyr::separate(protocolcode,
                  into = c("protocoltype", "protocolnumber", "language"),
                  sep = "-",
                  remove = FALSE) %>%
  filter(grepl(pattern = "\\d{3}.{0,3}", x = .$protocolnumber)) %>%
  mutate(protocolnumber_bare = stringr::str_sub(protocolnumber, 1, 3))

# some protocolcodes are listed more than once
reserved_codes <- reserved_codes %>%
  group_by(protocolcode, protocoltype, protocolnumber, language,
           protocolnumber_bare) %>%
  summarise(description = paste(description, collapse = " | "),
            inventory = paste(inventory, collapse = " | "),
            .groups = "drop")

inbo_affiliation <- c(
  en = "Research Institute for Nature and Forest (INBO)",
  nl = "Instituut voor Natuur- en Bosonderzoek (INBO)",
  fr = "Institut de Recherche sur la Nature et les For\u00eats (INBO)",
  de = "Institut f\u00fcr Natur- und Waldforschung (INBO)"
)

usethis::use_data(
  themes_df, reserved_codes, inbo_affiliation,
  internal = TRUE, overwrite = TRUE)
