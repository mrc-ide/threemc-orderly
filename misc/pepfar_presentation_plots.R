#### Plots for PEPFAR Presentation ####

#### libs ####

library(dplyr, warn.conflicts = FALSE)
library(data.table)
library(forcats)
library(ggplot2)
library(ggridges)
library(reactable)
# library(reactablefmtr)
library(sf)
# library(threemc)
devtools::load_all("../threemc")
library(orderly)
library(ggsci)
library(geofacet)
library(glue)
library(ggtext)
source("Shiny/src/functions.R")

#### Metadata ####

orderly_root <- here::here()

spec_age_group <- "15-29"
spec_years <- c(2010, 2021)

## for tabulating surveys
# remove circumcisions with missing type?
rm_missing_type <- FALSE

start_year <- 2002
cens_age <- 59

# Revert to using planar rather than spherical geometry in `sf`
sf::sf_use_s2(FALSE)

# location of data to include all of Africa in map plots
dat_loc <- "poster/data/"

# countries we attempted to model
ssa_iso3 <- sort(c(
  "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD",
  "COG", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN",
  "LBR", "LSO", "MLI", "MOZ", "MWI", "NAM", "NER", "NGA", "RWA",
  "SEN", "SLE", "SWZ", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"
))

ssa_countries <- countrycode::countrycode(ssa_iso3, "iso3c", "country.name")
ssa_countries <- case_when(
  ssa_countries == "Congo - Kinshasa"    ~ "DR Congo",
  ssa_countries == "Congo - Brazzaville" ~ "Congo",
  grepl("Ivoire", ssa_countries)         ~ "Cote d'Ivoire",
  ssa_countries == "Gambia"              ~ "The Gambia",
  TRUE                                   ~ ssa_countries
)

# countries/regions which have been targeted for 90% circumcision by 2021
target_iso3 <- c(
  # In Ethiopia Gambella is the only target province
  "BWA", "ETH_1_15", "KEN", "LSO", "MWI", "MOZ", "NAM", 
  "RWA", "ZAF", "SWZ", "UGA", "TZA", "ZMB", "ZWE"
)

colourPalette <- rev(colorRampPalette(
  c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf",
    "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")
  )(100))

#### Load Data ####

# pull most recent results for age groups
archives <- orderly::orderly_list_archive()
# dir_name <- orderly::orderly_list_archive() %>% 
#   filter(name == "03_shiny_consolidation") %>%
#   slice(n()) %>% 
#   pull(id)

dir_name <- orderly::orderly_search(
  query = "latest(parameter:is_paper == is_paper)", 
  name  = "03_shiny_consolidation",
  parameters = list(is_paper = TRUE)
)
results_agegroup <- readr::read_csv(file.path(
  orderly_root, 
  "archive/03_shiny_consolidation",
  dir_name,
  "/artefacts/results_agegroup.csv.gz"
))

# also pull for single ages (large!)
results_age <- readr::read_csv(file.path(
  orderly_root,
  "archive/03_shiny_consolidation",
  dir_name,
  "/artefacts/results_age.csv.gz"
))

results_agegroup_n_circ <- readr::read_csv(file.path(
  orderly_root, 
  "archive/03_shiny_consolidation",
  dir_name,
  "/artefacts/results_agegroup_n_circ.csv.gz"
))

missing_iso3 <- target_iso3[!target_iso3 %in% results_agegroup$area_id]

# pull shapefiles
areas <- sf::read_sf(file.path(
  orderly_root, 
  "archive/03_shiny_consolidation",
  dir_name,
  "depends/areas.geojson"
)) %>% 
  dplyr::mutate(space = seq_len(dplyr::n()))

# pull surveys
survey_dir_name <- orderly::orderly_list_archive() %>% 
  filter(name == "00b3_survey_join") %>%
  slice(n()) %>% 
  pull(id)

survey_circumcision <- survey_circumcision_orig <- read_circ_data(file.path(
    orderly_root, 
    "archive/00b3_survey_join",
    survey_dir_name,
    "artefacts/survey_circumcision.csv.gz"
  ),
  filters = c("sex" = "male")
) %>% 
  # only keep surveys for countries we have results for
  filter(iso3 %in% results_agegroup$iso3)

# pull agegroup populations
pops_dir_name <- orderly::orderly_list_archive() %>% 
  filter(name == "00c4_pops_aggregate") %>%
  slice(n()) %>% 
  pull(id)
populations <- read_circ_data(file.path(
    orderly_root, 
    "archive/00c4_pops_aggregate",
    pops_dir_name,
    "artefacts/population_agegroup_aggr.csv.gz"
  ),
  filters = c("sex" = "male")
) %>% 
  # only keep surveys for countries we have results for
  filter(iso3 %in% results_agegroup$iso3)

# last surveys 
last_surveys <- readr::read_csv("global/most_recent_surveys.csv")

# Additional areas from Oli, so map plot can be for all of SSA
non_ssa_afr_areas <- sf::read_sf(
  file.path(
    dat_loc, 
    "Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp"
  )
 ) %>%
 filter(
   CNTRY_NAME %in% c(
     "Western Sahara", "Mauritania", "Morocco", "Algeria", 
     "Libya", "Tunisia", "Egypt", "Equatorial Guinea", 
     "Somalia", "Djibouti", "Eritrea", "Sudan"
   )
 )
  
# areas for SDN and SSD
sudanese_areas <-   
  bind_rows(
    sf::read_sf(file.path(dat_loc, "sdn_adm_cbs_nic_ssa_20200831_shp/sdn_admbnda_adm1_cbs_nic_ssa_20200831.shp")),
    sf::read_sf(file.path(dat_loc, "ssd_admbnda_imwg_nbs_shp/ssd_admbnda_adm0_imwg_nbs_20180817.shp"))
  ) %>% 
  select(CNTRY_NAME = ADM0_EN) %>% 
  group_split(CNTRY_NAME) %>% 
  purrr::map(function(x) {
    cntry <- unique(x$CNTRY_NAME)
    x <- sf::st_union(x) # may need sf::st_combine?
    df <- data.frame("CNTRY_NAME" = cntry)
    sf::st_geometry(df) <- x
    return(df)
  }) %>%  
  bind_rows()
  
non_ssa_afr_areas <- non_ssa_afr_areas %>% 
  bind_rows(sudanese_areas) %>% 
  mutate(
    iso3 = countrycode::countrycode(CNTRY_NAME, "country.name", "iso3c"),
    area_level = 0
  ) %>% 
  filter(!is.na(iso3)) %>% 
  select(iso3, area_level)

# pull surveys and dmppt2 data 
dmppt2_data <- readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/dmppt2-2021_circumcision_coverage_shiny.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3)

survey_data <- readr::read_csv(paste0(
  orderly_root, 
  "/archive/03_shiny_consolidation/",
  dir_name,
  "/artefacts/survey-circumcision-coverage_shiny.csv.gz"
)) %>% 
  filter(iso3 %in% ssa_iso3)

# Pull single age results for just ZMB
zmb_agg_loc <- orderly::orderly_search(
  query      = "latest(parameter:cntry == cntry)", 
  name       = "02_aggregations", 
  parameters = list(cntry = "ZMB")
)

# zmb_results_age <- results_reader(
#   type = "age", 
#   dir_path = file.path(
#     orderly_root,
#     "archive/02_aggregations",
#     zmb_agg_loc,
#     "artefacts/"
#   )
# )

zmb_results_age <- filter(results_age, iso3 == "ZMB")

#### ZMB Figures ####

# Why choose ZMB? 
# Large variation in circumcision and circumcision type 
# Quite a few surveys available (2007 2013 2016 2018)
# Shows how MMC is "competing" with TMC
# One of 14 VMMC priority countries

# agegroup results for just ZMB
zmb_results_agegroup <- filter(results_agegroup, iso3 == "ZMB")

# number of plots to show (show all of ZMBs provinces)
n_plots <- areas %>% sf::st_drop_geometry() %>% 
  filter(iso3 == "ZMB", area_level == 1) %>% 
  group_by(area_level) %>% 
  count() %>% 
  pull()

#### ZMB Map Plot ####

zmb_plt1 <- plt_coverage_map(
  results_agegroup, 
  areas, 
  colourPalette = colourPalette,
  spec_age_group = spec_age_group, 
  spec_years = spec_years,
  spec_model = "No program data", 
  plot_type = "single",
  country_area_level = 1, 
  spec_countries = "ZMB", 
  inc_difference = TRUE
)

# remove colour bar for Medical and Traditional circumcision
zmb_plt1$Medical <- zmb_plt1$Medical + 
  theme(legend.position = "bottom")
zmb_plt1$Traditional <- zmb_plt1$Traditional # + 
  # theme(legend.position = "none")

# save as:
# Total: 1200 x 800
# Medical and Traditional: 800 x 632 (?)

#### Coverage & N_Performed ####

# Problem: N performed x-axis is weird! Use old function here

zmb_plt2 <- plt_mc_coverage_prevalence(
  zmb_results_agegroup,
  areas,
  spec_age_group = spec_age_group,
  spec_years = spec_years,
  area_levels = 0, # Look at national level
  spec_model = "No program data",
  main = "MC Coverage vs Year, ",
  n_plots = 1
)[[1]] + 
  scale_x_continuous(
    breaks = seq(first(spec_years) + 1, last(spec_years), by = 2)
  ) +
  ggtitle("ZMB, Zambia, 15-29")

zmb_plt2b <- plt_mc_coverage_prevalence(
  zmb_results_agegroup,
  areas,
  spec_age_group = spec_age_group,
  spec_years = spec_years,
  area_levels = 1, # Look at provincial level
  spec_model = "No program data",
  main = "MC Coverage vs Year, ",
  n_plots = 1
)[[10]] + # North-Western Province
  scale_x_continuous(
    breaks = seq(first(spec_years) + 1, last(spec_years), by = 2)
  ) +
  ggtitle("ZMB, North-Western Province, 15-29")

# Interesting that North-Western has seen high MMC growth even though 
# it had the highest TMC, when generally VMMC has tried to target low MC areas

# Save with size 1000 x 800 (?)
# Save with size 800 x 600 (copy to clipboard)

#### Ridge Plot for age at circumcision ####

zmb_plt3 <- plt_circ_age_ridge(
  zmb_results_age,
  areas,
  spec_ages = 0:30, 
  area_levels = 1, # provincial data
  n_plots = n_plots
)

# traditional circumcision focused on adolescents, as TMIC
# Medical circumcision focused on older ages, and in some provinces babies

# Maybe other countries have more interesting results here???

#### ZMB: Comparisons with Surveys ####

survey_data_zmb <- survey_data %>% 
  filter(iso3 == "ZMB") %>% 
  rename(type = indicator) %>% 
  mutate(
    type = case_when(
      type == "circumcised"  ~ "Total",
      type == "circ_medical" ~ "Medical",
      TRUE                   ~ "Traditional"
    )
  )

# coverage vs year 
zmb_plt4 <- plt_MC_modelfit_spec_age(
  df_results = left_join(
    zmb_results_agegroup,
    select(sf::st_drop_geometry(areas), area_id, area_level_label) 
  ),
  df_results_survey = survey_data_zmb, 
  mc_type_model = "MC coverage",
  mc_type_survey = "Total",
  # age_per = c("15-29", "15-49"),
  age_per = spec_age_group,
  # years = unique(survey_data_zmb$year),
  years = first(unique(survey_data_zmb$year)):2021,
  area_level_select = 1,
  model_type = "No program data",
  xlab = "Year",
  ylab = "Total Circumcision Coverage",
  title = "Circumcision Coverage vs Year, Ages 10-29",
  n_plots = n_plots
)[[1]] + 
  theme(legend.position = "none")

zmb_plt4a <- plt_MC_modelfit_spec_age(
  df_results = left_join(
    zmb_results_agegroup,
    select(sf::st_drop_geometry(areas), area_id, area_level_label) 
  ),
  df_results_survey = survey_data_zmb, 
  mc_type_model = "MC coverage",
  mc_type_survey = "Total",
  # age_per = c("15-29", "15-49"),
  age_per = spec_age_group,
  years = first(unique(survey_data_zmb$year)):2021,
  area_level_select = 0,
  model_type = "No program data",
  xlab = "Year",
  ylab = "Total Circumcision Coverage",
  title = "Circumcision Coverage vs Year, Ages 10-29",
  n_plots = n_plots
)[[1]] + 
  theme(legend.position = "none")

# coverage vs age group 
# (should maybe do for another country as well to show poor fit for earler years?)
zmb_plt5 <- plt_MC_modelfit(
  df_results = left_join(
    zmb_results_agegroup,
    select(sf::st_drop_geometry(areas), area_id, area_level_label)
  ),
  df_results_survey = survey_data_zmb, 
  mc_type_model = "MC coverage",
  mc_type_survey = "Total", 
  survey_years = unique(survey_data_zmb$year),
  area_level_select = 0, # just look at national level
  model_type = "No program data",
  xlab = "Age Group",
  ylab = "Circumcision Coverage",
  title = "Circumcision Coverage vs Age Group, ",
  n_plots = n_plots
)[[1]]

# Empirical vs Modelled Rates (include? See plots first!)
# zmb_plt6 <-  plt_empirical_model_rates(
#   results           = zmb_results_age, 
#   empirical_rates   = ,
#   areas             = areas,
#   # spec_type       = "MMC probability",
#   # spec_age_groups = spec_age_group, 
#   spec_years        = spec_years,
#   spec_area_levels  = 1,
#   main              = NULL, 
#   n_plots           = n_plots
# )


#### SSA Figures ####

#### Figure 1: Tabulate Surveys ####

# remove surveys which are not to be included in paper
# survey_circumcision <- survey_circumcision %>% 
#   filter(
#     !survey_id %in% c(
#       "MWI2020PHIA"
#     )
#   )

# Prepare circ data, normalise survey weights and apply Kish coefficients.
survey_circumcision <- prepare_survey_data(
  areas               = areas,
  survey_circumcision = select(survey_circumcision, -matches("area_level")),
  area_lev            = threemc::datapack_psnu_area_level,
  start_year          = start_year,
  cens_year           = TRUE,
  cens_age            = cens_age,
  rm_missing_type     = rm_missing_type,
  norm_kisk_weights   = TRUE
)

survey_circumcision <- data.table::rbindlist(survey_circumcision)

# what information do I want for each survey?
# - country, provider, do they have circ info, do they distinguish between types,
# number of observations, Neff
# Region in Africa (mostly for surveys plot)
# NID and citation (may have to do by hand?)
# Also add missing surveys as well!

# create initial table and find country, provider, year from survey_id
provider_key <- tibble::tribble(
  ~provider, ~provider_name,
  "AIS",     "AIDS Indicator Survey",
  "DHS",     "Demographic and Health Survey",
  "PHIA",    "Population-Based HIV Impact Assessment",
  "HSRC",    "Human Sciences Research Council",
  "BAIS",    "Botswana Aids Impact Survey",
  "MICS",    "Multiple Indicator Cluster Survey",
  "SBS",     "Sexual Behavior Survey"
)
survey_tbl <- survey_circumcision_orig %>%
  distinct(iso3, survey_id) %>%
  # split survey_id between year and provider after the 4th numchar
  mutate(survey_sep = stringr::str_remove(survey_id, iso3)) %>%
  tidyr::separate(
    survey_sep, c("year", "provider"), sep = "(?<=[0-9]{4})"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  # join in provider name
  left_join(provider_key, by = "provider")

# N and Neff for known surveys
n_survey <- survey_circumcision %>%
  distinct(survey_id, N, Neff)
survey_tbl <- survey_tbl %>%
  left_join(n_survey, by = "survey_id") %>%
  mutate(
    across(N:Neff, ~ ifelse(is.na(.), 0, .)),
    status = "Present"
  )

# Find what types of circumcision are in each survey
types_tbl <- survey_circumcision %>%
  distinct(survey_id, type) %>%
  mutate(is_present = 1) %>%  # initialise
  tidyr::pivot_wider(
    names_from = type, values_from = is_present, values_fill = 0
  ) %>%
  mutate(type = case_when(
    MMC == 1 & TMC == 1 & Missing == 1 ~ "MMC, TMC, MC",
    MMC == 1 & Missing == 1             ~ "MMC, MC",
    TMC == 1 & Missing == 1             ~ "TMC, MC",
    Missing == 1                         ~ "MC",
    TRUE                                 ~ NA_character_
  )) %>%
  select(survey_id, type)


# add missing surveys that are present in Cork paper
# NOTE: MICS Surveys may be obtainable through orderly + Oli's script
# could make this a lot simpler by just manually doing survey id!
missing_tbl <- tribble(
  ~iso3, ~survey_id,    ~year,  ~provider,
  # "BEN", "BEN2014MICS", 2014,   "MICS",
  "BWA", "BWA2001AIS",  2001,   "AIS",
  "BWA", "BWA2004AIS",  2004,   "AIS",
  "BWA", "BWA2008AIS",  2008,   "AIS",
  "BWA", "BWA2013AIS",  2013,   "AIS",
  # "CAF", "CAF2006MICS", 2006,   "MICS",
  "CIV", "CIV2005AIS",  2005,   "AIS",
  # "COG", "COG2014MICS", 2014,   "MICS",
  "ERI", "ERI2010",     2010,   "Population and Health Survey",
  "KEN", "KEN2007AIS",  2007,   "AIS",
  "KEN", "KEN2013AIS",  2013,   "AIS",
  "LSO", "LSO2014DHS",  2014,   "DHS",
  "NGA", "NGA2006",     2006,   "Core Welfare Indicators Questionnaire Survey",
  # "NGA", "NGA2017MICS", 2017,   "MICS",
  # "SWZ", "SWZ2010MICS", 2010,   "MICS",
  # "SWZ", "SWZ2014MICS", 2014,   "MICS",
  "TZA", "TZA2017AIS",  2017,   "AIS",
  "UGA", "UGA2005AIS",  2005,   "AIS",
  "ZMB", "ZMB2000SBS",  2000,   "SBS",
  "ZMB", "ZMB2003SBS",  2003,   "SBS",
  "ZMB", "ZMB2005SBS",  2005,   "SBS",
  "ZMB", "ZMB2009SBS",  2009,   "SBS",
  "ZWE", "ZWE2005",     2005,   "Chimanimani Behavioral Risks and HIV Serostatus Survey" #,
  # "ZWE", "ZWE2014MICS", 2014,   "MICS"
)

# are there any missing surveys which we have?
length(missing_tbl$survey_id[missing_tbl$survey_id %in% survey_tbl$survey_id]) == 0

missing_tbl <- missing_tbl %>%
  left_join(provider_key, by = "provider") %>%
  mutate(
    across(provider_name, ~ifelse(is.na(.), provider, .)),
    status = "Missing"
  )

# join missing surveys in with surveys present
survey_tbl <- bind_rows(survey_tbl, missing_tbl) %>%
  # join in types information
  left_join(types_tbl, by = "survey_id") %>%
  # translate iso3 to words
  mutate(
    country = countrycode::countrycode(
      iso3, origin = "iso3c", destination = "country.name"
    ),
    country = case_when(
      # country == "Congo - Brazzaville" ~ "Republic of the Congo",
      country == "Congo - Brazzaville" ~ "Congo",
      # country == "Congo - Kinshasa"    ~ "Democratic Republic of the Congo",
      country == "Congo - Kinshasa"    ~ "DR Congo",
      TRUE                             ~ country)
  ) %>%
  # re-order surveys and arrange appropriately
  select(
    iso3, country, survey_id, provider, provider_name, year, N, Neff, type, status
  ) %>%
  arrange(iso3, year, provider, N)


# Pull regional information for each country
loc <- RCurl::getURL("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
iso_df <- read.csv(text = loc)
iso_df <- iso_df %>%
  filter(region == "Africa") %>%
  mutate(across(intermediate.region, ~ifelse(. == "", sub.region, .))) %>%
  select(
    iso3 = alpha.3, region = intermediate.region
  ) %>%
  mutate(region = ifelse(region == "", "Other", region))

survey_tbl <- survey_tbl %>%
  left_join(iso_df, by = "iso3")

# save
# readr::write_csv(survey_tbl, "data/tabulated_surveys.csv")

## Plotting ##

# Figure 1: summary
table(survey_tbl$provider)
table(survey_tbl$provider)


plot_surveys <- survey_tbl %>%
  # don't want missing surveys
  filter(!is.na(N)) %>%
  # pivot type column wider, with N as values
  tidyr::pivot_wider(names_from = type, values_from = N) %>%
  # drop any pre-existing factor levels
  droplevels() %>%
  # arrange region as a factor, approximately counter-clockwise
  mutate(
    region = factor(region, levels = c("Western Africa", "Middle Africa",
                                       # "Southern Africa", "Eastern Africa")
                                       "Eastern Africa", "Southern Africa")
    )
  ) %>%
  # arrange by region (as a factor) and country (alphabetically)
  arrange(region, country) %>%
  # convert country to factor
  mutate(country = fct_inorder(country))


fig1data <- plot_surveys %>%
  mutate(
    # plot position for countries, combining alphabetical order + region
    # add for region to highlight change in region on plot
    country_idx = as.integer(fct_rev(country)) +
      c(0, 1, 2, 3)[
        match(region, rev(c("Western Africa", "Middle Africa",
                            # "Southern Africa", "Eastern Africa")))
                            "Eastern Africa", "Southern Africa")))
      ],
    # Used for point shapes in later dotplot
    data = ifelse(
      # !is.na(`MMC, TMC, MC`), "Distinct Types", "No Type Info"
      !is.na(`MMC, TMC, MC`), "Present", "Unavailable"
    )
  ) %>%
  # get N again, as it is used for point sizes
  mutate(N = ifelse(is.na(`MMC, TMC, MC`), MC, `MMC, TMC, MC`))

# labels to display for each country
country_labels <- fig1data %>%
  distinct(region, country, country_idx)

# position on plot grid for each country
country_positions <- fig1data %>%
  group_by(region) %>%
  summarise(max(country_idx)) %>%
  pull()
country_positions <- country_positions + 1

# Add PHIA surveys
fig1data <- fig1data %>% 
  bind_rows(
    tribble(
      ~ iso3, ~country
    )
  )


p1 <- fig1data %>%
  # specify aesthetic varibales
  ggplot(
    aes(year, country_idx, colour = provider, shape = data, size = N)
  ) +
  # add horizontal lines for regional country (don't have line on top)
  geom_hline(yintercept = country_positions[-1]) +
  # annotate plot with (vertical) regional labels
  annotate(
    "text",
    # 2000.7,
    2020,
    # have labels in between horizontal lines
    zoo::rollmean(c(country_positions, 0), 2),
    label = c("Western", "Central", "Eastern", "Southern"),
    angle = 270,
    fontface = "bold",
    size = 6
  ) +
  geom_point(stroke = 1) +
  # specify x and y labels, subbing country name for country_idx
  # scale_x_continuous("Survey Year", breaks = 2001:2019, minor_breaks = NULL) +
  scale_x_continuous(
    "Survey Year", 
    breaks = seq(2001, 2019, by = 2), 
    # minor_breaks = NULL
    minor_breaks = waiver()
  ) +
  scale_y_continuous(
    element_blank(),
    breaks = country_labels$country_idx,
    minor_breaks = NULL,
    labels = country_labels$country,
    position = "left",
    expand = expand_scale(add = 0.6)
  ) +
  # New England Journal of Medicine colourscheme
  ggsci::scale_colour_nejm() +
  theme_light(10) +
  guides(
    colour = guide_legend(order = 1),
    shape = guide_legend(order = 2),
    size = guide_legend(order = 3)
  ) +
  labs(
    colour = "Survey Type",
    # shape = "Type Distinction",
    shape = "Information on Circumcision Type",
    size = "Sample Size"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1.0, size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 15, face= "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing = unit(0, "cm"),
    legend.key.size = unit(0, "pt"),
    legend.justification = c(1, 0),
    legend.text.align = 0,
    # plot.margin = margin(t = 1, l = 18, unit = "pt")
    plot.margin = margin(0.25, 1, 0, 0, "cm") 
  ) +
  coord_cartesian(xlim = c(2002, 2018.75), clip = "off")
p1

# save dims? at 800 x 800 now, but could be twice as big! Could make plot 
# text bigger then as well

#### Figure 2: Map of MC Coverage across SSA 2010-2021 15-29 year olds ####

## Map Plot ##
main_title <- paste0(
  "Male Circumcision Coverage, ",
  paste0(spec_years[1], "-", spec_years[2]),
  ", age ",
  spec_age_group,
  " years"
)

# add non-SSA countries to areas
areas_all_afr <- bind_rows(areas, non_ssa_afr_areas) %>% 
  filter(!is.na(iso3))

# ggplot(areas_all_afr) +
#   geom_sf(data = areas_all_afr,
#           colour = "black",
#           size = 0.5,
#           fill = NA)

p2 <- plt_coverage_map(
  # results_agegroup   = filter(results_agegroup, type == "MC coverage"),
  results_agegroup   = results_agegroup,
  areas              = areas_all_afr,
  colourPalette      = colourPalette,
  spec_age_group     = spec_age_group,
  spec_years         = spec_years,
  spec_model         = "No program data",
  plot_type          = "map",
  country_area_level = 0, 
  inc_difference     = TRUE,
  spec_main_title    = main_title
)

p2a <- p2$Total + 
  ggtitle("") + # remove title
  # ggtitle("Male Circumcision Coverage, 2010-2021") +
  theme(
    # increase plot title size
    # plot.title = element_text(size = 30, face = "bold"),
    # bold facet names
    strip.text.x = element_text(size = 26, face = "bold")
)
p2a

# export at 1400 x 1400

## Maps for Traditional and Medical Circumcision

p2b <- p2$Medical + 
  ggtitle("Medical Circumcision") + 
  # facet_wrap(~ year, dir = "v") + # make maps vertical
  # # make colour bar vertical and smaller
  # scale_fill_gradientn(
  #   colours = colourPalette, 
  #   # breaks = seq(0, 1, by = 0.2),
  #   breaks = seq(0, 1, by = 0.2),
  #   limits = c(0, 1),  
  #   label = scales::label_percent(accuracy = 1),
  #   guide = guide_colourbar(
  #     direction = "vertical", 
  #     label = TRUE, 
  #     draw.ulim = TRUE,
  #     draw.llim = TRUE,
  #     frame.colour = "black", 
  #     ticks = TRUE, 
  #     # barheight = 1,
  #     barheight = 30,
  #     # barwidth = 30
  #     barwidth = 1
  #   )
  # ) + 
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    # bold facet names
    strip.text.x = element_text(size = 26, face = "bold"),
    legend.position = "none"
  )
p2b

p2c <- p2$Traditional + 
  ggtitle("Traditional Circumcision") + 
  scale_fill_gradientn(
    colours = colourPalette,
    breaks = seq(0, 1, by = 0.2),
    limits = c(0, 1),
    label = scales::label_percent(accuracy = 1),
    guide = guide_colourbar(
      label = TRUE,
      draw.ulim = TRUE,
      draw.llim = TRUE,
      frame.colour = "black",
      ticks = TRUE,
      # barheight = 1,
      barheight = 1,
      barwidth = 30
    )
  ) + 
  theme(
    legend.text = element_text(size = 20), 
    plot.title = element_text(size = 26, face = "bold"),
    # bold facet names
    strip.text.x = element_text(size = 26, face = "bold"),
    legend.position = "bottom"
  )
p2c


#### Figure 3: National Level Geofaceted Coverage 2010-2020 10-29 year olds ####

# Create area plot of (type-split) coverage for each country

p3 <- plt_coverage_year_national(
  # results_agegroup = results_agegroup %>% filter(iso3 != "GMB"),
  results_agegroup = results_agegroup,
  areas            = areas,
  last_surveys     = last_surveys,
  spec_age_group   = spec_age_group,
  # spec_years       = spec_years,
  spec_years       = spec_years[1]:last(spec_years),
  spec_model       = "No program data",
  main             = "",
  # n_plots          = length(unique(results_agegroup_esa_wca[[i]]$iso3))
  n_plots          = length(unique(results_agegroup$iso3))
)[[1]]

# grid for geofaceting, standardise country names
ssa_grid <- geofacet::africa_countries_grid1 %>%
  mutate(
    name = case_when(
      grepl("Ivoire", name)                      ~ "Cote d'Ivoire",
      name == "Gambia"                           ~ "The Gambia",
      name == "Democratic Republic of the Congo" ~ "DR Congo",
      name == "Republic of the Congo"            ~ "Congo",
      name == "Equatorial Guinea"                ~ "Eq. Guinea",
      name == "Central African Republic"          ~ "Cent. Af. Rep.",
      TRUE                                       ~ name
    )
  ) %>%
  filter(name %in% c(ssa_countries, "Eq. Guinea", "Cent. Af. Rep.")) %>% 
  # filter(name != "Senegal") %>% 
  # mutate(name = ifelse(name == "The Gambia", "Senegal", name)) %>% 
  identity()

# remove missing rows and columns (only looking at SSA, not all of Africa)
min_row <- min(ssa_grid$row)
if (min_row > 1) {
  ssa_grid <- ssa_grid %>%
    mutate(row = row - (min_row - 1))
}
min_col <- min(ssa_grid$col)
if (min_col > 1) {
  ssa_grid <- ssa_grid %>%
    mutate(col = col - (min_col - 1))
}

p3_geo <- p3 +
  # geofacet based on SSA shape
  geofacet::facet_geo(~ area_name, grid = ssa_grid) +
  # make x-axis text smaller to fit all of map
  ggtitle("MMC and TMC Coverage, ages 15-29, 2010-2021") +
  theme(
    strip.text = element_text(size = 18, face = "bold"),
    legend.position = "none", # remove legend, add with crop
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Arial"),
    # axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1)
    axis.text.x = element_text(size = 12, angle = 0, hjust = 1, vjust = 1)
  ) + 
  labs(y = "") + # remove x-axis
  scale_x_continuous(
    # breaks = seq(spec_years[1], last(spec_years), by = 2),
    breaks = seq(spec_years[1], last(spec_years), by = 5),
    limits = c(spec_years[1] - 0.25, last(spec_years) + 0.75)
  )
p3_geo

# for legend: 
# p3_geo +
#   # increase size of all annotations
#   theme(
#     plot.title = element_text(size = 10, face = "bold"),
#     strip.text.x = element_text(size = 10, face = "bold"),
#     legend.position = "bottom",
#     legend.text = element_text(size = 10, face = "bold")
#   )

# save with higher height (1200 x 800)

#### Sub-National Variation in MC Coverage Plot ####

# order plot West to East, North to South
plot_order <- c(
  "SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", 
  "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "SSD", "ETH", "GAB", "COG", "COD",
  "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "ZWE", "NAM", 
  "SWZ", "LSO", "ZAF"
)
plot_order <- plot_order[plot_order %in% results_agegroup$iso3]

# take MC coverage for specific age group and final year
plt_data <- results_agegroup %>%
  filter(
    age_group == spec_age_group, 
    year == last(spec_years),
    type == "MC coverage"
  ) %>% 
  # add country column 
  mutate(
    country = countrycode::countrycode(iso3, "iso3c", "country.name"),
    country = case_when(
      country == "Congo - Brazzaville" ~ "Congo",
      country == "Congo - Kinshasa"    ~ "DR Congo",
      TRUE                             ~ country)
  ) %>%
  # join in ESA-WCA distinctions
  left_join(threemc::esa_wca_regions) %>% 
  # join in age-group populations
  left_join(select(populations, area_id, age_group, population)) %>% 
  group_by(iso3, area_level) %>%
  # for each country, take district-level results (or provincial if not possible)
  # filter(area_level == min(2, max(area_level))) %>% 
  # weigh population by average district population in each country
  mutate(population = population / median(population)) %>%
  ungroup()

p4 <- plt_data %>% 
  # take max area level for each country
  group_by(iso3) %>% 
  filter(area_level == max(area_level)) %>% 
  ggplot(
    aes(
      # countries on the x-axis, in specified order
      x = forcats::fct_rev(forcats::fct_relevel(iso3, levels = plot_order)), 
      # median MC Coverage on the y-axis
      y = median
    )
  ) +
  # add points coloured by region with weighted populations determining size
  geom_jitter(
    aes(color = region, size = population), 
    shape = 20, 
    width = 0.1, 
    alpha = 0.5
  ) +
  # add median national level to plot as white dots
  geom_point(
    data = filter(plt_data, area_level == 0),
    shape = 21, 
    # size = 4, 
    size = 6,
    fill = "white", 
    col = "black", 
    # alpha = 0.9
    alpha = 1
  ) +
  # add horizontal line at 90% circumcision
  geom_hline(
    yintercept = 0.9, 
    size = 0.8,
    linetype = "dashed",
    colour = "grey50"
  ) +
  moz.utils::standard_theme() + # Oli's pretty theme
  scale_x_discrete(
    labels = function(x, family = "bold", colour = "black") {
      labs <- countrycode::countrycode(x, "iso3c", "country.name")
      labs <- ifelse(
        labs == "Congo - Brazzaville", 
        "DR Congo",
        ifelse(
          grepl("Congo", labs),
          "Congo",  
          countrycode::countrycode(x, "iso3c", "country.name")
        )
      )
      labs <- glue::glue(
        "<b style='font-family:{family}; color:{colour}'>{labs}</b>"
      )
    }
  ) +
  scale_y_continuous(
    # n.breaks = 5, 
    breaks = c(0, 0.25, 0.5, 0.75, 0.9, 1),
    labels = scales::percent # , 
    # limits = c(0, 1)
  ) +
  scale_size_continuous(
    # breaks = c(0.5, 1, 10, 20, 100), 
    breaks = c(1, 10, 20), 
    # labels = paste0(c(0.5, 1, 10, 20, 100), "x")
    labels = paste0(c(1, 10, 20), "x")
  ) +
  labs(
    y = "Median Male Circumcision Coverage", 
    x = element_blank(), 
    # size = "District pop. relative\nto median district size", 
    size = "District pop. relative to\n median district size", 
    color = "Region"
  ) +
  ggtitle("District-Level MC Coverage, 2020") + 
  scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1, 4)]) +
  theme(
    legend.title.align = 0.5,
    # legend.text.align = -3, # not working!
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 15),
    legend.position = "bottom",
    axis.title.x = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = c(rep(15, 3), 18, 15)),
    axis.text.y = ggtext::element_markdown(size = 17), # hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  ) + 
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  # coord_flip(ylim = c(0.04, 0.975), clip = "off")
  coord_flip(clip = "off")
p4

#### Ridge Plot ####

# iso3_with_type <- results_age %>% 
#   filter(type == "MMC coverage", mean != 0) %>% 
#   distinct(iso3) %>% 
#   pull()

results_age_target <- results_age %>% 
  filter(area_id %in% target_iso3) %>% 
  mutate(
    area_name = case_when(
      area_id == "TZA" ~ "Tanzania",
      grepl("ETH", area_id) ~ "Gambella (Ethiopia)",
      TRUE ~ area_name
    )
  )

areas_spec <- areas %>% 
  filter(area_id %in% target_iso3) %>% 
  mutate(
    area_name = case_when(
      area_id == "TZA" ~ "Tanzania",
      grepl("ETH", area_id) ~ "Gambella (Ethiopia)",
      TRUE ~ area_name
    )
  )

# 
# results_age <- results_age %>% 
#   mutate(area_id = case_when(
#     iso3 == ""
#   )
#   ))
  

plt_circ_age_ridge(
  results_age_target,
  areas_spec,
  spec_ages = 0:30, 
  area_levels = c(0, 1),
  # n_plots = 12
  # n_plots = ceiling(length(target_iso3[target_iso3 != "BWA"]) / 2)
  n_plots = 14
)[[1]] + 
  ggtitle("Mean Age at Circumcision, Split by Type") + 
  theme(plot.title = element_text(size = 22, face = "bold"))

#### Figure 5: Table ####

# national level age-group results for target countries for 10-29 year olds
results_target <- results_agegroup %>%
  filter(
    area_id %in% target_iso3,
    age_group == "10-29",
    year == "2020",
    grepl("coverage", type)
  ) %>%
  select("Country" = area_name, type, mean, upper, lower) %>%
  mutate(
    type = case_when(
      type == "MC coverage"  ~ "MC Coverage",
      type == "MMC coverage" ~ "MMC Coverage",
      type == "TMC coverage" ~ "TMC Coverage"
    ),
    Country = case_when(
      Country == "Gambella"      ~ "Gambella Province (Ethiopia)",
      grepl("Tanzania", Country) ~ "Tanzania",
      TRUE                       ~ Country
    )
  ) 

results_upper <- results_target %>% 
  select(Country, type, mean = upper) %>% 
  mutate(
    type = case_when(
      grepl("MMC", type) ~ "upper_mmc",
      grepl("TMC", type) ~ "upper_tmc",
      TRUE               ~ "upper_mc"
    )
  )

results_lower <- results_target %>% 
  select(Country, type, mean = lower) %>% 
  mutate(
    type = case_when(
      grepl("MMC", type) ~ "lower_mmc",
      grepl("TMC", type) ~ "lower_tmc",
      TRUE               ~ "lower_mc"
    )
  )

results_target <- results_target %>% 
  select(-c(lower, upper)) %>% 
  bind_rows(results_upper, results_lower) %>%
   # mutate(mean = round(mean, 2)) %>% 
  tidyr::pivot_wider(names_from = "type", values_from = mean)

# add rows for missing countries
if (length(missing_iso3) != 0) {
  results_target <- bind_rows(
    results_target,
    data.frame(
      "Country" = countrycode::countrycode(
        missing_iso3, origin = "iso3c", destination = "country.name"
      )
    )
  )
}

# function to convert val between 0 & 1 to red-green-blue scale from map plots
red_green_blue_pal <-  function(x) {
  if (is.na(x)) {
    "#808080" # grey
  } else { 
    rgb(colorRamp(c(
      # "#5e4fa2", "#3288bd", 
      "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#ffffbf", 
      "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142"
    ))(x), maxColorValue = 255)
  }
}

# function to give cells colour
cell_style <- function(value) {
  color <- red_green_blue_pal(value)
  # list(background = color, font = "arial", fontWeight = "bold")
  list(background = color, fontWeight = "bold")
}

# function to format cells 
cell_format <- function(value, index, df, type) {
  # browser()
  if (!is.na(value)) {
    # format values to force two s.f. and to percentage
    val <- format(round(100 * value, 1), nsmall = 1)
    # add leading 0 to val if required
    # if (value < 0.1) val <- paste0(0, val)
    
    # format lower and upper CIs
    lower_val <- df[index, paste0("lower_", type)]
    lower <- format(round(100 * lower_val, 1), nsmall = 1)
    # if (lower_val < 0.1) lower <- paste0(0, lower)
    
    upper_val <- df[index, paste0("upper_", type)]
    upper <- format(round(100 * upper_val, 1), nsmall = 1)
    # if (upper_val < 0.1) upper <- paste0(0, upper)
    
    # return formatted cell
    glue("{val}% ({lower}-{upper}%)") 
  } else glue("")
}

# reactable table 
p5 <- reactable(
  # remove CI columns
  data = select(results_target, -c(contains("upper"), contains("lower"))),
  defaultPageSize = 14, # show all 14 countries 
  # define & format columns
  columns = list(
    Country = colDef(
      align = "left" # ,
      # maxWidth = 50
    ),
    `MC Coverage` = colDef(
      name = "Male Circumcision Coverage",
      align = "center", # align to the right
      cell = function(value, index) {
        cell_format(value, index, results_target, type = "mc")
      },
      style = function(value) cell_style(value) # ,
      # maxWidth = 50
    ),
    `MMC Coverage` = colDef(
      name = "Medical Male Circumcision Coverage",
      align = "center", 
      cell = function(value, index) {
        cell_format(value, index, results_target, type = "mmc")
      },
      style = function(value) cell_style(value) # ,
       # maxWidth = 50
    ),
    `TMC Coverage` = colDef(
      name = "Traditional Male Circumcision Coverage",
      align = "center", 
      cell = function(value, index) {
        cell_format(value, index, results_target, type = "tmc")
      },
      style = function(value) cell_style(value) # ,
      # maxWidth = 50
    )
  ),
  defaultSorted = "MC Coverage", 
  defaultSortOrder = "desc",
  style = list(fontFamily = "Arial"),
  bordered = TRUE,
  defaultColDef = colDef(sortNALast = TRUE), 
  width = 800,
  showSortIcon = FALSE
)
p5 <- p5 %>% 
  # reactablefmtr::add_title(
  #   # "UNAIDS Priority Country Circumcision Coverage, 2020, age 10-29 years",  
  #   "UNAIDS VMMC Priority Countries \n
  #   Circumcision coverage in 2020 among men 10-29 years",
  #   align = "left", 
  #   font_size = 20, 
  # )
  htmlwidgets::prependContent(
    htmltools::tags$h1(
      "UNAIDS VMMC Priority Countries",
      style = "
      font-family: Arial; 
      margin-bottom: -20px
      "
    ),
    htmltools::tags$h2(
      "Circumcision Coverage, 2020",
      style = "
      font-family: Arial; 
      margin-bottom: -1px
      "
    )
)
p5

# save with dimensions 1500 x 1200? (smaller than this anyway!)

#### DMPPT2 Comparison Plots ####

#### Coverage vs Year ####

# Need to do for: MOZ (country level), KEN, LSO, RWA, ZWE
# Increase annotation size!!!

survey_data_2 <- survey_data %>% 
  filter(
    (indicator == "circumcised" & iso3 != "LSO") | 
      (indicator == "circ_medical" & iso3 == "LSO")
  )

# make title, x-axis and facet labels larger

df_results <- left_join(
    results_agegroup,
    select(sf::st_drop_geometry(areas), area_id, area_level_label) 
)

p_moz <- plt_dmppt2_compare_year(
  circ_data   = df_results %>% filter(iso3 == "MOZ"),
  dmppt2_data = dmppt2_data,
  survey_data = survey_data_2,
  # age_per = "15-49",
  age_per = spec_age_group,
  years = first(spec_years):last(spec_years),
  area_levels = 0,
  xlab = "Year",
  ylab = "Circumcision Coverage",
  # title = main_title,
  title = "Black line denotes DMPPT2 coverage, Blue dots denote survey coverage, ",
  n_plots = 100
)[[1]] + 
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, face = "bold"),
    strip.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none"
  ) 
p_moz

p_mwi <- plt_dmppt2_compare_year(
  circ_data   = df_results %>% filter(iso3 == "MWI"),
  dmppt2_data = dmppt2_data,
  survey_data = survey_data_2,
  # age_per = "15-49",
  age_per = spec_age_group,
  years = first(spec_years):last(spec_years),
  area_levels = 0,
  xlab = "Year",
  ylab = "Circumcision Coverage",
  # title = main_title,
  title = "Black line denotes DMPPT2 coverage, Blue dots denote survey coverage, ",
  n_plots = 100
)[[1]] + 
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, face = "bold"),
    strip.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none"
  ) 
p_mwi

p_ken <- plt_dmppt2_compare_year(
  circ_data   = df_results %>% filter(iso3 == "KEN"),
  dmppt2_data = dmppt2_data,
  survey_data = survey_data_2,
  # age_per = "15-49",
  age_per = spec_age_group,
  years = first(spec_years):last(spec_years),
  area_levels = 1,
  xlab = "Year",
  ylab = "Circumcision Coverage",
  # title = main_title,
  title = "Black line denotes DMPPT2 coverage, Blue dots denote survey coverage, ",
  n_plots = 100
)[[1]] + 
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, face = "bold"),
    strip.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
p_ken

p_lso <- plt_dmppt2_compare_year(
  circ_data   = df_results %>% filter(iso3 == "LSO"),
  dmppt2_data = dmppt2_data,
  survey_data = survey_data_2,
  # age_per = "15-49",
  age_per = spec_age_group,
  years = first(spec_years):last(spec_years),
  area_levels = 1,
  xlab = "year",
  ylab = "circumcision coverage",
  # title = main_title,
  title = "black line denotes dmppt2 coverage, blue dots denote survey coverage, ",
  n_plots = 100
)[[1]] + 
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, face = "bold"),
    strip.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
p_lso

p_rwa <- plt_dmppt2_compare_year(
  circ_data   = df_results %>% filter(iso3 == "RWA"),
  dmppt2_data = dmppt2_data,
  survey_data = survey_data_2,
  # age_per = "15-49",
  age_per = spec_age_group,
  years = first(spec_years):last(spec_years),
  area_levels = 1,
  xlab = "year",
  ylab = "circumcision coverage",
  # title = main_title,
  title = "black line denotes dmppt2 coverage, blue dots denote survey coverage, ",
  n_plots = 100
)[[1]] + 
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, face = "bold"),
    strip.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
p_rwa

p_zwe <- plt_dmppt2_compare_year(
  circ_data   = df_results %>% filter(iso3 == "ZWE"),
  dmppt2_data = dmppt2_data,
  survey_data = survey_data_2,
  # age_per = "15-49",
  age_per = spec_age_group,
  years = first(spec_years):last(spec_years),
  area_levels = 1,
  xlab = "Year",
  ylab = "Circumcision Coverage",
  # title = main_title,
  title = "Black line denotes DMPPT2 coverage, Blue dots denote survey coverage, ",
  n_plots = 100
)[[1]] + 
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, face = "bold"),
    strip.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
p_zwe

p_tza <- plt_dmppt2_compare_year(
  circ_data   = df_results %>% filter(iso3 == "TZA"),
  dmppt2_data = dmppt2_data,
  survey_data = survey_data_2,
  # age_per = "15-49",
  age_per = spec_age_group,
  years = first(spec_years):last(spec_years),
  area_levels = 0,
  xlab = "Year",
  ylab = "Circumcision Coverage",
  # title = main_title,
  title = "Black line denotes DMPPT2 coverage, Blue dots denote survey coverage, ",
  n_plots = 100
)[[1]] + 
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, face = "bold"),
    strip.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )
p_tza

#### Threemc Coverage vs DMPPT2 Coverage ####

# Need to do for: ZMB, ZWE, TZA (actually can just leave as is in slides)


#### Stats ####

# New circumcisions performed 
## number of circumcisions performed from 2010 to 2020 (with error bounds)
results_agegroup_n_circ %>%
  filter(
    area_level == 0,
    age_group == spec_age_group,
    type %in% c(
      "Number circumcised (MC)",
      "Number circumcised (MMC)",
      "Number circumcised (TMC)"
    ),
    year %in% spec_years
  ) %>%
  arrange(area_id, type, year) %>%
  group_by(area_name, type) %>%
  mutate(
    circs = c(0, diff(mean)),
    upper_circs = c(0, diff(upper)),
    lower_circs = c(0, diff(lower))
  ) %>%
  ungroup() %>% 
  # group_by(type) %>%
  group_by(type) %>% 
  summarise(across(contains("circs"), sum))

# countries reaching above 80% circumcision: all SSA countries
results_agegroup %>% 
  filter(
    area_id %in% iso3,
    age_group == spec_age_group,
    type == "MC coverage",
    year == last(spec_years),
    mean > 0.8
  ) %>% 
  distinct(area_id) %>% 
  pull()

results_agegroup %>% 
  filter(
    area_id %in% target_iso3,
    age_group == spec_age_group,
    type == "MC coverage",
    year == last(spec_years),
    mean > 0.8
  ) %>% 
  distinct(area_id) %>% 
  pull()

# Within country median difference in coverage between districts
# lowest and highest coverage, smallest and largest 
temp <- results_agegroup %>%
  filter(
    age_group == spec_age_group,
    type == "MC coverage",
    year == last(spec_years)
  ) %>%
  group_by(iso3) %>%
  # filter(area_level == min(max(area_level), 2))
  filter(area_level == max(area_level))

temp1 <- temp %>%
  filter(mean == min(mean))
temp2 <- temp %>%
  filter(mean == max(mean))

temp <- rbind(temp1, temp2) %>%
  ungroup() %>%
  arrange(iso3, mean)

# find difference between highest and lowest coverage for each country
final <- temp %>%
  group_by(iso3) %>%
  summarise(mean = diff(mean), .groups = "drop") %>%
  arrange(desc(mean))

# find median
median(final$mean)

# find range for country with lowest and highest variation
temp %>%
  filter(iso3 == !!last(final$iso3))
temp %>%
  filter(iso3 == !!first(final$iso3))
