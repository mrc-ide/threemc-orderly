#### Coalating Population data for each country

#### Initial ####

# sharepoint remote
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

# number of cores
cores <- detectCores()

# countries
iso3 <- c("ago", "bdi", "ben", "bfa", "bwa", "caf", "civ", "cmr", "cod",
          "cog", "eth", "gab", "gha", "gin", "gmb", "gnb", "gnq", "hti",
          "ken", "lbr", "lso", "mli", "moz", "mwi", "nam", "ner", "nga",
          "rwa", "sen", "sle", "swz", "tcd", "tgo", "tza", "uga", "zmb",
          "zwe", "zaf")

# countries whose populations are pulled from other orderly tasks
orderly_iso3 <-  c(
  "ben", "cod", "cog", "gab", "lbr", "mli", "ner", "sen", "sle", "tcd"
)

# individual file names for each country
pop_file_names <- sort(c(
               ago = "ago_population_worldpop-2015.csv",
               bdi = "bdi_population_gpw.csv",
               bfa = "bfa_population_gpw.csv",
               bwa = "bwa_population_stats-bw.csv",
               civ = "civ_population.csv",
               cmr = "cmr_population_ins.csv",
               cod = "cod_population_local.csv",
               eth = "eth_population.csv",
               gmb = "gmb_population_moh-projections_health-region.csv",
               hti = "hti_population_ihsi.csv",
               ken = "ken_population_subcounty-khis.csv",
               lso = "lso_population_census16-defacto.csv",
               moz = "moz_population_nso_v2021.csv",
               mwi = "mwi_population_projections18.csv",
               nam = "nam_population_gpw.csv",
               nga = "nga_population_npopc.csv",
               rwa = "rwa_population.csv",
               swz = "swz_population_worldpop.csv",
               tgo = "tgo_population_nso.csv",
               tza = "tza_population_tnbs.csv",
               uga = "uga_population_ubos.csv",
               zmb = "zmb_population_nso.csv",
               zwe = "zwe_population_nso.csv"))

iso3 <- intersect(iso3, names(pop_file_names))

# download population files:

# population folder path
path <- "Shared Documents/Circumcision coverage/raw/population"

# List files in folder
folder <- sharepoint$folder("HIVInferenceGroup-WP", URLencode(path))

# find urls for files
urls <- URLencode(
  file.path("sites/HIVINferenceGroup-WP", paste0(path, "/", folder$files()$name))
)
# only download required urls
urls <- urls[grepl(paste(pop_file_names, collapse = "|"), urls)]
# order alphabetically, so download order matches above order
urls <- urls[order(basename(urls))]
# check urls match pop_file_names
basename(urls) == pop_file_names

# download pop_files to temporary repository
# pop_files <- unlist(lapply(urls, sharepoint$download))
pop_files <- unlist(mclapply(urls, sharepoint$download, mc.cores = cores))
names(pop_files) <- pop_file_names

# download pjnz_files

# pjnz_files <- c(list.files("~/Data/Spectrum files/2021 final shared/ESA", recursive = TRUE, full.names = TRUE),
#                 list.files("~/Data/Spectrum files/2021 final shared/WCA", recursive = TRUE, full.names = TRUE))

# spectrum_path <- "Shared Documents/Data/Spectrum files/2021 final shared"
spectrum_path <- "Shared Documents/Circumcision coverage/raw/Spectrum files"
path <- list(
  "wca" = file.path(spectrum_path, "WCA"),
  "esa" = file.path(spectrum_path, "ESA")
)

# List files in folders
wca_folder <- sharepoint$folder("HIVInferenceGroup-WP", URLencode(path$wca))
esa_folder <- sharepoint$folder("HIVInferenceGroup-WP", URLencode(path$esa))

urls <- list(wca = paste0(path$wca,"/", wca_folder$files()$name),
             esa = paste0(path$esa,"/", esa_folder$files()$name))
urls <- unlist(lapply(lapply(urls, function(x) {
  file.path("sites/HIVInferenceGroup-WP", x)
}), URLencode))
urls <- urls[tolower(tools::file_ext(urls)) == "pjnz"]

# pjnz_files <- unlist(lapply(urls, function(x) lapply(x, sharepoint$download)))
pjnz_files <- unlist(lapply(urls, function(x) {
  mclapply(x, sharepoint$download, mc.cores = cores)
}))
names(pjnz_files) <- tolower(
  vapply(pjnz_files, eppasm::read_iso3, character(1))
)

pjnz_files <- pjnz_files %>%
  split(names(.))

pjnz_files <- pjnz_files[names(pjnz_files) %in% iso3]

# specpop_raw <- lapply(pjnz_files, naomi::extract_pjnz_naomi)
specpop_raw <- mclapply(pjnz_files, naomi::extract_pjnz_naomi, mc.cores = cores)

specpop <- specpop_raw %>%
  Map(mutate, ., iso3 = toupper(names(.))) %>%
  bind_rows() %>%
  as_tibble() %>% 
  select(iso3, everything())

# Areas files
areas <- read_circ_data("depends/areas.geojson") %>% 
  filter(iso3 %in% c(toupper(iso3), "ZAF"))

#### Raw district population ####

pop_raw <- lapply(pop_files, read_circ_data)

# load orderly populations
pop_orderly <- lapply(
  paste0("depends/", orderly_iso3, "_interpolated_population.csv"), 
  read_circ_data
)
names(pop_orderly) <- orderly_iso3
# join area name into pop_orderly
pop_orderly <- pop_orderly %>% 
  Map(mutate, ., iso3 = toupper(names(.))) %>% 
  bind_rows() %>% 
  left_join(
    (areas %>%
       sf::st_drop_geometry() %>%
       select(iso3, area_id, area_name))
) %>% 
  split(.$iso3)
  
# join with raw populations from sharepoint
pop_raw <- c(pop_raw, pop_orderly)

pop_raw <- pop_raw %>%
  Map(mutate, ., iso3 = toupper(substr(names(.), 0, 3))) %>%
  bind_rows() %>%
  select(iso3, area_id, area_name, source, calendar_quarter, year, sex, age_group, population)

pop_raw <- pop_raw %>%
  filter(iso3 != "BWA", !grepl("KEN_3", area_id))

# pop_raw %>%
#   anti_join(areas, by = "area_id") %>%
#   count(iso3)

# Recode MOZ area_id for Maputo in population dataset
# pop_raw %>%
#   distinct(iso3, area_id_pop = area_id, area_name) %>%
#   filter(iso3 == "MOZ") %>%
#   mutate(area_level = as.integer(substr(area_id_pop, 5, 5))) %>%
#   full_join(
#     areas %>%
#     st_drop_geometry() %>%
#     filter(iso3 == "MOZ") %>%
#     select(area_level, area_id_circ = area_id, area_name)
#   ) %>%
#   filter(area_id_circ != area_id_pop) %>%
#   print(n = Inf)


moz_id_recode <- c("MOZ_2_1209" = "MOZ_2_0101",
                   "MOZ_2_1210" = "MOZ_2_0102",
                   "MOZ_2_1211" = "MOZ_2_0103",
                   "MOZ_2_1212" = "MOZ_2_0104",
                   "MOZ_2_1213" = "MOZ_2_0105",
                   "MOZ_2_1214" = "MOZ_2_0106",
                   "MOZ_2_1215" = "MOZ_2_0107",
                   "MOZ_2_1206" = "MOZ_2_0201",
                   "MOZ_2_1208" = "MOZ_2_0202",
                   "MOZ_2_1202" = "MOZ_2_0203",
                   "MOZ_2_1201" = "MOZ_2_0204",
                   "MOZ_2_1205" = "MOZ_2_0205",
                   "MOZ_2_1207" = "MOZ_2_0206",
                   "MOZ_2_1204" = "MOZ_2_0207",
                   "MOZ_2_1203" = "MOZ_2_0208")

pop_raw <- pop_raw %>%
  mutate(
    area_id = recode(area_id, !!!moz_id_recode)
  )

# pop_raw %>%
#   distinct(iso3, area_id, area_name) %>%
#   anti_join(areas, by = "area_id") %>%
#   print(n = Inf)


#### Interpolate population annually for 2000 to 2021 ####

pop_interp <- pop_raw %>%
  mutate(
    year = ifelse(!is.na(calendar_quarter),
                  as.integer(substr(calendar_quarter, 3, 6)),
                  year)
  ) %>%
  select(-c(calendar_quarter, source)) %>%
  filter(year %in% 2000:2021) %>%
  mutate(year = factor(year, 2000:2021)) %>%
  complete(nesting(iso3, area_id, area_name), sex, age_group, year) %>%
  mutate(year = type.convert(year, as.is = TRUE))

pop_interp <- pop_interp %>%
  arrange(iso3, area_id, sex, age_group, year) %>%
  group_by(iso3, area_id, sex, age_group) %>%
  mutate(population = naomi:::log_linear_interp(population, year)) %>%
  ungroup()

# pop_interp1 <- pop_raw %>%
#   mutate(
#     year = ifelse(!is.na(calendar_quarter),
#                   as.integer(substr(calendar_quarter, 3, 6)),
#                   year)
#   ) %>%
#   select(-c(calendar_quarter, source)) %>%
#   filter(year %in% 2000:2021) %>%
#   mutate(year = factor(year, 2000:2021)) %>%
#   complete(nesting(iso3, area_id, area_name), sex, age_group, year) %>%
#   mutate(year = type.convert(year, as.is = TRUE))
# 
# 
# pop_interp1 <- crossing(area_id = unique(areas$area_id),
#                                      year = 2000:2021,
#                                      sex = c("male"),
#                                      age_group = unique(pop_interp1$age_group)) %>%
#   left_join(
#     (pop_interp1 %>% 
#        select(iso3, area_id, area_name, year, sex, age_group, population))
#   ) %>%
#   group_by(area_id, sex, age_group) %>%
#   mutate(population = exp(zoo::na.approx(log(population), na.rm = FALSE))) %>% 
#   ungroup()

#### Graduate 5-year age group Stats SA pops to single-year of age ####

expand_single_age <- function(df) {

  x <- filter(df, age_group != "Y080_999")
  pop1 <- beers::beers_sub_ordinary(x$population)

  val <- tibble(
    iso3 = x$iso3[1],
    area_id = x$area_id[1],
    year = x$year[1],
    sex = x$sex[1],
    age = seq_along(pop1) - 1,
    population = pop1
  ) %>%
    bind_rows(
      filter(df, age_group == "Y080_999") %>%
      mutate(age = 80) %>%
      select(iso3, area_id, year, sex, age, population)
    )

  val
}

pop5_lst <- pop_interp %>%
  arrange(iso3, area_id, year, sex, age_group) %>%
  split(paste(.$iso3, .$area_id, .$year, .$sex))

# This takes a while (about 5 minutes on desktop)
pop1_lst <- parallel::mclapply(pop5_lst, expand_single_age, mc.cores = cores)
pop1 <- bind_rows(pop1_lst)

# stopifnot(
#   round(sum(pop1$population), 1) == round(sum(pop_interp$population), 1)
# )

#### Adjust district pops to match Spectrum pop by single year of age ####

# Bit of data cleaning
# * TGO, ZMB -- recode to national Spectrum file
# * MOZ -- recode Maputo region code to 22

areas_corrected <- areas %>%
  sf::st_drop_geometry() %>%
  mutate(
    spectrum_region_code = case_when(
      iso3 %in% c("TGO", "ZMB")                       ~ 0,
      iso3 == "MOZ" & spectrum_region_code %in% 19:20 ~ 21,
      TRUE                                            ~ spectrum_region_code)
  )

pop1adj <- pop1 %>%
  left_join(
    areas_corrected %>%
    select(iso3, area_id, area_level, area_name, spectrum_region_code),
    by = c("iso3", "area_id")
  )

pop1_specreg <- pop1adj %>%
  count(iso3, area_level, spectrum_region_code, year, sex, age, wt = population,
        name = "population_raw")

pop1_calib <- pop1_specreg %>%
  left_join(
    specpop %>%
    select(
      iso3, spectrum_region_code, year, sex, age, population_spectrum = totpop
    ),
    by = c("iso3", "spectrum_region_code", "year", "sex", "age")
  ) %>%
  mutate(ratio = population_spectrum / population_raw)
  

pop1adj <- pop1adj %>%
  left_join(
    select(
      pop1_calib, iso3, area_level, spectrum_region_code, year, sex, age, ratio
    ),
    by = c("iso3", "area_level", "spectrum_region_code", "year", "sex", "age")
  ) %>%
  mutate(population = population * ratio,
         spectrum_region_code = NULL,
         ratio = NULL)

# stopifnot(round(sum(pop1adj$population, na.rm = TRUE), 3) ==
#           round(sum(pop1_calib$population_spectrum, na.rm = TRUE), 3))

pop1adj <- pop1adj %>% 
  select(iso3, area_id, area_level, area_name, year, sex, age, population) %>% 
  filter(sex == "male")

# Aggregate MOZ Maputo province populations

pop1adj <- pop1adj %>%
  filter(area_id != "MOZ_1_12") %>%
  bind_rows(
    select(., -c(area_name, area_level)) %>%
    rename(area_id2 = area_id) %>%
    inner_join(
      sf::st_drop_geometry(areas) %>%
      filter(parent_area_id %in% c("MOZ_1_01", "MOZ_1_02")) %>%
      select(area_id2 = area_id, area_id = parent_area_id) %>%
      inner_join(
        select(sf::st_drop_geometry(areas), area_id, area_level, area_name)
      )
    ) %>%
    count(iso3, area_id, area_level, area_name, sex, age, year,
          wt = population, name = "population")
  ) 

#### Add South Africa population ####

# Download data from sharepoint
urls <- list(
  zaf_pop = "sites/HIVInferenceGroup-WP/Shared Documents/Circumcision coverage/raw/zaf/zaf_population_district_singleage.csv",
  zaf_areas = "sites/HIVInferenceGroup-WP/Shared Documents/Circumcision coverage/raw/zaf/zaf_area_hierarchy.csv"
)
urls <- lapply(urls, URLencode)
files <- lapply(urls, sharepoint$download)

zaf_pop <- read_circ_data(files[[1]])
zaf_areas <- read_circ_data(files[[2]])

zaf_pop <- zaf_pop %>%
  filter(sex == "male") %>%
  mutate(
    iso3 = "ZAF",
    year = as.integer(substr(calendar_quarter, 3, 6))
  ) %>%
  left_join(zaf_areas, by = "area_id") %>%
  select(all_of(names(pop1adj))) 

pop1adj <- bind_rows(pop1adj, zaf_pop)

readr::write_csv(pop1adj, "artefacts/population_singleage.csv.gz", na = "")