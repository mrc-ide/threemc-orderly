library(dplyr)
library(sf)
library(naomi)
library(readr)
library(here)
library(tidyr)

iso3 <- c("ago", "bdi", "ben", "bfa", "bwa", "caf", "civ", "cmr", "cod",
          "cog", "eth", "gab", "gha", "gin", "gmb", "gnb", "gnq", "hti",
          "ken", "lbr", "lso", "mli", "moz", "mwi", "nam", "ner", "nga",
          "rwa", "sen", "sle", "swz", "tcd", "tgo", "tza", "uga", "zmb",
          "zwe")



pop_files <- c(ago = "ago_population_worldpop-2015.csv",
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
               zwe = "zwe_population_nso.csv")

iso3 <- intersect(iso3, names(pop_files))

pjnz_files <- c(list.files("~/Data/Spectrum files/2021 final shared/ESA", recursive = TRUE, full.names = TRUE),
                list.files("~/Data/Spectrum files/2021 final shared/WCA", recursive = TRUE, full.names = TRUE))
names(pjnz_files) <- tolower(vapply(pjnz_files, eppasm::read_iso3, character(1)))

pjnz_files <- pjnz_files %>%
  split(names(.))

pjnz_files <- pjnz_files[names(pjnz_files) %in% iso3]


specpop_raw <- lapply(pjnz_files, naomi::extract_pjnz_naomi)

specpop <- specpop_raw %>%
  Map(mutate, ., iso3 = toupper(names(.))) %>%
  bind_rows() %>%
  as_tibble()

specpop <- select(specpop, iso3, everything())


#' # Areas files

areas <- read_sf("areas.geojson")
areas <- filter(areas, iso3 %in% toupper(iso3))

#' # Raw district population

pop_raw <- lapply(pop_files, function(x) read_csv(here::here(file.path("raw", "population", x))))

pop_raw <- pop_raw %>%
  Map(mutate, ., iso3 = toupper(names(.))) %>%
  bind_rows() %>%
  select(iso3, area_id, area_name, source, calendar_quarter, sex, age_group, population)


pop_raw <- pop_raw %>%
  filter(iso3 != "BWA", !grepl("KEN_3", area_id))

#' Remaining MOZ issue: Maputo area_id

#' Interpolate population annually for 2000 to 2021
pop_interp <- pop_raw %>%
  mutate(year = as.integer(substr(calendar_quarter, 3, 6))) %>%
  select(-calendar_quarter, -source) %>%
  filter(year %in% 2000:2021) %>%
  mutate(year = factor(year, 2000:2021)) %>%
  complete(nesting(iso3, area_id, area_name), sex, age_group, year) %>%
  mutate(year = type.convert(year, as.is = TRUE))

pop_interp <- pop_interp %>%
  arrange(iso3, area_id, sex, age_group, year) %>%
  group_by(iso3, area_id, sex, age_group) %>%
  mutate(population = naomi:::log_linear_interp(population, year)) %>%
  ungroup()


#' Graduate 5-year age group Stats SA populations to single-year of age.

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

#' This takes a while (about 5 minutes on desktop)
system.time(pop1_lst <- parallel::mclapply(pop5_lst, expand_single_age, mc.cores = 12))
pop1 <- bind_rows(pop1_lst)

stopifnot(
  round(sum(pop1$population), 1) == round(sum(pop_interp$population), 1)
)

#' Adjust district populations to match Spectrum population by single
#' year of age

#' Bit of data cleaning
#' * TGO -- recode to national Spectrum file

areas_corrected <- areas %>%
  st_drop_geometry() %>%
  mutate(spectrum_region_code = if_else(iso3 %in% c("TGO", "ZMB"), 0, spectrum_region_code))

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
      select(iso3, spectrum_region_code, year, sex, age, population_spectrum = totpop),
    by = c("iso3", "spectrum_region_code", "year", "sex", "age")
  ) %>%
  mutate(ratio = population_spectrum / population_raw)


pop1adj <- pop1adj %>%
  left_join(
    select(pop1_calib, iso3, area_level, spectrum_region_code, year, sex, age, ratio),
    by = c("iso3", "area_level", "spectrum_region_code", "year", "sex", "age")
  ) %>%
  mutate(population = population * ratio,
         spectrum_region_code = NULL,
         ratio = NULL)

stopifnot(round(sum(pop1adj$population, na.rm = TRUE), 3) ==
            round(sum(pop1_calib$population_spectrum, na.rm = TRUE), 3))

pop1adj <- select(pop1adj, iso3, area_id, area_level, area_name, year, sex, age, population)
pop1adj <- filter(pop1adj, sex == "male")

write_csv(pop1adj, "population_singleage.csv.gz", na = "")
