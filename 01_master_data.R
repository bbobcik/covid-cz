library(tidyr)
library(tibble)
library(readr)
library(dplyr)
library(stringr)


################################################################################
# Stahnout ciselniky obci s rozsirenou pusobnosti (ORP) vcetne vazby na kraje
# a okresy a vcetne informaci o populaci

orp <- read_delim(
    file='http://apl.czso.cz/iSMS/cisexp.jsp?kodcis=65&typdat=0&cisvaz=80007_97&cisjaz=203&format=2&separator=%3B',
    delim=';',
    col_names=c('orp_code', 'name', 'full_name', 'ruian_orp_id'),
    col_types=cols(
        col_skip(),
        col_skip(),
        col_skip(),
        col_integer(),
        col_character(),
        col_character(),
        col_skip(),
        col_skip(),
        col_integer()
    ),
    skip=1L,
    locale=locale(encoding="windows-1250"),
)

orp_region <- read_delim(
    file='http://apl.czso.cz/iSMS/cisexp.jsp?kodcis=65&typdat=1&cisvaz=108_97&cisjaz=203&format=2&separator=%3B',
    delim=';',
    skip=1L,
    col_types=cols(
        col_skip(),
        col_skip(),
        col_skip(),
        col_skip(),
        col_integer(),
        col_character(),
        col_skip(),
        col_skip(),
        col_character(),
        col_character()
    ),
    col_names=c('orp_code', 'name', 'region_code', 'region'),
    locale=locale(encoding="windows-1250")
)

region_abbr <- tribble(~region_code, ~region_abbr,
                       'CZ010', 'Prg',
                       'CZ020', 'Str',
                       'CZ031', 'JCe',
                       'CZ032', 'Plz',
                       'CZ041', 'KVa',
                       'CZ042', 'Ust',
                       'CZ051', 'Lib',
                       'CZ052', 'HKr',
                       'CZ053', 'Par',
                       'CZ063', 'Vys',
                       'CZ064', 'JMo',
                       'CZ071', 'Olo',
                       'CZ072', 'Zli',
                       'CZ080', 'MSl',
)

district <- read_delim(
    file='http://apl.czso.cz/iSMS/cisexp.jsp?kodcis=109&typdat=0&cisjaz=203&format=2&separator=%3B',
    col_types=cols(
        col_skip(),
        col_skip(),
        col_skip(),
        col_character(),
        col_character(),
        col_character(),
        col_skip(),
        col_skip(),
        col_skip()
    ),
    col_names=c('district_code', 'district', 'district_full_name'),
    skip=1L,
    delim=';',
    locale=locale(encoding="windows-1250")
)

orp_population_by_age <- read_csv(
    file="https://www.czso.cz/documents/62353418/137000724/130181-20data043020.csv",
    skip=1L,
    col_types=cols(
        col_skip(),
        col_integer(), # hodnota
        col_skip(),
        col_skip(),
        col_character(), # pohlavi_kod
        col_skip(),
        col_character(), # vek_kod
        col_skip(),
        col_integer(), # vuzemi_kod
        col_skip(),
        col_skip(),
        col_skip(),
        col_skip()
    ),
    col_names=c('count', 'sex', 'age_group', 'orp_code')
) %>% 
    mutate(
        sex = if_else(sex == '1', 'M', 'F'),
        age_from = as.integer(str_sub(age_group, start=3L, end=6L)),
        age_to = as.integer(str_sub(age_group, start=9L, end=12L)),
        age_group = NULL,
    )

orp_population <- orp_population_by_age %>% 
    group_by(orp_code) %>% 
    summarise(
        pop = sum(count),
        pop_young = sum((age_to <= 20L) * count),
        pop_old = sum((age_from >= 65L) * count),
        weighted_age = sum(count * age_from),
        .groups = 'drop'
    ) %>% 
    mutate(
        avg_age = weighted_age / pop,
        q_young = pop_young / pop,
        q_old = pop_old / pop,
        weighted_age = NULL,
    )

orp_info <- orp %>%
    left_join(orp_region, by=c('orp_code'='orp_code', 'full_name'='name')) %>% 
    left_join(region_abbr, by='region_code') %>% 
    left_join(orp_population, by='orp_code') %>% 
    mutate(region_code = factor(region_code))

region_population_by_age <- orp_region %>% 
    inner_join(region_abbr, by='region_code') %>% 
    left_join(orp_population_by_age, by='orp_code') %>% 
    group_by(region_code, region_abbr, region, sex, age_from, age_to) %>% 
    arrange(.by_group=T) %>% 
    summarise(count = sum(count), .groups = 'drop')

region_population <- region_population_by_age %>% 
    group_by(region_code, region_abbr, region, sex) %>% 
    summarise(
        population = sum(count),
        weighted_age = sum(count * age_from),
        .groups = 'drop'
    ) %>%
    pivot_wider(names_from=sex, values_from=c(population, weighted_age)) %>% 
    mutate(
        population = population_M + population_F,
        males = population_M,
        females = population_F,
        q_males = males / population,
        q_females = females / population,
        avg_age = (weighted_age_M + weighted_age_F) / population,
        avg_age_males = weighted_age_M / population_M,
        avg_age_females = weighted_age_F / population_F,
        population_M = NULL,
        population_F = NULL,
        weighted_age_M = NULL,
        weighted_age_F = NULL,
    ) %>% 
    arrange(region_code)

rm(orp, orp_region, region_abbr)


################################################################################
# Ulozit stazena data

saveRDS(orp_info, 'data/orp_info.RDS')
saveRDS(orp_population_by_age, 'data/orp_population_by_age.RDS')
saveRDS(orp_population, 'data/orp_population.RDS')
saveRDS(region_population, 'data/region_population.RDS')
saveRDS(region_population_by_age, 'data/region_population_by_age.RDS')
saveRDS(district, 'data/district.RDS')
