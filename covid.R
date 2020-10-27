library(tidyr)
library(tibble)
library(readr)
library(lubridate)
library(stringr)
library(forcats)
library(dplyr)
library(purrr)
library(ggplot2)
library(scales)
library(directlabels)
library(zoo)
library(mgcv)
library(cowplot)
library(patchwork)
library(slider)
library(ggrepel)


standard_label <- function(title) {
    labs(title=title, caption='Autor: Boleslav Bobčík, https://github.com/bbobcik/covid-cz\nZdroj dat: MZČR, https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19')
}


custom_theme <- theme_bw() +
    theme(
        plot.caption=element_text(size=8, colour='darkgray')
    )

file_date <- place_cases %>% pull(date) %>% max() %>% strftime('%Y-%m-%d')



################################################################################
# Pomocne funkce

clamp_value <- function(q, default_value=99, clamp=99) {
    if (length(clamp) == 1L) {
        clamp_max <- abs(clamp[1])
        clamp_min <- -clamp_max
    } else {
        clamp_min <- clamp[1]
        clamp_max <- clamp[2]
    }
    q2 <- if_else(is.na(q) | is.nan(q), default_value, q)
    q3 <- pmax(q2, clamp_min)
    q4 <- pmin(q3, clamp_max)
    return (q4)
}


################################################################################
# Nacist ciselniky obci, kraju a okresu

orp_info <- readRDS('data/orp_info.RDS')
orp_population_by_age <- readRDS('data/orp_population_by_age.RDS')
orp_population <- readRDS('data/orp_population.RDS')
region_population <- readRDS('data/region_population.RDS')
region_population_by_age <- readRDS('data/region_population_by_age.RDS')
district <- readRDS('data/district.RDS')


################################################################################
# Stahnout COVID data

covid_orp <- read_delim(
        file='https://onemocneni-aktualne.mzcr.cz/api/account/verejne-distribuovana-data/file/dip%252Fweb_orp.csv',
        delim=";",
        col_types=cols(
            col_skip(),
            col_date("%Y-%m-%d"),
            col_integer(),
            col_skip(),
            col_integer(), # Incidence
            col_integer(),
            col_integer(),
            col_integer(), # Prevalence
            col_integer(),
            col_integer(),
            col_integer(), # Pocet hospitalizovanych
            col_integer(),
            col_integer()
        ),
        col_names=c('date', 'orp_code', 'inc_week', 'inc_week_65', 'inc_week_75', 'prev', 'prev_65', 'prev_75', 'hospit', 'hospit_delta', 'tests'),
        skip=1L,
    ) %>% 
    mutate(
        # Kody ORP jsou v nekterych pripadech posunute
        orp_code = case_when(
            orp_code == 0L            ~ 1000L,            # Praha
            orp_code %in% 2000L:2999L ~ orp_code + 100L,  # Stredocesky kraj
            orp_code %in% 6000L:6999L ~ orp_code - 200L,  # Jihomoravsky kraj
            orp_code %in% 8000L:8999L ~ orp_code + 100L,  # Moravskoslezsky kraj
            TRUE                      ~ orp_code
        ),
        # Odstranit NA
        hospit = coalesce(hospit, 0L),
        tests = coalesce(tests, 0L),
    )

detected_patients <- read_csv(
        file='https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.csv',
        col_types=cols(
            col_date("%Y-%m-%d"),
            col_integer(),
            col_character(),
            col_character(),
            col_character(),
            col_skip(),
            col_character()
        ),
        col_names=c('date', 'age', 'sex', 'region_code', 'district_code', 'origin_country'),
        skip=1L
    ) %>% 
    mutate(
        sex = fct_recode(sex, F='Z', M='M'),
    ) %>% 
    arrange(date, region_code, district_code, age, sex)

saveRDS(covid_orp, 'data/covid_orp.RDS')
saveRDS(detected_patients, 'data/detected_patients.RDS')


covid_full_info <- covid_orp %>% 
    left_join(orp_info, by='orp_code') %>% 
    select(region_abbr, name, date, pop, avg_age, inc_week, prev, hospit, tests,
           inc_week_65, inc_week_75, prev_65, prev_75, hospit_delta,
           pop_young, pop_old, q_young, q_old,
           region, region_code, full_name, orp_code, ruian_orp_id) %>% 
    group_by(region_code, orp_code) %>% 
    arrange(.by_group=T, date) %>% 
    mutate(day_idx = row_number()) %>% 
    ungroup()


covid_dow <- covid_full_info %>% 
    group_by(region_abbr, name, orp_code, ruian_orp_id, pop) %>% 
    arrange(.by_group=T, date) %>% 
    mutate(
        week = floor_date(date, unit="week", week_start=1L),
        dow = factor(wday(date, label=T, abbr=F, locale='Czech'), ordered=T, levels=c('pondělí', 'úterý', 'středa', 'čtvrtek', 'pátek', 'sobota', 'neděle')),
        new_cases = pmax(prev - lag(prev, default=0L), 0),
        new_hospit = pmax(hospit - lag(hospit, default=0L), 0),
    ) %>%
    group_by(.add=T, dow) %>% 
    arrange(.by_group=T) %>% 
    summarise(
        new_cases = sum(new_cases),
        new_hospit = sum(new_hospit),
        prevalence = last(prev),
        hospitalised = last(hospit),
        .groups = 'drop_last'
    ) %>%
    mutate(
        q_new = new_cases / pmax(sum(new_cases), 1),
        c_new = cumsum(q_new),
    ) %>% 
    ungroup()


covid_dow <- covid_full_info %>% 
    group_by(region_abbr, name, orp_code, ruian_orp_id, pop) %>% 
    arrange(.by_group=T, date) %>% 
    mutate(
        week = floor_date(date, unit="week", week_start=1L),
        dow = factor(wday(date, label=T, abbr=F, locale='cs_CZ.UTF8'), ordered=T, levels=c('Pondělí', 'Úterý', 'Středa', 'Čtvrtek', 'Pátek', 'Sobota', 'Neděle')),
        new_cases = pmax(prev - lag(prev, default=0L), 0),
        new_hospit = pmax(hospit - lag(hospit, default=0L), 0),
    ) %>%
    group_by(.add=T, dow) %>% 
    arrange(.by_group=T) %>% 
    summarise(
        new_cases = sum(new_cases),
        new_hospit = sum(new_hospit),
        prevalence = last(prev),
        hospitalised = last(hospit),
        .groups = 'drop_last'
    ) %>%
    mutate(
        q_new = new_cases / pmax(sum(new_cases), 1),
        c_new = cumsum(q_new),
    ) %>% 
    ungroup()

covid_dow_total <- covid_dow %>% 
    group_by(dow) %>% 
    summarise(
        new_cases = sum(new_cases),
        .groups = 'drop'
    ) %>% 
    arrange(dow) %>% 
    transmute(
        dow_idx = as.integer(dow),
        dow,
        new_cases,
        q_new = new_cases / sum(new_cases),
        c_new = cumsum(q_new),
        predict_factor = 1 / c_new,
    )

covid_dow %>%
    group_by(region_abbr, dow) %>% 
    arrange(.by_group=T) %>% 
    summarise(
        q_new_25 = quantile(q_new, 0.25),
        q_new_50 = quantile(q_new, 0.50),
        q_new_75 = quantile(q_new, 0.75),
        q_new_avg = mean(q_new),
        .groups='drop'
    ) %>%
    inner_join(covid_dow_total, by='dow') %>% 
    ggplot(aes(x=dow_idx)) +
    geom_ribbon(aes(ymin=q_new_25, ymax=q_new_75), fill='skyblue', alpha=0.5) +
    geom_line(aes(y=q_new_50), colour='black', size=0.5, alpha=0.5) +
    geom_point(aes(y=q_new_avg), colour='red', size=1.5) +
    geom_line(aes(y=q_new_avg), colour='red', size=0.5) +
    geom_line(aes(y=q_new), colour='black', size=0.3, linetype=2) +
    facet_wrap(vars(region_abbr), ncol=5L) +
    scale_x_continuous(name=NULL, breaks=1:7, labels=levels(covid_dow$dow), minor_breaks=NULL) +
    scale_y_continuous(name=NULL, labels=percent_format(), minor_breaks=NULL) +
    standard_label('Podíl nových případů podle dnů v týdnu') +
    custom_theme +
    NULL

covid_dow_total %>% 
    ggplot(aes(x=dow, y=q_new)) +
    geom_col(fill='darkblue', alpha=0.6) +
    scale_x_discrete(name=NULL) +
    scale_y_continuous(name=NULL, labels=percent_format(), minor_breaks=NULL) +
    standard_label('Podíl detekovaných nákaz podle dnů v týdnu') +
    custom_theme +
    NULL



covid_week_scaling_factor <- function(dates) {
    max_date = max(dates)
    max_dow = wday(max_date, week_start=1L)
    week_factor = covid_dow_total$predict_factor[max_dow]
    return (week_factor)
}



covid_week <- covid_full_info %>% 
    group_by(region_abbr, region, region_code, name, full_name, orp_code, ruian_orp_id, pop, avg_age, pop_young, pop_old, q_young, q_old) %>% 
    arrange(.by_group=T, date) %>% 
    mutate(
        week = floor_date(date, unit="week", week_start=1L),
        new_cases = pmax(prev - lag(prev, default=0L), 0L),
        new_hospit = pmax(hospit - lag(hospit, default=0L), 0L),
    ) %>%
    group_by(.add=T, week) %>% 
    summarise(
        week_factor = covid_week_scaling_factor(date),
        new_cases = sum(new_cases),
        new_hospit = sum(new_hospit),
        prevalence = last(prev),
        hospitalised = last(hospit),
        .groups = 'drop_last'
    ) %>%
    mutate(
        new_cases_actual = new_cases,
        new_cases = as.integer(round(week_factor * new_cases)),
        week_num = 1L - row_number(desc(week)),
    ) %>% 
    ungroup() %>% 
    arrange(week, region_code, orp_code) %>% 
    mutate(
        week_disc = fct_inorder(strftime(week, '%e.%b.')),
    )

covid_region <- covid_week %>% 
    group_by(region_abbr, region, region_code, week, week_num) %>% 
    arrange(.by_group=T) %>% 
    summarise(
        total_pop = sum(pop),
        median_pop = median(pop),
        #
        new_cases_total = sum(new_cases),
        new_cases_25 = quantile(new_cases, 0.25, type=1L),
        new_cases_50 = quantile(new_cases, 0.50, type=1L),
        new_cases_75 = quantile(new_cases, 0.75, type=1L),
        new_cases_mad = mad(new_cases, low=T),
        #
        new_hospit_total = sum(new_hospit),
        new_hospit_25 = quantile(new_hospit, 0.25, type=1L),
        new_hospit_50 = quantile(new_hospit, 0.50, type=1L),
        new_hospit_75 = quantile(new_hospit, 0.75, type=1L),
        new_hospit_mad = mad(new_hospit, low=T),
        #
        prevalence_total = sum(prevalence),
        prevalence_50 = quantile(prevalence, 0.50, type=1L),
        #
        hospitalised = sum(hospitalised),
        avg_age = median(avg_age),
        pop_young = sum(pop_young),
        pop_old = sum(pop_old),
        .groups = 'drop'
    ) %>% 
    mutate(
        q_young = pop_young / total_pop,
        q_old = pop_old / total_pop,
    )


gov_interventions <- tribble(~date, ~intervention_level, ~comment,
    '2020-08-01', 1L, 'Nic',
    '2020-08-28', 2L, 'Praha-oranzova',
    '2020-09-10', 3L, 'Rousky v interierech',
    '2020-10-05', 5L, 'Zacatek nouzoveho stavu',
    '2020-10-09', 8L, 'Uzavreni fitness apod',
    '2020-10-14', 9L, 'Uzavreni divadel',
    '2020-10-21', 10L, 'Povinne rousky',
    '2020-10-22', 12L, 'Uzavreni maloobchodu',
    ) %>% 
    mutate(
        date = ymd(date),
    )

gov_interventions_timeline <- tibble(
    date = seq(min(gov_interventions$date), ymd('2021-12-31'), by='day'),
    ) %>% 
    left_join(gov_interventions, by='date') %>% 
    mutate(
        intervention_start_date = if_else(is.na(intervention_level), NA_Date_, date),
    ) %>% 
    fill(intervention_level, intervention_start_date, .direction='down') %>% 
    mutate(
        intervention_duration = (date - intervention_start_date) / ddays(),
        run_level = slide2_dbl(as.numeric(intervention_level), intervention_duration, ~sum(.x*.y)/sum(.y), .before=7L, .complete=F),
        run_level = if_else(is.nan(run_level), as.numeric(intervention_level), run_level),
        effect_lag_factor = pmax(log10(100 + intervention_duration - 14) / 2, 1),
        decay_factor = pmin(exp(-(intervention_duration-7) / max(1+intervention_duration)), 1),
        intervention_effect = run_level * effect_lag_factor * decay_factor,
    )


(g05 <- covid_week %>% 
    filter(week >= ymd('2020-09-14')) %>% 
    mutate(
        rel_prev = prevalence / pop,
        #rel_new = new_cases / prevalence,
        rel_new = new_cases / (pop - prevalence),
    ) %>%
    arrange(week, desc(pop), region_abbr, desc(rel_prev), orp_code) %>% 
    ggplot(aes(x=rel_prev, y=rel_new, colour=week_disc)) +
    geom_jitter(aes(size=pop), width=0.0002, height=0.0002) +
    geom_smooth(method='gam', formula=y~x, se=F, fullrange=T, na.rm=T, colour="darkred", size=0.4, alpha=0.6) +
    expand_limits(x=c(0,0.03), y=0) +
    scale_x_continuous(name="Počet aktivních případů v poměru k počtu obyvatel", labels=percent_format()) +
    scale_y_continuous(name="Počet nových případů v poměru k počtu zdravých obyvatel", labels=percent_format()) +
    scale_colour_brewer(name="Týden", type="qual", palette="PuBu") +
    scale_size_continuous(name="Počet obyvatel", breaks=c(0,1000,10000,50000,100000,200000,500000,10000000,Inf), labels=number_format(), range=c(0.4, 6)) +
    standard_label("Dynamika nakažlivosti COVID-19 na úrovni ORP") +
    custom_theme +
    theme(
        axis.title.x.bottom=element_text(hjust=0.97, margin=margin(-30,0,20,0, unit='pt')),
        axis.title.y.left=element_text(hjust=0.95, margin=margin(0,-50,0,40, unit='pt')),
    ) +
    NULL)


png(paste0('outputs/', file_date,  '/covid_dyn_', file_date, '.png'), width=3000L, height=2000L, type='cairo', res=120)
g05
dev.off()
png(paste0('outputs/covid_dyn_latest.png'), width=2000L, height=1300L, type='cairo', res=120)
g05
dev.off()






covid_week %>% 
    filter(week_num > -8L) %>% 
    left_join(master_orp, by=c('ruian_orp_id'='orp_id')) %>% 
    mutate(
        rel_prev = prevalence / pop,
        rel_new = new_cases / (pop - prevalence),
    ) %>%
    #filter(rel_prev >= quantile(rel_prev, 0.75)) %>% 
    ggplot(aes(x=pop/places, rel_new, colour=week_disc)) +
    geom_point() +
    expand_limits(y=0) +
    scale_x_continuous(trans=pseudo_log_trans(1,10), breaks=log_breaks(n=8), labels=number_format(big.mark='\'')) +
    scale_colour_brewer(name="Týden", type="qual", palette="PuBu") +
    NULL
    



covid_week %>% 
    select(orp_code, pop, week, week_num, new_cases, prevalence) %>% 
    group_by(orp_code) %>% 
    arrange(.by_group=T, week) %>% 
    mutate(
        q2 = new_cases / lag(prevalence, n=2L),
    ) %>% 
    group_by(week, week_num) %>% 
    summarise(
        q_25 = quantile(q2, 0.25, na.rm=T),
        q_50 = quantile(q2, 0.50, na.rm=T),
        q_75 = quantile(q2, 0.75, na.rm=T),
        .groups = 'drop'
    ) %>% 
    filter(week_num > -12L) %>% 
    ggplot(aes(x=week)) +
    geom_ribbon(aes(ymin=q_25, ymax=q_75), fill='skyblue', alpha=0.25) +
    geom_line(aes(y=q_50), colour="blue", size=0.5) +
    geom_point(aes(y=q_50), colour="blue", size=1.5) +
    expand_limits(y=0) +
    scale_x_date(name=NULL, date_breaks='2 weeks', date_minor_breaks='1 week', date_labels='%e.%b.') +
    scale_y_continuous(name=NULL, labels=percent_format()) +
    standard_label('Poměr nových případů oproti celkovému počtu nakažených před 2 týdny') +
    custom_theme + 
    NULL
    

covid_region_day <- covid_full_info %>% 
    left_join(master_orp, by=c('ruian_orp_id'='orp_id'), suffix=c('', '_orp')) %>% 
    group_by(region_abbr, date) %>% 
    arrange(.by_group=T) %>% 
    summarise(
        q_active_orp = mean(prev > 0L),
        population = sum(pop),
        prevalence = sum(prev),
        wavg_prev = sum(as.numeric(prev) * places) / sum(places),
        hospitalised = sum(hospit),
        .groups = 'drop_last'
    ) %>% 
    mutate(
        day = row_number(date),
        healthy = population - prevalence,
        q_healthy = healthy / population,
    ) %>% 
    ungroup()

fit_prevalence <- function(data) {
    model <- gam(log10(prevalence) ~ date + intervention_effect + s(dom, bs='cr') + te(dow,decay_factor), data=data)
    return (model)
}

fit_hospitalised <- function(data) {
    model <- gam(hospitalised ~ date + prevalence + intervention_effect + s(dom) + te(dow) + 1, data=data)
    return (model)
}

predict_test <- function(preval_model, hospit_model, date_from, date_to) {
    dat <- tibble(
        date = seq(date_from, date_to, by='day')
        ) %>% 
        mutate(
            day = row_number(),
            dom = mday(date),
            dow = wday(date, week_start=7L),
        ) %>% 
        left_join(gov_interventions_timeline, by='date')
    x <- predict(preval_model, newdata=dat)
    dat$prevalence = 10^x
    y <- predict(hospit_model, newdata=dat)
    dat$hospitalised = y #10^y
    return (dat)
}

all_region_abbr <- covid_full_info %>% distinct(region_abbr) %>% pull(region_abbr)
    
covid_region_model <- covid_region_day %>% 
    filter(
        date >= ymd('2020-09-05'),
        date <= ymd('2020-10-21'),
    ) %>% 
    left_join(gov_interventions_timeline, by='date') %>% 
    group_by(region_abbr) %>% 
    arrange(.by_group=T, date) %>% 
    mutate(
        dom = mday(date),
        dow = wday(date, week_start=7L),
    ) %>% 
    nest() %>%
    mutate(
        preval_model = map(data, fit_prevalence),
        hospit_model = map(data, fit_hospitalised),
        data = NULL,
    ) %>% 
    ungroup()

#saveRDS(covid_region_model, file='forecast_params_20201021.rds')
#covid_region_model <- readRDS('forecast_params_20201021.rds')

region_forecast <- tibble(region_abbr = all_region_abbr) %>% 
    left_join(covid_region_model, by='region_abbr') %>% 
    mutate(
        forecast = map2(preval_model, hospit_model, predict_test, date_from=ymd('2020-10-12'), date_to=ymd('2020-11-08') ),
        preval_model = NULL,
        hospit_model = NULL,
    ) %>% 
    unnest(forecast) %>% 
    mutate(
        fcast_prevalence = c(prevalence),
        fcast_hospitalised = c(hospitalised),
        prevalence = NULL,
        hospitalised = NULL,
    ) %>% 
    left_join(covid_region_day %>% filter(date < today()), by=c('region_abbr', 'date')) %>% 
    mutate(
        day = day.x,
        day.x = NULL,
        day.y = NULL,
        region_abbr = fct_reorder2(region_abbr, date, fcast_hospitalised),
    )

#saveRDS(region_forecast, 'region_forecast_20201021.rds')


(g01 <- region_forecast %>% 
    ggplot(aes(x=date, y=fcast_prevalence, colour=region_abbr)) +
    geom_line(size=0.2) +
    geom_segment(aes(xend=date, yend=prevalence), size=0.2, na.rm=T) +
    geom_point(aes(y=prevalence), na.rm=T) +
    geom_dl(aes(label=region_abbr), method=list('last.qp', cex=8/12)) +
    expand_limits(x=ymd('2020-10-31'), y=0) +
    scale_x_date(name=NULL, date_breaks="day", date_labels='%e.%b.', minor_breaks=NULL) +
    scale_y_continuous(name=NULL, breaks=extended_breaks(n=10L), minor_breaks=NULL, labels=number_format()) + 
    scale_colour_hue(guide='none') +
    theme(axis.text.x=element_text(size=7)) +
    labs(title="Počet diagnostikovaných podle krajů - krátkodobá předpověď") +
    NULL)

(g02 <- region_forecast %>% 
    ggplot(aes(x=date, y=fcast_hospitalised, colour=region_abbr)) +
    geom_line(size=0.2) +
    geom_segment(aes(xend=date, yend=hospitalised), size=0.2, na.rm=T) +
    geom_point(aes(y=hospitalised), na.rm=T) +
    geom_dl(aes(label=region_abbr), method=list('last.qp', cex=8/12)) +
    expand_limits(x=ymd('2020-10-31'), y=0) +
    scale_x_date(name=NULL, date_breaks="day", date_labels='%e.%b.', minor_breaks=NULL) +
    scale_y_continuous(name=NULL, breaks=extended_breaks(n=10L), minor_breaks=NULL, labels=number_format()) + 
    scale_colour_hue(guide='none') +
    theme(axis.text.x=element_text(size=7)) +
    labs(title="Počet hospitalizovaných podle krajů - krátkodobá předpověď") +
    NULL)

(g03 <- region_forecast %>% 
    group_by(date) %>% 
    summarise(across(fcast_prevalence:hospitalised, sum), .groups='drop') %>% 
    ggplot(aes(x=date)) +
    geom_line(aes(y=fcast_prevalence), size=0.2) +
    geom_segment(aes(xend=date, y=fcast_prevalence, yend=prevalence), size=0.2, na.rm=T) +
    geom_point(aes(y=prevalence), na.rm=T) +
    expand_limits(y=0) +
    scale_x_date(name=NULL, date_breaks="day", date_labels='%e.%b.', minor_breaks=NULL) +
    scale_y_continuous(name=NULL, breaks=extended_breaks(n=10L), minor_breaks=NULL, labels=number_format()) + 
    theme(axis.text.x=element_text(size=7)) +
    labs(title="Celkový počet diagnostikovaných - krátkodobá předpověď") +
    NULL)

(g04 <- region_forecast %>% 
    group_by(date) %>% 
    summarise(across(fcast_prevalence:hospitalised, sum), .groups='drop') %>% 
    ggplot(aes(x=date)) +
    geom_line(aes(y=fcast_hospitalised), size=0.2) +
    geom_segment(aes(xend=date, y=fcast_hospitalised, yend=hospitalised), size=0.2, na.rm=T) +
    geom_point(aes(y=hospitalised), na.rm=T) +
    expand_limits(y=0) +
    scale_x_date(name=NULL, date_breaks="day", date_labels='%e.%b.', minor_breaks=NULL) +
    scale_y_continuous(name=NULL, breaks=extended_breaks(n=10L), minor_breaks=NULL, labels=number_format()) + 
    theme(axis.text.x=element_text(size=7)) +
    labs(title="Celkový počet hospitalizovaných - krátkodobá předpověď") +
    NULL)


(g03/g01) | (g04/g02)



covid_full_info %>% 
    filter(date >= ymd('2020-08-10')) %>% 
    group_by(date) %>% 
    summarise(
        hospit = sum(hospit),
        .groups = 'drop'
    ) %>% 
    ggplot(aes(x=date, y=hospit)) +
    geom_point() +
    geom_smooth(method='glm', formula=y~x, se=F) +
    expand_limits(y=100) +
    scale_y_continuous(trans=log10_trans())
    
    
################################################################################

region_totals <- covid_orp %>% 
    left_join(orp_info, by='orp_code') %>% 
    group_by(region_code, region, date) %>% 
    arrange(.by_group=T) %>% 
    mutate(
        prev_delta_week = prev - lag(prev, n=7L, default=0L),
        hospit_delta_week = hospit - lag(hospit, n=7L, default=0L),
    ) %>% 
    summarise(
        inc_week = sum(inc_week),
        prev = sum(prev),
        prev_delta_week = sum(prev_delta_week),
        hospit = sum(hospit),
        hospit_delta_week = sum(hospit_delta_week),
        .groups = 'drop',
    )

last_date <- region_totals %>% pull(date) %>% max()

region_totals %>% 
    filter(date %in% seq(last_date - weeks(10), last_date, by='weeks')) %>% 
    ggplot(aes(x=date, y=hospit/prev, colour=region, group=region)) +
    geom_line(show.legend=F) +
    geom_dl(aes(label=region), method=dl.combine('first.qp', 'last.qp')) +
#    scale_y_continuous(trans=log10_trans()) +
    expand_limits(x=c(ymd('2020-07-01'), ymd('2020-11-30'))) +
    scale_y_continuous(labels=percent_format())




covid_orp %>% 
    filter(hospit > 0L, date >= ymd('2020-08-24')) %>% 
    mutate(
        week = floor_date(date, unit="week", week_start=1L)
    ) %>% 
    group_by(week, orp_code, orp_name) %>% 
    arrange(.by_group=T) %>% 
    summarise(
        prev = max(prev),
        hospit = max(hospit),
        tests = max(tests),
        .groups = 'drop'
    ) %>% 
    group_by(week) %>% 
    arrange(.by_group=T, desc(prev), desc(hospit)) %>% 
    filter(row_number() <= 20L) %>% 
    mutate(score = percent_rank(prev)) %>% 
    group_by(orp_code, orp_name) %>% 
    mutate(
        total_score = sum(score),
    ) %>% 
    ungroup() %>% 
    filter(total_score >= median(total_score)) %>% 
    view()
    
    
    
    view()

    
covid_orp %>% filter(orp_name=='Jindřichův Hradec', prev>0L) %>% view()


x <- lm(log10(hospit+1L) ~ log10(prev)*inc_week, data=covid_orp %>% filter(orp_name=='Jindřichův Hradec', prev>0L))
glimpse(x)
plot(x)

covid_orp %>%
    filter(orp_name=='Jindřichův Hradec', prev>0L) %>% 
    ggplot(aes(x=prev, y=hospit)) +
    geom_point() +
    stat_smooth(method='lm')


(ymd('2020-08-24') %--% ymd('2020-10-18')) / dweeks()




orp_trend <- covid_orp %>% 
    group_by(orp_code) %>% 
    arrange(.by_group=T, date) %>% 
    mutate(
        week = floor_date(date, unit="week", week_start=1L),
        prev_diff = prev - lag(prev, default=0L),
        hospit_diff = hospit - lag(hospit, default=0L),
    ) %>% 
    group_by(.add=T, week) %>% 
    arrange(.by_group=T, date) %>% 
    summarise(
        detected = sum(prev_diff),
        hospit_change = sum(hospit_diff),
        prev = last(prev),
        hospit = last(hospit),
        .groups = 'drop_last'
    ) %>%
    mutate(
        detected_q = clamp_value(detected / lag(detected, n=2L)),
        hospit_q = clamp_value(hospit_change / lag(detected, n=2L)),
    ) %>% 
    filter(week >= ymd('2020-08-10')) %>% 
    mutate(
        week_num = 1L - row_number(desc(week)),
    ) %>%
    ungroup() %>%
    left_join(orp, by='orp_code') %>% 
    group_by(region, week, week_num) %>% 
    arrange(.by_group=T, orp_code) %>% 
    mutate(
        reference = (row_number() == 1L),
        detected_q_25 = quantile(detected_q, 0.25, na.rm=T),
        detected_q_50 = quantile(detected_q, 0.50, na.rm=T),
        detected_q_75 = quantile(detected_q, 0.75, na.rm=T),
        hospit_q_25 = quantile(hospit_q, 0.25, na.rm=T),
        hospit_q_50 = quantile(hospit_q, 0.50, na.rm=T),
        hospit_q_75 = quantile(hospit_q, 0.75, na.rm=T),
    )

region_trend %>% 
    group_by(region_code, region, region_abbr, week, week_num) %>% 
    summarise(
        detected = sum(detected),
        hospit_change = sum(hospit_change),
        .groups = 'drop'
    )

orp_trend %>% 
    ggplot(aes(x=week)) +
    geom_ribbon(data=~filter(.x, reference), mapping=aes(ymin=detected_q_25, ymax=detected_q_75), fill='blue', alpha=0.2) +
    geom_line(aes(y=detected_q, group=name), size=0.25, alpha=0.6, colour='darkgray') +
    geom_line(data=~filter(.x, reference), mapping=aes(y=detected_q_50), size=0.5, colour='blue') +
    geom_hline(yintercept=0, colour='darkgreen', linetype=5, size=0.3, alpha=0.6) +
    geom_hline(yintercept=1, colour='darkred', linetype=5, size=0.3, alpha=0.6) +
    facet_wrap(vars(region), nrow=3L) +
    coord_cartesian(ylim=c(-5,15)) +
    scale_x_date(name=NULL, date_breaks="week", minor_breaks=NULL, date_labels="%e.%b.") +
    scale_y_continuous(name=NULL, minor_breaks=NULL) +
    labs(title="Poměr detekovaných případů v týdnu W oproti W-2") +
    theme_bw() +
    theme(axis.text.x=element_text(size=5)) +
    NULL

orp_trend %>% 
    ggplot(aes(x=week)) +
    geom_ribbon(data=~filter(.x, reference), mapping=aes(ymin=hospit_q_25, ymax=hospit_q_75), fill='orchid', alpha=0.2) +
    geom_line(aes(y=hospit_q, group=name), size=0.25, alpha=0.6, colour='darkgray') +
    geom_line(data=~filter(.x, reference), mapping=aes(y=hospit_q_50), size=0.5, colour='red') +
    facet_wrap(vars(region), nrow=3L) +
    coord_cartesian(ylim=c(0, 0.5)) +
    scale_x_date(name=NULL, date_breaks="week", minor_breaks=NULL, date_labels="%e.%b.") +
    scale_y_continuous(name=NULL, minor_breaks=NULL) +
    theme_bw() +
    labs(title="Poměr hospitalizovaných oproti případům detekovaných před 2 týdny") +
    NULL

    

covid_region %>% 
    filter(week_num >= -10L) %>% 
    filter(region_abbr != 'Prg') %>% 
    ggplot(aes(x=week)) +
    geom_ribbon(aes(ymin=new_cases_25, ymax=new_cases_75), fill='skyblue') +
    geom_line(aes(y=new_cases_50), colour='blue') +
    facet_wrap(vars(region), nrow=3L) +
    coord_cartesian(ylim=c(-10, 250)) +
    NULL

covid_region %>% 
    group_by(region) %>% 
    arrange(.by_group=T, week) %>% 
    mutate(
        q_new_cases = clamp_value(prevalence / lag(prevalence), clamp=9),
    ) %>% 
    ungroup() %>% 
    filter(
        week_num >= -8L,
#        region_abbr != 'Prg'
    ) %>% 
    ggplot(aes(x=week, y=q_new_cases, group=region_abbr, colour=region_abbr)) +
    geom_line() +
    scale_y_continuous() +
    coord_cartesian(ylim=c(0, 3)) +
    NULL




region_population_by_age %>% 
    arrange(region_code, age_from) %>% 
    ggplot(aes(x=age_from, y=count, group=sex, colour=sex)) +
    geom_line() +
    facet_wrap(vars(region)) +
    NULL

orp_population_by_age %>% 
    inner_join(orp_info, by='orp_code') %>% 
    group_by(region, region_abbr, orp_code, age_from) %>% 
    summarise(count = sum(count), .groups = 'drop') %>% 
    ggplot(aes(x=age_from, y=count, group=orp_code)) +
    facet_wrap(vars(region), scales="free_y") +
    geom_line(alpha=0.3) +
    scale_y_continuous()
    


patients_by_age_group <- detected_patients %>% 
    filter(age<=120L) %>% 
    mutate(
        week = floor_date(date, unit='week', week_start=1L),
        age_group = cut(age, breaks=c(0L, 6L, 15L, 25L, 40L, 60L, 80L, Inf), labels=c('Předškoláci (0-5)', 'Děti (6-14)', 'Mladí (15-24)', 'Dospělí (25-39)', 'Střední věk (40-59)', 'Staří (60-79)', 'Nejstarší (80+)'), right=F),
    ) %>% 
    group_by(region_code, age_group, week) %>% 
    arrange(.by_group=T) %>% 
    summarise(
        cases = n(),
        .groups = 'drop'
    ) %>% 
    inner_join(region_population %>% select(region_code, region, region_abbr), by='region_code')


(g05 <- patients_by_age_group %>% 
    filter(week < ymd('2020-10-18')) %>% 
    filter(week >= floor_date(today(), 'week', week_start=1L) - weeks(4L)) %>% 
    ggplot(aes(y=age_group, x=week, size=cases, colour=cases)) +
    geom_point() +
    facet_wrap(vars(region), nrow=4L) +
    scale_x_date(name=NULL, date_breaks='week', minor_breaks=NULL, date_labels='%e.%b.\n%V. týden', guide=guide_axis(check.overlap=T, n.dodge=1L)) +
    scale_y_discrete(name=NULL) +
    scale_size(range=c(0,5), breaks=c(0,1,2,4,8,16), guide='none') +
    scale_colour_distiller(name='Počet nových případů', palette='Spectral', labels=number_format(), guide=guide_coloursteps(direction='horizontal', title.position='top', title.hjust=0, title.vjust=0)) +
    labs(title="Vývoj absolutního počtu diagnostikovaných případů") +
    theme_dark() +
    theme(
        legend.position=c(1,0),
        legend.justification=c(1,0),
        axis.ticks=element_blank(),
        axis.text=element_text(size=8)
    ) +
    NULL)

(g06 <- patients_by_age_group %>% 
    filter(week < ymd('2020-10-18')) %>% 
    filter(week >= floor_date(today(), 'week', week_start=1L) - weeks(5L)) %>% 
    group_by(region, age_group) %>% 
    mutate(
        prev_cases = lag(cases),
        change = cases - prev_cases,
    ) %>% 
    ungroup() %>% 
    filter(!is.na(change)) %>% 
    ggplot(aes(y=age_group, x=week, size=change, colour=change)) +
    geom_point() +
    facet_wrap(vars(region), nrow=4L) +
    scale_x_date(name=NULL, date_breaks='week', minor_breaks=NULL, date_labels='%e.%b.\n%V. týden', guide=guide_axis(check.overlap=T, n.dodge=1L)) +
    scale_y_discrete(name=NULL) +
    scale_size(range=c(0,5), breaks=c(0,1,2,4,8,16), guide='none') +
    scale_colour_distiller(name='Změna počtu nových případů', palette='Spectral', guide=guide_coloursteps(direction='horizontal', title.position='top', title.hjust=0, title.vjust=0)) +
    labs(title="Vývoj mezitýdenního počtu diagnostikovaných případů") +
    theme_dark() +
        theme(
            legend.position=c(1,0),
            legend.justification=c(1,0),
            axis.ticks=element_blank(),
            axis.text=element_text(size=8)
        ) +
    NULL)

(g07 <- patients_by_age_group %>% 
        filter(week >= floor_date(today(), 'week', week_start=1L) - weeks(5L)) %>% 
        group_by(region, age_group) %>% 
        mutate(
            prev_cases = lag(cases),
            change = cases / prev_cases,
        ) %>% 
        ungroup() %>% 
        filter(!is.na(change)) %>% 
        ggplot(aes(y=age_group, x=week, size=change, colour=change)) +
        geom_point() +
        facet_wrap(vars(region), nrow=4L) +
        scale_x_date(name=NULL, date_breaks='week', minor_breaks=NULL, date_labels='%e.%b.\n%V. týden', guide=guide_axis(check.overlap=T, n.dodge=1L)) +
        scale_y_discrete(name=NULL) +
        scale_size(range=c(0,5), breaks=c(0,1,2,4,8,16)) + #, guide='none') +
        scale_colour_distiller(name='Poměr počtu nových případů', palette='Spectral', guide=guide_coloursteps(
        #scale_colour_distiller(name='x', palette='Spectral', guide=guide_coloursteps(
            direction='horizontal',
            title='Pomer poctu novych pripadu',
            title.position='top',
            title.hjust=0, title.vjust=0,
            #barwidth=unit(90, 'points'),
            
            )) +
        labs(title="Vývoj mezitýdenního poměru počtu diagnostikovaných případů") +
        theme_dark() +
        theme(
            legend.position=c(1,0),
            legend.justification=c(1,0),
            axis.ticks=element_blank(),
            axis.text=element_text(size=8),
            legend.box.background=element_rect(fill='blue', colour='black', size=1),
            legend.direction='vertical',
            legend.box.just='right',
            legend.background=element_rect(fill='yellow')
        ) +
        NULL)




aligned_plots <- align_patches(g01, g02, g05, g06, g07)
cairo_pdf('covid_trends.pdf', width=297/25.4, height=210/25.4, onefile=T)
g03/g04
aligned_plots[[1]]
aligned_plots[[2]]
aligned_plots[[3]]
aligned_plots[[4]]
aligned_plots[[5]]
dev.off()    
rm(aligned_plots)


g05/g06


region_forecast %>% 
    transmute(
        region_abbr,
        date,
        expected_prevalence = as.integer(fcast_prevalence),
        expected_hospitalised = as.integer(fcast_hospitalised),
    ) %>% 
    filter(date >= today()) %>% 
    arrange(date, region_abbr) %>% 
    format_csv() %>% 
    clipr::write_clip()


    

xjh <- c(1L, 1L, 0L, 2L, 2L, 2L, 1L, 4L, 9L, 4L, 2L, 18L, 29L, 37L, 99L, 175L, 418L)

xff <- function(x, pop=1, .c=1) {
    y <- 2^(as.numeric(x)-17) * as.numeric(pop)
    return (y)
}

tibble(
    x = seq_along(xjh),
    jh = log10(as.numeric(xjh) / 47113),
    ref = round(xff(0:(length(xjh)-1), pop=1, .c=1))
) %>% 
ggplot(aes(x=x)) +
    geom_line(aes(y=jh, colour='JH')) +
    geom_line(aes(y=ref, colour='Ref'))



covid_week %>% 
    filter(
        ruian_orp_id == 345L,
        week >= ymd('2020-06-29')
    ) %>% view()
    select(week, new_cases, prevalence, hospitalised)
    
