library(plotly)


covid_orp_2 <- read_delim(
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


forecast_validation_region <- covid_orp_2 %>% 
    filter(
        date > ymd('2020-10-21'),
        date < today()
   ) %>% 
    left_join(orp_info, by='orp_code') %>% 
    select(region_abbr, name, date, pop, avg_age, inc_week, prev, hospit, tests,
           inc_week_65, inc_week_75, prev_65, prev_75, hospit_delta,
           pop_young, pop_old, q_young, q_old,
           region, region_code, full_name, orp_code) %>% 
    group_by(region_abbr, date) %>% 
    arrange(.by_group=T) %>% 
    summarise(
        prevalence = sum(prev),
        hospitalised = sum(hospit),
        .groups = 'drop'
    )

forecast_validation_total <- forecast_validation_region %>% 
    group_by(date) %>% 
    summarise(
        prevalence = sum(prevalence),
        hospitalised = sum(hospitalised),
        .groups = 'drop'
    )

region_forecast_2 <- readRDS('models/region_forecast_20201021.rds')


(g11 <- region_forecast_2 %>% 
        ggplot(aes(x=date, y=fcast_prevalence, colour=region_abbr)) +
        geom_line(size=0.2) +
        geom_segment(aes(xend=date, yend=prevalence), size=0.2, na.rm=T) +
        geom_point(aes(y=prevalence), na.rm=T) +
        geom_point(data=forecast_validation_region, mapping=aes(y=prevalence), shape=3, size=3) +
        geom_dl(aes(label=region_abbr), method=list('last.qp', cex=8/12)) +
        expand_limits(x=ymd('2020-10-31'), y=0) +
        scale_x_date(name=NULL, date_breaks="day", date_labels='%e.%b.', minor_breaks=NULL) +
        scale_y_continuous(name=NULL, breaks=extended_breaks(n=10L), minor_breaks=NULL, labels=number_format()) + 
        scale_colour_hue(guide='none') +
        theme(axis.text.x=element_text(size=5)) +
        labs(title="Počet diagnostikovaných podle krajů - krátkodobá předpověď") +
        NULL)

(g12 <- region_forecast_2 %>% 
        ggplot(aes(x=date, y=fcast_hospitalised, colour=region_abbr)) +
        geom_line(size=0.2) +
        geom_segment(aes(xend=date, yend=hospitalised), size=0.2, na.rm=T) +
        geom_point(aes(y=hospitalised), na.rm=T) +
        geom_point(data=forecast_validation_region, mapping=aes(y=hospitalised), shape=3, size=3) +
        geom_dl(aes(label=region_abbr), method=list('last.qp', cex=8/12)) +
        expand_limits(x=ymd('2020-10-31'), y=0) +
        scale_x_date(name=NULL, date_breaks="day", date_labels='%e.%b.', minor_breaks=NULL) +
        scale_y_continuous(name=NULL, breaks=extended_breaks(n=10L), minor_breaks=NULL, labels=number_format()) + 
        scale_colour_hue(guide='none') +
        theme(axis.text.x=element_text(size=5)) +
        labs(title="Počet hospitalizovaných podle krajů - krátkodobá předpověď") +
        NULL)

(g13 <- region_forecast_2 %>% 
        group_by(date) %>% 
        summarise(across(fcast_prevalence:hospitalised, sum), .groups='drop') %>% 
        ggplot(aes(x=date)) +
        geom_line(aes(y=fcast_prevalence), size=0.2) +
        geom_segment(aes(xend=date, y=fcast_prevalence, yend=prevalence), size=0.2, na.rm=T) +
        geom_point(aes(y=prevalence), na.rm=T) +
        geom_point(data=forecast_validation_total, mapping=aes(y=prevalence), shape=3, size=3) +
        expand_limits(y=0) +
        scale_x_date(name=NULL, date_breaks="day", date_labels='%e.%b.', minor_breaks=NULL) +
        scale_y_continuous(name=NULL, breaks=extended_breaks(n=10L), minor_breaks=NULL, labels=number_format()) + 
        theme(axis.text.x=element_text(size=5)) +
        labs(title="Celkový počet diagnostikovaných - krátkodobá předpověď") +
        NULL)

(g14 <- region_forecast_2 %>% 
        group_by(date) %>% 
        summarise(across(fcast_prevalence:hospitalised, sum), .groups='drop') %>% 
        ggplot(aes(x=date)) +
        geom_line(aes(y=fcast_hospitalised), size=0.2) +
        geom_segment(aes(xend=date, y=fcast_hospitalised, yend=hospitalised), size=0.2, na.rm=T) +
        geom_point(aes(y=hospitalised), na.rm=T) +
        geom_point(data=forecast_validation_total, mapping=aes(y=hospitalised), shape=3, size=3) +
        expand_limits(y=0) +
        scale_x_date(name=NULL, date_breaks="day", date_labels='%e.%b.', minor_breaks=NULL) +
        scale_y_continuous(name=NULL, breaks=extended_breaks(n=10L), minor_breaks=NULL, labels=number_format()) + 
        theme(axis.text.x=element_text(size=5)) +
        labs(title="Celkový počet hospitalizovaných - krátkodobá předpověď") +
        NULL)


fcast_eval_date <- strftime(today(), '%Y-%m-%d')
png(paste0('outputs/', fcast_eval_date, '/covid_forecast_eval_', fcast_eval_date, '.png'), width=2400L, height=1200L, type='cairo', res=120)
(g13/g11) | (g14/g12)
dev.off()
png('outputs/covid_forecast_eval_latest.png', width=1500L, height=800L, type='cairo', res=90)
((g13/g11) | (g14/g12)) +
    plot_annotation(
        caption='Autor: Boleslav Bobčík, https://github.com/bbobcik/covid-cz\nZdroj dat: MZČR, https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19',
        theme = theme(plot.caption=element_text(size=8, colour='darkgray')))
dev.off()



htmlwidgets::saveWidget(as_widget(ggplotly(g11)), 'region_diag.html')
htmlwidgets::saveWidget(as_widget(ggplotly(g12)), 'region_hosp.html')
htmlwidgets::saveWidget(as_widget(ggplotly(g13)), 'total_diag.html')
htmlwidgets::saveWidget(as_widget(ggplotly(g14)), 'total_hosp.html')
