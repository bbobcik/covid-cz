library(slider)


place_cases <- read_delim(
    file='obec_20201024.csv',
    delim=';',
    col_types=cols_only(
        date = col_date('%Y-%m-%d'),
        region_id = col_character(),
        district_id = col_character(),
        orp_code = col_integer(),
        opou_code = col_integer(),
        opou_name = col_character(),
        place_code = col_integer(),
        place_name = col_character(),
        new_cases = col_integer(),
        act_cases = col_integer()
    ),
    col_names=c(
        'day', 'date', 'region_id', 'region_name', 'district_id', 'district_name',
        'orp_code', 'orp_name', 'opou_code', 'opou_name', 'place_code', 'place_name',
        'new_cases', 'act_cases'
    ),
    skip=1L
)

place_list <- place_cases %>%
    select(region_id, district_id, orp_code, opou_code, opou_name, place_code, place_name) %>% 
    distinct() %>% 
    arrange(opou_name, place_name)
    

interesting_places <- place_cases %>% 
    filter(place_code %in% c(
        546798L, # NB
        546101L, # Cimer
        547212L, # Stare M.p.L
#        541150L, # Kammeny Ujezd
#        592579L, # Slavkov
#        554804L, # UnL
        -1L
    ))



(g19 <- interesting_places %>%
    filter(
        date >= ymd('2020-09-14'),
        date < today(),
    ) %>% 
    group_by(place_name) %>% 
    arrange(.by_group=T, date) %>% 
    mutate(
        act_cases = case_when(
            date == max(date)                        ~ act_cases,
            act_cases == lag(act_cases, default=-1L) ~ NA_integer_,
            TRUE                                     ~ act_cases,
        )
    ) %>% 
    filter(!is.na(act_cases)) %>% 
    ggplot(aes(x=date, y=act_cases, group=place_name, colour=place_name)) +
    geom_step(size=2, alpha=0.5, show.legend=F) +
    geom_point(size=4, alpha=0.75, show.legend=F) +
    geom_dl(aes(label=place_name), method=list(cex=0.8, dl.trans(x=x+0.25), 'maxvar.qp')) +
    scale_x_date(name=NULL, date_breaks='1 week', date_minor_breaks='1 day', date_labels='%e.%b.', expand=expansion(0, c(2,10))) +
    scale_y_continuous(name=NULL, breaks=extended_breaks(Q=c(2,5,1,3,4), w=c(0.2, 0.4, 0.15, 0.25)), minor_breaks=NULL, labels=number_format(accuracy=1)) +
    standard_label('Počet aktivních případů Covid-19 ve vybraných obcích') +
    custom_theme +        
    theme(
        panel.grid.major.x=element_line(color='#B0B0B0'),
        plot.title=element_text(size=12),
    ) +
    NULL)

interest_plot_date = interesting_places %>% pull(date) %>% max() %>% strftime('%Y-%m-%d')
png(paste0('outputs/', interest_plot_date, '/covid_focus_', interest_plot_date, '.png'), width=2000, height=1200, res=120, type='cairo')
g19
dev.off()
png('outputs/covid_focus_latest.png', width=1500, height=800, res=90, type='cairo')
g19
dev.off()


place_cases %>% 
    filter(
        orp_code == 3105L, # JH
#        place_code != 545881L, # JH
        date >= ymd('2020-09-14'),
    ) %>% 
    group_by(place_name) %>% 
    arrange(.by_group=T, date) %>% 
    mutate(
        act_cases = case_when(
            date == max(date)                        ~ act_cases,
            act_cases == lag(act_cases, default=-1L) ~ NA_integer_,
            TRUE                                     ~ act_cases,
        ),
        last_cases = last(act_cases),
    ) %>% 
    ungroup() %>% 
    filter(last_cases > 0L) %>% 
    mutate(
        bucket = cut(x=last_cases, breaks=c(-Inf,0,1,5,10,20,50,100,200,500,1000,2000,5000,10000,20000,50000,Inf)),
    ) %>% 
    filter(!is.na(act_cases)) %>% 
    ggplot(aes(x=date, y=act_cases, group=place_name, colour=place_name)) +
    geom_step(size=0.3, alpha=0.5, show.legend=F) +
    geom_point(size=1, alpha=0.75, show.legend=F) +
    geom_dl(aes(label=place_name), method=list(cex=0.8, dl.trans(x=x+0.25), 'last.bumpup')) +
    scale_x_date(name=NULL, date_breaks='1 week', date_minor_breaks='1 day', date_labels='%e.%b.', expand=expansion(0, c(2,10))) +
    scale_y_continuous(name=NULL, breaks=extended_breaks(Q=c(2,5,1,3,4), w=c(0.2, 0.4, 0.15, 0.25)), minor_breaks=NULL, labels=number_format(accuracy=1)) +
    facet_wrap(vars(bucket), scales='free_y') +
    labs(title='Počet aktivních případů Covid-19', caption='Zdroj dat: https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19') +
    theme_bw() +
    theme(
        panel.grid.major.x=element_line(color='#B0B0B0'),
        plot.title=element_text(size=12),
        plot.caption=element_text(size=8, colour='#404040'),
        plot.margin=unit(c(5,5,5,5), 'mm')
    ) +
    NULL

place_list %>% 
    filter(district_id != 'CZ099Y') %>% 
    transmute(
        #place_name,
        region_code = as.integer(str_sub(region_id, 3L)),
        district_code = strtoi(str_sub(district_id, 3L), base=16L),
        orp_code,
        opou_code,
        place_code
    )



place_case_changes <- place_cases %>% 
    select(place_code, date, act_cases, new_cases) %>% 
    group_by(place_code) %>% 
    arrange(.by_group=T, date) %>% 
    mutate(
        prev_cases = lag(act_cases, default=0L),
        dif_cases = act_cases - prev_cases,
        dif_cases = case_when(
            date == min(date) ~ dif_cases,
            date == max(date) ~ dif_cases,
            dif_cases != 0L   ~ dif_cases,
            TRUE              ~ NA_integer_,
        ),
        q_cases = case_when(
            is.na(dif_cases)  ~ NA_real_,
            0L == prev_cases  ~ 1,
            TRUE              ~ dif_cases / prev_cases,
        ),
    ) %>% 
    ungroup() %>% 
    filter(!is.na(dif_cases))


weighted_ratio <- function(x,y) {
    if (y == 0) {
        return(sign(x))
    }
    q = log10(abs(y)+1) * x / y
    return(q)
}


covid_dynamic <- place_case_changes %>% 
    inner_join(master_place, by=c(place_code='place_id')) %>% 
    group_by(date, region_abbr, district_name, orp_name) %>% 
    summarise(
        orp_act = sum(act_cases),
        orp_prev = sum(prev_cases, na.rm=T),
        orp_new = sum(new_cases),
        orp_dif = sum(dif_cases),
        .groups = 'drop_last'
    ) %>% 
    mutate(
        orp_q = weighted_ratio(orp_dif, orp_prev),
        district_act = sum(orp_act),
        district_prev = sum(orp_prev),
        district_new = sum(orp_new),
        district_dif = sum(orp_dif),
        district_q = weighted_ratio(district_dif, district_prev),
    ) %>% 
    ungroup(district_name) %>% 
    mutate(
        region_act = sum(orp_act),
        region_prev = sum(orp_prev),
        region_new = sum(orp_new),
        region_dif = sum(orp_dif),
        region_q = weighted_ratio(region_dif, region_prev),
    ) %>% 
    ungroup(region_abbr) %>% 
    mutate(
        total_act = sum(orp_act),
        total_prev = sum(orp_prev),
        total_new = sum(orp_new),
        total_dif = sum(orp_dif),
        total_q = weighted_ratio(total_dif, total_prev),
    ) %>% 
    ungroup()
    
covid_dynamic_window <- covid_dynamic %>%     
    filter(date >= ymd('2020-09-21'))


covid_dyn_total <- covid_dynamic_window %>% 
    group_by(date) %>% 
    filter(row_number() == 1L) %>% 
    ungroup() %>% 
    transmute(
        date,
        scope = 'total',
        spec = 'Total',
        region = NA_character_,
        prio = 9L,
        q = total_q,
        dif = total_dif
    )

covid_dyn_region <- covid_dynamic_window %>% 
    group_by(date, region_abbr) %>% 
    filter(row_number() == 1L) %>% 
    ungroup() %>% 
    transmute(
        date,
        scope = 'region',
        spec = region_abbr,
        region = region_abbr,
        prio = 8L,
        q = region_q,
        dif = region_dif,
    )

covid_dyn_district <- covid_dynamic_window %>% 
    group_by(date, region_abbr, district_name) %>% 
    filter(row_number() == 1L) %>% 
    ungroup() %>% 
    transmute(
        date,
        scope = 'district',
        spec = district_name,
        region = region_abbr,
        prio = 7L,
        q = district_q,
        dif = district_dif,
    )

covid_dyn_orp <- covid_dynamic_window %>% 
    group_by(date, orp_name, region_abbr) %>% 
    filter(row_number() == 1L) %>% 
    ungroup() %>% 
    transmute(
        date,
        scope = 'orp',
        spec = orp_name,
        region = region_abbr,
        prio = 6L,
        q = orp_q,
        dif = orp_dif,
    )


covid_dyn_region %>% 
    bind_rows(covid_dyn_total) %>% 
    mutate(xspec = paste0(prio, spec)) %>% 
    ggplot(aes(x=date, y=q, group=xspec, colour=scope, size=scope)) +
    geom_line() +
    geom_hline(yintercept=0, colour='darkred', size=0.5, alpha=0.5) +
    scale_x_date(name=NULL, date_breaks='week', date_minor_breaks='day', date_labels='%e.%b.') +
    scale_y_continuous(name=NULL, breaks=extended_breaks(10)) +
    scale_colour_manual(name=NULL, values=c('total'='black', 'region'='gray'), guide=guide_none()) +
    scale_size_manual(name=NULL, values=c('total'=2, 'region'=0.2), guide=guide_none()) +
    theme_bw() +
    theme(panel.grid.major.x=element_line(color='#B0B0B0')) +
    NULL

covid_dyn_district %>% 
    bind_rows(covid_dyn_region) %>% 
    mutate(xspec = paste0(prio, spec)) %>% 
    ggplot(aes(x=date, y=q, group=xspec, colour=scope, size=scope)) +
    geom_line() +
    geom_hline(yintercept=0, colour='darkred', size=0.5, alpha=0.5) +
    facet_wrap(vars(region)) +
    scale_x_date(name=NULL, date_breaks='week', date_minor_breaks='day', date_labels='%e.%b.') +
    scale_y_continuous(name=NULL, breaks=extended_breaks(10)) +
    scale_colour_manual(name=NULL, values=c('region'='black', 'district'='gray'), guide=guide_none()) +
    scale_size_manual(name=NULL, values=c('region'=1, 'district'=0.1), guide=guide_none()) +
    theme_bw() +
    theme(panel.grid.major.x=element_line(color='#B0B0B0')) +
    NULL

covid_dyn_orp %>% 
    bind_rows(covid_dyn_region) %>% 
    mutate(xspec = paste0(prio, spec)) %>% 
    ggplot(aes(x=date, y=q, group=xspec, colour=scope, size=scope)) +
    geom_line() +
    geom_hline(yintercept=0, colour='darkred', size=0.5, alpha=0.5) +
    facet_wrap(vars(region)) +
    scale_x_date(name=NULL, date_breaks='week', date_minor_breaks='day', date_labels='%e.%b.') +
    scale_y_continuous(name=NULL, breaks=extended_breaks(10)) +
    scale_colour_manual(name=NULL, values=c('region'='black', 'orp'='gray'), guide=guide_none()) +
    scale_size_manual(name=NULL, values=c('region'=1, 'orp'=0.1), guide=guide_none()) +
    theme_bw() +
    theme(panel.grid.major.x=element_line(color='#B0B0B0')) +
    NULL




covid_growth_by_place <- place_cases %>% 
    filter(
        date >= max(date) - weeks(6L),
        new_cases > 0L,
    ) %>% 
    left_join(master_place, by=c('place_code'='place_id'), suffix=c('', '_place')) %>% 
    mutate(
        place_name = place_name_place,
        week = floor_date(date, 'week', week_start=1L),
    ) %>% 
    group_by(region_abbr, place_code, place_name, week) %>% 
    arrange(.by_group=T, date) %>% 
    summarise(
        across(c(place_x, place_y, pou_dist, region_dist, weight, act_cases), last),
        volatil = sd(new_cases),
        new_cases = sum(new_cases),
        .groups = 'keep'
    ) %>% 
    filter(new_cases > 0L) %>% 
    mutate(
        volatil = coalesce(volatil, 0) / pmax(act_cases, 1),
        score_base = log10(pou_dist + max(act_cases,0) + 10) / log10(weight + 10),
        dyn_factor = case_when(
            new_cases == 0L         ~  0,
            new_cases == 1L         ~  score_base,
            new_cases == act_cases  ~  score_base,
            TRUE                    ~  score_base * new_cases * volatil,
        ),
        dyn_factor = signif(dyn_factor, 4L),
    ) %>% 
    group_by(week) %>% 
    #filter(dyn_factor >= quantile(dyn_factor, 0.5, na.rm=T)) %>% 
    top_n(n=80L, dyn_factor) %>% 
    ungroup() %>% 
    arrange(place_code, week) %>% 
    mutate(
        place_name_single = na_if(place_name, lead(place_name, default='')),
    ) %>% 
    arrange(week, weight, desc(dyn_factor), desc(new_cases), place_code)

covid_growth_by_orp <- place_cases %>% 
    filter(
        date >= max(date) - weeks(6L),
    ) %>% 
    left_join(master_place, by=c('place_code'='place_id'), suffix=c('', '_place')) %>% 
    left_join(master_orp, by='orp_id', suffix=c('', '_orp')) %>% 
    mutate(
        week = floor_date(date, 'week', week_start=1L),
    ) %>% 
    group_by(region_abbr, orp_id, orp_name, week, place_code) %>% 
    arrange(.by_group=T, date) %>% 
    mutate(
        first_act_cases = case_when(
            row_number() == 1L  ~ first(act_cases-new_cases),
            TRUE                ~ NA_integer_,
        ),
    ) %>% 
    group_by(region_abbr, orp_id, orp_name, week) %>% 
    summarise(
        across(c(orp_x, orp_y, region_dist, avg_weight), last),
        status = max(status),
        act_cases = sum(first_act_cases, na.rm=T),
        volatil = sd(new_cases),
        new_cases = sum(new_cases),
        .groups = 'keep'
    ) %>% 
    mutate(
        volatil = coalesce(volatil, 0) / pmax(act_cases, 1),
        score_base = log10(region_dist + max(act_cases,0) + 10) / log10(avg_weight + 10),
        dyn_factor = case_when(
            new_cases == 0L         ~  0,
            new_cases == act_cases  ~  1,
            TRUE                    ~  score_base * new_cases * volatil,
        ),
        dyn_factor = signif(dyn_factor, 4L),
    ) %>% 
    group_by(week) %>% 
    #filter(dyn_factor >= quantile(dyn_factor, 0.5, na.rm=T)) %>% 
    top_n(n=80L, dyn_factor) %>% 
    ungroup() %>% 
    arrange(orp_id, week) %>% 
    mutate(
        orp_name_single = na_if(orp_name, lead(orp_name, default='')),
    ) %>% 
    arrange(week, status, desc(dyn_factor), desc(new_cases), orp_id)

(g21 <- covid_growth_by_orp %>% 
    mutate(
        orp_short = str_trunc(orp_name_single, 17L, ellipsis=''),
        disc_week = factor(strftime(week, '%e.%b.')),
        disc_week = fct_inorder(disc_week),
    ) %>% 
    ggplot(aes(x=orp_x, y=orp_y)) +
    geom_jitter(aes(colour=disc_week, size=dyn_factor), alpha=0.7, width=3000, height=3000) +
    geom_text(aes(label=orp_short), na.rm=T, size=4, alpha=0.6) +
    scale_size_continuous(name='Dynamika', range=c(0, 12), guide=guide_legend(order=1L)) +
    scale_colour_brewer(name='Týden', type='seq', palette='PuRd', guide=guide_legend(order=2L)) +
    labs(title='Dynamika nákazy podle obvodů obcí s rozšířenou působností', caption='Zdroj dat: https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19') +
    theme_minimal() +
    theme(
        panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.line.x = NULL,
        axis.line.y = NULL,
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        legend.position=c(1,1),
        legend.justification=c(1,1),
        legend.direction='horizontal'
    ) +
    NULL)

(g22 <- covid_growth_by_place %>% 
    mutate(
        name_short = str_trunc(place_name_single, 17L, ellipsis=''),
        disc_week = factor(strftime(week, '%e.%b.')),
        disc_week = fct_inorder(disc_week),
    ) %>% 
    ggplot(aes(x=place_x, y=place_y)) +
    geom_jitter(aes(colour=disc_week, size=dyn_factor), alpha=0.7, width=3000, height=3000) +
    geom_text(aes(label=name_short), na.rm=T, size=3, alpha=0.6) +
    scale_size_continuous(name='Dynamika', range=c(0, 12), guide=guide_legend(order=1L)) +
    scale_colour_brewer(name='Týden', type='seq', palette='PuBu', guide=guide_legend(order=2L)) +
    labs(title='Dynamika nákazy podle obcí', caption='Zdroj dat: https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19') +
    theme_minimal() +
    theme(
        panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.line.x = NULL,
        axis.line.y = NULL,
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        legend.position=c(1,1),
        legend.justification=c(1,1),
        legend.direction='horizontal'
    ) +
    NULL)




file_date <- place_cases %>% pull(date) %>% max() %>% strftime('%Y-%m-%d')
g2x_aligned <- align_patches(g21, g22)
png(paste0('covid_dynamic_orp-', file_date, '.png'), width=2000L, height=1200L, type='cairo', res=120)
g2x_aligned[[1]]
dev.off()
png(paste0('covid_dynamic_place-', file_date, '.png'), width=2000L, height=1200L, type='cairo', res=120)
g2x_aligned[[2]]
dev.off()

    

place_cases %>% 
    filter(date >= '2020-08-03') %>% 
    group_by(date) %>%
    arrange(.by_group=T) %>% 
    summarise(
        new_cases = sum(new_cases),
    ) %>% 
    ungroup() %>% 
    mutate(
        prev_new = lag(new_cases, n=7L, default=0L),
        q_new = if_else(prev_new == 0L, 0, as.numeric(new_cases) / prev_new),
    ) %>%
    ggplot(aes(x=date, y=q_new)) +
    geom_point() +
    coord_cartesian(ylim=c(0,5))



weighted_sum <- function(x, weights=c(1,2,5,8,10), normalize=TRUE) {
    lx <- length(x)
    if (lx <= 1L) {
        return (coalesce(x, 0))
    } else if (length(weights) <= 0L || near(sum(weights), 0)) {
        return (x)
    } else if (length(weights) != length(x)) {
        weights <- approx(weights, n=length(x), na.rm=TRUE)$y
    }
    if (any(is.na(x))) {
        sub_x <- x[1:(lx-1)]
        mod_weights <- weights[1:(lx-1)]
        mod_weights[is.na(sub_x)] <- 0
        weights[1:(lx-1)] <- mod_weights
    }
    wsum <- sum(x * weights, na.rm=T)
    if (normalize==TRUE) {
        wsum <- wsum / sum(weights)
    }
    return (c(wsum))
}

spread_by_region <- place_cases %>% 
    filter(
        date >= '2020-08-03',
        place_code != 999999L,
    ) %>% 
    group_by(place_code) %>% 
    arrange(.by_group=T, date) %>%
    mutate(
        weighted_new_cases = slide_dbl(new_cases, .f=weighted_sum, .before=3L, .after=3L, weights=c(0.1, 0.25, 0.8, 1, 0.8, 0.25, 0.1), normalize=F),
        spread_factor = lead(weighted_new_cases, n=7L) / na_if(weighted_new_cases, 0L),
    ) %>%
    ungroup() %>% 
    filter(!is.na(spread_factor)) %>% 
    group_by(region_id, date) %>% 
    arrange(.by_group=T) %>% 
    summarise(
        spread_25 = quantile(spread_factor, 0.25, na.rm=T),
        spread_50 = quantile(spread_factor, 0.50, na.rm=T),
        spread_75 = quantile(spread_factor, 0.75, na.rm=T),
        .groups = 'drop'
    ) %>%
    mutate(week = floor_date(date, unit='week', week_start=1L)) %>% 
    group_by(week) %>% 
    mutate(
        week_spread_50 = median(spread_50, na.rm=T),
    ) %>% 
    ungroup() %>% 
    filter(date < today() - weeks(1)) %>%
    inner_join(region_abbr_enum, by=c('region_id'='nuts'))


spread_by_region %>% 
    ggplot(aes(x=date)) +
    geom_ribbon(aes(ymin=spread_25, ymax=spread_75), fill='skyblue', alpha=0.5) +
    geom_line(aes(y=spread_50), colour='darkblue', size=0.25) +
    geom_point(aes(y=spread_50), colour='darkblue', size=0.5) +
    geom_line(aes(y=week_spread_50), colour='black', size=0.5, linetype=2L, alpha=0.5) +
    coord_cartesian(ylim=c(0,3)) +
    facet_wrap(vars(region_abbr), ncol=5L) +
    scale_x_date(name=NULL) +
    scale_y_continuous(name=NULL, minor_breaks=NULL, labels=percent_format()) +
    standard_label('Faktor šíření nových případů (N=7 dnů)') +
    custom_theme
    
        
