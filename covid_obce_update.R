current_case_file <- list.files(pattern='obec_\\d+.csv') %>%
    sort() %>%
    tail(1L)

place_cases <- read_delim(
    file=current_case_file,
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

rm(current_case_file)



interesting_places <- place_cases %>% 
    filter(place_code %in% c(
        546798L, # NB
        546101L, # Cimer
        547212L, # Stare M.p.L
        #        546542L, # Kardašova Řečice
        -1L
    ))

interesting_places2 <- place_cases %>% 
    filter(place_code %in% c(
        592579L, # Slavkov
        592188L, # HorNěmčí
        592145L, # DolNěmčí
        592439L, # Nivnice
        592056L, # Boršice
        592170L, # Hluk
        592617L, # Strání
        -1L
    ))

interesting_places3 <- place_cases %>% 
    filter(place_code %in% c(
        541150L, # Kamenný Újezd
        555657L, # Toužim
        531316L, # Karlštejn
        555681L, # Útvina
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
        standard_label('Počet aktivních případů Covid-19 ve vybraných obcích - Novobystřicko') +
        custom_theme +        
        theme(
            panel.grid.major.x=element_line(color='#B0B0B0'),
            plot.title=element_text(size=12),
        ) +
        NULL)

(g19b <- interesting_places2 %>%
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
        standard_label('Počet aktivních případů Covid-19 ve vybraných obcích - Slovácko') +
        custom_theme +        
        theme(
            panel.grid.major.x=element_line(color='#B0B0B0'),
            plot.title=element_text(size=12),
        ) +
        NULL)

(g19c <- interesting_places3 %>%
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
        standard_label('Počet aktivních případů Covid-19 ve vybraných obcích - průřez JBR') +
        custom_theme +        
        theme(
            panel.grid.major.x=element_line(color='#B0B0B0'),
            plot.title=element_text(size=12),
        ) +
        NULL)

png(paste0('outputs/', file_date, '/covid_focus_nb_', file_date, '.png'), width=2000, height=1200, res=120, type='cairo')
print(g19)
dev.off()
png('outputs/covid_focus_nb_latest.png', width=1500, height=800, res=90, type='cairo')
print(g19)
dev.off()
png('outputs/covid_focus_sl_latest.png', width=1500, height=800, res=90, type='cairo')
print(g19b)
dev.off()
png('outputs/covid_focus_jbr_latest.png', width=1500, height=800, res=90, type='cairo')
print(g19c)
dev.off()

rm(g19, g19b, g19c, interesting_places, interesting_places2, interesting_places3)



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

(g32 <- spread_by_region %>% 
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
        custom_theme +
        NULL)

png('outputs/spread_factor.png', width=2000L, height=1200L, type='cairo', res=120)
print(g32)
dev.off()

rm(g32, spread_by_region)



place_rel_cases <- place_cases %>% 
    filter(
        date >= today() - weeks(1L),
        date < today(),
    ) %>% 
    group_by(place_code) %>% 
    arrange(.by_group=T, date) %>% 
    summarise(
        new_cases = sum(new_cases),
        init_act_cases = first(act_cases),
        act_cases = last(act_cases),
        .groups = 'drop'
    ) %>% 
    filter(
        new_cases > 0L,
        act_cases > 0L,
    ) %>% 
    inner_join(master_place, by=c('place_code'='place_id')) %>% 
    filter(pop_total > 0L) %>% 
    transmute(
        place_name,
        q_act_cases = act_cases / pop_total,
        q_new_cases = new_cases / (pop_total - init_act_cases),
        init_act_cases,
        new_cases,
        act_cases,
        region_abbr,
        district_nuts,
        place_id = place_code,
        pop_total,
        avg_age_total,
        place_x,
        place_y,
    )

place_top_cases <- place_rel_cases %>% 
    mutate(
        q_act_cases_threshold = quantile(q_act_cases, 0.9),
        q_new_cases_threshold = quantile(q_new_cases, 0.9),
    ) %>% 
    filter(
        pop_total >= 500L,
        new_cases >= 5L,
    ) %>% 
    group_by(region_abbr) %>% 
    arrange(.by_group=T, desc(q_act_cases), desc(q_new_cases), desc(pop_total)) %>% 
    slice_head(n=20L) %>% 
    ungroup() %>% 
    filter(
        (q_new_cases >= q_new_cases_threshold) | (q_act_cases >= q_act_cases_threshold),
        !is.na(place_x),
        !is.na(place_y),
    ) %>% 
    arrange(q_new_cases, q_act_cases, pop_total)

region_boundaries <- rnaturalearth::ne_states(country = 'Czech Republic', returnclass='sf')
krovak_projection <- CRS('+init=epsg:5514')

place_top_sf <- SpatialPointsDataFrame(
    place_top_cases %>% select(place_x, place_y),
    data=place_top_cases,
    proj4string=krovak_projection
) %>% 
    sf::st_as_sf() %>% 
    mutate(
        name_short = str_trunc(place_name, 17L, ellipsis=''),
    )

(g31 <- ggplot() +
        geom_sf(data=region_boundaries, colour='darkgray', fill='#4B4B4B', alpha=0.3, size=0.3) +
        geom_sf(data=place_top_sf, aes(size=q_act_cases, colour=q_new_cases), alpha=0.3) +
        geom_text_repel(
            data = place_top_sf,
            mapping = aes(geometry=geometry, x=after_stat(x), y=after_stat(y), label=place_name),
            colour='black',
            stat = 'sf_coordinates',
            size = 3,
            na.rm = T,
            box.padding=unit(2, 'pt'),
            max.iter = 500,
            seed = 199137317L,
        ) +
        scale_size_continuous(name='Zasažení populace', range=c(0.1, 8), labels=percent_format(accuracy=1), guide=guide_legend(order=1L)) +
        scale_colour_gradient(name='Relativní přírůstek', low='blue', high='red', label=percent_format(accuracy=1), guide=guide_legend(order=2L)) +
        standard_label('Významný výskyt nákaz za posledních 7 dnů') +
        theme_minimal() +
        theme(
            #text=element_text(debug=T),
            panel.border=element_blank(),
            panel.grid=element_blank(),
            axis.line.x = NULL,
            axis.line.y = NULL,
            axis.text = element_blank(), 
            axis.title = element_blank(), 
            legend.position=c(1,1),
            legend.justification=c(1,1),
            legend.direction='horizontal',
            legend.box.just='right',
            legend.box.margin=margin(0,0,0,0),
            legend.background=element_rect(fill='white'),
        ) +
        NULL)

png(paste0('outputs/', file_date,  '/covid-hotspots-', file_date, '.png'), width=3000L, height=2000L, type='cairo', res=120)
print(g31)
dev.off()
png(paste0('outputs/covid_hotspots_latest.png'), width=2000L, height=1300L, type='cairo', res=120)
print(g31)
dev.off()

rm(g31, place_top_sf, place_top_cases, place_rel_cases, krovak_projection, region_boundaries)
