library(sf, include.only=c())
library(maps, include.only=c())
library(rnaturalearth, include.only=c())
library(rnaturalearthdata, include.only=c())
library(rgdal)


region_boundaries <- rnaturalearth::ne_states(country = 'Czech Republic', returnclass='sf')

krovak_projection <- CRS('+init=epsg:5514')

orp_latest_sf <- sf::st_as_sf(SpatialPointsDataFrame(covid_growth_by_orp %>% select(orp_x, orp_y), data=covid_growth_by_orp, proj4string=krovak_projection)) %>% 
    mutate(
        name_short = str_trunc(orp_name_single, 17L, ellipsis=''),
        disc_week = factor(strftime(week, '%e.%b.')),
        disc_week = fct_inorder(disc_week),
    )

place_latest_sf <- sf::st_as_sf(SpatialPointsDataFrame(covid_growth_by_place %>% select(place_x, place_y), data=covid_growth_by_place, proj4string=krovak_projection)) %>% 
    mutate(
        name_short = str_trunc(place_name_single, 17L, ellipsis=''),
        disc_week = factor(strftime(week, '%e.%b.')),
        disc_week = fct_inorder(disc_week),
    )

(g21 <- ggplot() +
    geom_sf(data=region_boundaries, colour='black', fill='#4B4B4B', alpha=0.3) +
    geom_sf(data=orp_latest_sf, aes(size=dyn_factor, colour=disc_week), alpha=0.3) +
    geom_sf_text(data=orp_latest_sf, aes(label=name_short), size=3, na.rm=T) +
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
        legend.direction='horizontal',
        legend.box.just='right',
        legend.box.margin=margin(0,0,0,0),
        legend.background=element_rect(fill='white')
    ) +
    NULL)
    
(g22 <- ggplot() +
    geom_sf(data=region_boundaries, colour='black', fill='#4B4B4B', alpha=0.3) +
    geom_sf(data=place_latest_sf, aes(size=dyn_factor, colour=disc_week), alpha=0.3) +
    geom_sf_text(data=place_latest_sf, aes(label=name_short), size=3, na.rm=T) +
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
        legend.direction='horizontal',
        legend.box.just='right',
        legend.box.margin=margin(0,0,0,0),
        legend.background=element_rect(fill='white')
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
g31
dev.off()
png(paste0('outputs/covid_hotspots_latest.png'), width=2000L, height=1300L, type='cairo', res=120)
g31
dev.off()


ggplotly(g31)




ggplot() +
    geom_sf(data=region_boundaries, colour='darkgray', fill='#4B4B4B', alpha=0.3, size=0.3) +
    geom_sf(data=place_top_sf, aes(size=q_act_cases, colour=q_new_cases), alpha=0.3) +
    #geom_sf_text(data=place_top_sf, aes(label=name_short), size=2.5, na.rm=T, check_overlap=F) +
    NULL

if (requireNamespace("sf", quietly = TRUE)) {

    nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
    
    ggplot(nc) +
        stat_sf_coordinates()
    
    ggplot(nc) +
        geom_errorbarh(
            aes(geometry = geometry,
                xmin = after_stat(x) - 0.1,
                xmax = after_stat(x) + 0.1,
                y = after_stat(y),
                height = 0.04),
            stat = "sf_coordinates"
        )
    
    ggplot(nc) +
        geom_text(
            mapping = aes(geometry=geometry, x=after_stat(x), y=after_stat(y), label=NAME),
            stat = 'sf_coordinates',
        )

    ggplot2::stat_sf_coordinates(data=nc, mapping=aes(geometry=geometry)) %>% str()
    
}
