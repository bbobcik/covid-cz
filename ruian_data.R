# Obce detailne

# region_mzcr se pouziva v prehledech zdravotnickych kapacit
region_abbr_enum <- tribble(~nuts, ~region_abbr, ~region_iso, ~region_mzcr,
   'CZ010', 'Prg', 'CZ-PR', 'PHA',
   'CZ020', 'Str', 'CZ-ST', 'STC',
   'CZ031', 'JCe', 'CZ-JC', 'JHC',
   'CZ032', 'Plz', 'CZ-PL', 'PLK',
   'CZ041', 'KVa', 'CZ-KA', 'KVK',
   'CZ042', 'Ust', 'CZ-US', 'ULK',
   'CZ051', 'Lib', 'CZ-LI', 'LBK',
   'CZ052', 'HKr', 'CZ-KR', 'HKK',
   'CZ053', 'Par', 'CZ-PA', 'PAK',
   'CZ063', 'Vys', 'CZ-VY', 'VYS',
   'CZ064', 'JMo', 'CZ-JM', 'JHM',
   'CZ071', 'Olo', 'CZ-OL', 'OLK',
   'CZ072', 'Zli', 'CZ-ZL', 'ZLK',
   'CZ080', 'MSl', 'CZ-MO', 'MSK',
    )

obec_status_enum <- tribble(~status_id, ~status, ~status_text, ~weight,
    1L, 'MIL', 'Vojenský újezd', 1,
    2L, 'PLC', 'Obec', 1,
    3L, 'TWN', 'Město', 10,
    4L, 'CIT', 'Statutární město', 100,
    5L, 'CAP', 'Hlavní město', 1000,
    6L, 'SUB', 'Městys', 1,
    ) %>%
    mutate(
        status = factor(status, ordered=T, levels=c('MIL', 'SUB', 'PLC', 'TWN', 'CIT', 'CAP'))
    )

master_ruian <- read_delim(
    file='ruian.csv',
    delim=';',
    col_names=c('type', 'id', 'name', 'nuts', 'region', 'district', 'orp', 'pou', 'admin', 'status_id', 'x', 'y'),
    col_types=cols(
        type = readr::col_factor(),
        id = col_integer(),
        name = col_character(),
        nuts = col_character(),
        region = col_integer(),
        district = col_integer(),
        orp = col_integer(),
        pou = col_integer(),
        admin = col_integer(),
        status_id = col_integer(),
        x = col_double(),
        y = col_double()
    )
)

basic_places <- master_ruian %>% 
    filter(type == 'OBEC') %>% 
    inner_join(obec_status_enum, by='status_id') %>% 
    filter(weight > 0) %>% 
    transmute(
        id,
        place_x = x,
        place_y = y,
        weight,
        place_pou = pou,
        place_district = district,
    ) %>% 
    arrange(id)

basic_pou <- master_ruian %>% 
    filter(type == 'PUI') %>% 
    inner_join(basic_places, by=c(id='place_pou')) %>% 
    group_by(id, name, orp) %>% 
    summarise(
        places = n(),
        center_x = sum(place_x * weight) / sum(weight),
        center_y = sum(place_y * weight) / sum(weight),
        weight = sum(weight),
        net_km = sum(sqrt((x-place_x)^2 + (y-place_y)^2)/1000),
        .groups = 'drop'
    ) %>% 
    mutate(
        avg_km = round(net_km / places, 1),
        net_km = round(net_km, 1),
        avg_weight = round(weight / places, 1),
    ) %>% 
    select(id, name, orp, places, weight, avg_weight, net_km, avg_km, center_x, center_y) %>% 
    arrange(id)

basic_orp <- master_ruian %>% 
    filter(type == 'ORP') %>% 
    inner_join(basic_pou, by=c(id='orp'), suffix=c('', '_pou')) %>% 
    inner_join(basic_places, by=c(id_pou='place_pou'), suffix=c('', '_place')) %>%
    group_by(id, name, region) %>% 
    summarise(
        places = n(),
        weight = sum(weight_place),
        center_x = sum(place_x * weight_place) / sum(weight_place),
        center_y = sum(place_y * weight_place) / sum(weight_place),
        net_km = sum(sqrt((x-place_x)^2 + (y-place_y)^2)/1000),
        .groups = 'drop'
    ) %>% 
    mutate(
        avg_km = round(net_km / places, 1),
        net_km = round(net_km, 1),
        avg_weight = round(weight / places, 1),
    ) %>% 
    select(id, name, region, places, weight, avg_weight, net_km, avg_km, center_x, center_y) %>% 
    arrange(id)

basic_district <- master_ruian %>% 
    filter(type == 'OKRES') %>% 
    inner_join(basic_places, by=c(id='place_district'), suffix=c('', '_place')) %>% 
    group_by(id, name, region) %>% 
    summarise(
        places = n(),
        center_x = sum(place_x * weight) / sum(weight),
        center_y = sum(place_y * weight) / sum(weight),
        weight = sum(weight),
        net_km = sum(sqrt((center_x-place_x)^2 + (center_y-place_y)^2)/1000),
        .groups = 'drop'
    ) %>% 
    mutate(
        avg_km = round(net_km / places, 1),
        net_km = round(net_km, 1),
        avg_weight = round(weight / places, 1),
    ) %>% 
    select(id, name, region, places, weight, avg_weight, net_km, avg_km, center_x, center_y) %>% 
    arrange(id)

basic_region <- master_ruian %>% 
    filter(type == 'KRAJ') %>% 
    inner_join(basic_orp, by=c(id='region'), suffix=c('', '_orp')) %>% 
    inner_join(basic_pou, by=c(id_orp='orp'), suffix=c('', '_pou')) %>% 
    inner_join(basic_places, by=c(id_pou='place_pou'), suffix=c('', '_place')) %>% 
    group_by(id, name) %>% 
    summarise(
        places = n(),
        center_x = sum(place_x * weight) / sum(weight),
        center_y = sum(place_y * weight) / sum(weight),
        weight = sum(weight),
        net_km = sum(sqrt((center_x-place_x)^2 + (center_y-place_y)^2)/1000),
        .groups = 'drop'
    ) %>% 
    mutate(
        avg_km = round(net_km / places, 1),
        net_km = round(net_km, 1),
        avg_weight = round(weight / places, 1),
    ) %>% 
    select(id, name, places, weight, avg_weight, net_km, avg_km, center_x, center_y) %>% 
    arrange(id)


master_region <- master_ruian %>% 
    filter(type == 'KRAJ') %>% 
    inner_join(region_abbr_enum, by='nuts') %>% 
    inner_join(basic_region, by='id', suffix=c('', '_basic')) %>% 
    transmute(
        region_id = id,
        region_nuts = nuts,
        region_abbr,
        region_name = name,
        region_iso,
        region_x = x,
        region_y = y,
        center_x,
        center_y,
        places,
        weight,
        avg_weight,
        net_km,
        avg_km,
    ) %>% 
    arrange(region_nuts)

master_district <- master_ruian %>% 
    filter(type == 'OKRES') %>% 
    inner_join(master_region, by=c(region='region_id')) %>% 
    transmute(
        district_id = id,
        district_nuts = nuts,
        district_name = name,
        region_nuts,
        region_abbr,
        district_x = x,
        district_y = y,
    ) %>% 
    inner_join(basic_district, by=c(district_id='id')) %>% 
    transmute(
        district_id,
        district_nuts,
        district_name,
        region_nuts,
        region_abbr,
        district_x,
        district_y,
        center_x,
        center_y,
        places,
        weight,
        avg_weight,
        net_km,
        avg_km,
    ) %>% 
    arrange(region_nuts, district_nuts)

master_orp <- master_ruian %>% 
    filter(type == 'ORP') %>% 
    inner_join(master_region, by=c(region='region_id'), suffix=c('', '_region')) %>% 
    transmute(
        orp_id = id,
        orp_name = name,
        region_nuts,
        region_abbr,
        orp_x = x,
        orp_y = y,
        region_dist = round(sqrt((x-region_x)^2 + (y-region_y)^2) / 1000, 1),
    ) %>% 
    inner_join(basic_orp, by=c(orp_id='id')) %>% 
    transmute(
        orp_id,
        orp_name,
        region_nuts,
        region_abbr,
        orp_x,
        orp_y,
        places,
        weight,
        avg_weight,
        net_km,
        avg_km,
        region_dist,
    ) %>% 
    arrange(region_nuts, orp_id)

master_pou <- master_ruian %>% 
    filter(type == 'PUI') %>% 
    inner_join(master_orp, by=c(orp='orp_id'), suffix=c('', '_orp')) %>% 
    transmute(
        pou_id = id,
        pou_name = name,
        region_nuts,
        region_abbr,
        orp_id = orp,
        orp_name,
        pou_x = x,
        pou_y = y,
    ) %>% 
    inner_join(basic_pou, by=c(pou_id='id')) %>% 
    transmute(
        pou_id,
        pou_name,
        region_nuts,
        region_abbr,
        orp_id,
        orp_name,
        pou_x,
        pou_y,
        places,
        weight,
        avg_weight,
        net_km,
        avg_km,
    ) %>% 
    arrange(region_nuts, orp_id, pou_id)

place_population <- read_delim(
    file='data/place_population.csv',
    delim=';',
    col_names=c('place_id', 'pop_total', 'pop_m', 'pop_f', 'avg_age_total', 'avg_age_m', 'avg_age_f'),
    col_types=cols(
        col_integer(),
        col_integer(),
        col_integer(),
        col_integer(),
        col_double(),
        col_double(),
        col_double()
    ),
    skip=1L,
)

master_place <- master_ruian %>% 
    inner_join(obec_status_enum, by='status_id') %>%
    filter(type == 'OBEC', weight > 0) %>% 
    inner_join(master_pou, by=c(pou='pou_id'), suffix=c('', '_pou')) %>% 
    inner_join(master_district, by=c(district='district_id'), suffix=c('', '_district')) %>% 
    inner_join(master_region, by='region_nuts', suffix=c('', '_region')) %>% 
    inner_join(place_population, by=c('id'='place_id'), suffix=c('', '_popul')) %>% 
    transmute(
        place_id = id,
        place_name = name,
        place_nuts = nuts,
        region_nuts,
        region_abbr,
        region_name,
        district_nuts,
        district_name,
        orp_id,
        orp_name,
        pou_id = pou,
        pou_name,
        place_x = x,
        place_y = y,
        status,
        status_text,
        weight,
        pou_dist = round(sqrt((x-pou_x)^2 + (y-pou_y)^2) / 1000, 1),
        district_dist = round(sqrt((x-district_x)^2 + (y-district_y)^2) / 1000, 1),
        region_dist = round(sqrt((x-region_x)^2 + (y-region_y)^2) / 1000, 1),
        pop_total,
        pop_m,
        pop_f,
        avg_age_total,
        avg_age_m,
        avg_age_f,
    ) %>%
    arrange(region_nuts, district_nuts, desc(weight), place_id)
    
