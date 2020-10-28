################################################################################
# Regular update script
################################################################################

options(
    'encoding' = 'utf-8',
    'rgdal_show_exportToProj4_warnings' = 'none'
)

library(tidyr)
suppressWarnings(library(tibble))
suppressWarnings(library(readr))
library(lubridate, exclude=c('intersect', 'setdiff', 'union'), warn.conflicts=F)
library(stringr)
library(forcats)
library(dplyr, warn.conflicts=F)
library(purrr)
library(ggplot2)
library(scales, exclude=c('col_factor', 'discard'))
library(directlabels)
library(cowplot, exclude=c('stamp'))
suppressWarnings(library(patchwork, warn.conflicts=F))
library(slider)
library(ggrepel)
suppressMessages(library(sf))
library(rnaturalearth)
library(rnaturalearthdata)
suppressMessages(library(rgdal))


################################################################################
# Common functions and helper constants

standard_label <- function(title) labs(
    title = title,
    caption = 'Autor: Boleslav Bobčík, https://github.com/bbobcik/covid-cz\nZdroj dat: MZČR, https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19'
)

custom_theme <- theme_bw() +
    theme(
        plot.caption=element_text(size=8, colour='darkgray')
    )

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


################################################################################
# Prepare master data

source('ruian_data.R')
orp_info <- readRDS('data/orp_info.RDS')
orp_population_by_age <- readRDS('data/orp_population_by_age.RDS')
orp_population <- readRDS('data/orp_population.RDS')
region_population <- readRDS('data/region_population.RDS')
region_population_by_age <- readRDS('data/region_population_by_age.RDS')
district <- readRDS('data/district.RDS')


################################################################################
# Output preparation

file_date <- strftime(today(), '%Y-%m-%d')
today_dir <- paste0('outputs/', file_date)
if (!dir.exists(today_dir)) {
    dir.create(today_dir, recursive=T, showWarnings=F)
}


################################################################################
# Evaluate forecast

source('fcast_evaluation.R', local=T)


################################################################################
# Export regional trends

source('covid_obce_update.R', local=T)
