superior_mean_precipitation <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/prc_sup_land_mon.csv", skip = 5, strip.white = TRUE, row.names = 1)
michigan_mean_precipitation <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/prc_mic_land_mon.csv", skip = 5, strip.white = TRUE, row.names = 1)
huron_mean_precipitation <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/prc_hur_land_mon.csv", skip = 5, strip.white = TRUE, row.names = 1)
erie_mean_precipitation <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/prc_eri_land_mon.csv", skip = 5, strip.white = TRUE, row.names = 1)
ontario_mean_precipitation <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/prc_ont_land_mon.csv", skip = 5, strip.white = TRUE, row.names = 1)
superior_land_area <- 128000
michigan_land_area <- 118000
huron_land_area <- 51200
erie_land_area <- 61000
ontario_land_area <- 64000
superior_precipitation_flow <- superior_mean_precipitation[as.character(2000:2020), ] * superior_land_area * 1000 / c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) / 24 / 60 / 60
michigan_precipitation_flow <- michigan_mean_precipitation[as.character(2000:2020), ] * michigan_land_area * 1000 / c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) / 24 / 60 / 60
huron_precipitation_flow <- huron_mean_precipitation[as.character(2000:2020), ] * huron_land_area * 1000 / c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) / 24 / 60 / 60
erie_precipitation_flow <- erie_mean_precipitation[as.character(2000:2020), ] * erie_land_area * 1000 / c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) / 24 / 60 / 60
ontario_precipitation_flow <- ontario_mean_precipitation[as.character(2000:2020), ] * ontario_land_area * 1000 / c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) / 24 / 60 / 60
save.image("降水径流量.RData")
