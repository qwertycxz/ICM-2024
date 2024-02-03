library(readxl)
# 平均水位
getOfficalData <- function(sheet) {
    data_frame <- as.data.frame(read_excel("D/Problem_D_Great_Lakes.xlsx", sheet, na = '---', skip = 6))
    row.names(data_frame) <- data_frame$Year
    return(data_frame[, -1])
}
superior_mean_water_level <- getOfficalData("Lake Superior")
michigan_huron_mean_water_level <- getOfficalData("Lake Michigan and Lake Huron")
erie_mean_water_level <- getOfficalData("Lake Erie")
ontario_mean_water_level <- getOfficalData("Lake Ontario")
# 表层平均水温
superior_mean_surface_water_temperature <- matrix(dimnames = list(1995:2022, 1:366), ncol = 366, nrow = 28)
michigan_huron_mean_surface_water_temperature <- superior_mean_surface_water_temperature
erie_mean_surface_water_temperature <- superior_mean_surface_water_temperature
ontario_mean_surface_water_temperature <- superior_mean_surface_water_temperature
for (i in 1995:2022) {
    data_frame <- read.csv(paste0("https://apps.glerl.noaa.gov/coastwatch/ftp/glsea/avgtemps/", i, "/glsea-temps", i, "_1024.dat"), comment.char = "-", sep = "", skip = 7)
    if (nrow(data_frame) == 365) {
        data_frame <- rbind(data_frame[1:59, ], NA, data_frame[60:365, ])
    }
    index <- as.character(i)
    superior_mean_surface_water_temperature[index, ] <- data_frame$Sup.
    michigan_huron_mean_surface_water_temperature[index, ] <- rowMeans(data_frame[, 4:5])
    erie_mean_surface_water_temperature[index, ] <- data_frame$Erie
    ontario_mean_surface_water_temperature[index, ] <- data_frame$Ont.
}
# 平均蒸发量
superior_mean_evaporation <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/evaporation_sup.csv", skip = 3, row.names = 1)
michigan_mean_evaporation <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/evaporation_mic.csv", skip = 3, row.names = 1)
huron_mean_evaporation <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/evaporation_hur.csv", skip = 3, row.names = 1)
erie_mean_evaporation <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/evaporation_eri.csv", skip = 3, row.names = 1)
ontario_mean_evaporation <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/evaporation_ont.csv", skip = 3, row.names = 1)
# 日数据
getGlerlData <- function(path) {
    data_frame <- read.csv(paste0("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/", path), skip = 8, strip.white = TRUE)
    data_frame <- cbind(format(as.Date(data_frame[, 1]), "%Y"), data_frame)
    min_air_temperature <- matrix(dimnames = list(1940:2021, 1:366), ncol = 366, nrow = 82)
    max_air_temperature <- min_air_temperature
    mean_precipitation <- min_air_temperature
    mean_air_temperature <- min_air_temperature
    mean_dewpoint <- min_air_temperature
    mean_wind_speed <- min_air_temperature
    mean_cloud_cover <- min_air_temperature
    for (i in 1940:2021) {
        index <- as.character(i)
        data_frame_per_year <- data_frame[data_frame[, 1] == index, ]
        if (sum(data_frame[, 1] == index) == 365) {
            data_frame_per_year <- rbind(data_frame[1:59, ], NA, data_frame[60:365, ])
        }
        len <- seq_len(nrow(data_frame_per_year))
        min_air_temperature[index, len] <- data_frame_per_year[, 3]
        max_air_temperature[index, len] <- data_frame_per_year[, 4]
        mean_precipitation[index, len] <- data_frame_per_year[, 5]
        mean_air_temperature[index, len] <- data_frame_per_year[, 6]
        mean_dewpoint[index, len] <- data_frame_per_year[, 7]
        mean_wind_speed[index, len] <- data_frame_per_year[, 8]
        mean_cloud_cover[index, len] <- data_frame_per_year[, 9]
    }
    return(list(min_air_temperature, max_air_temperature, mean_precipitation, mean_air_temperature, mean_dewpoint, mean_wind_speed, mean_cloud_cover))
}
superior_basin_daliy <- getGlerlData("daily/subdata_eri_basn.csv")
michigan_basin_daliy <- getGlerlData("daily/subdata_mic_basn.csv")
huron_basin_daliy <- getGlerlData("daily/subdata_hur_basn.csv")
erie_basin_daliy <- getGlerlData("daily/subdata_eri_basn.csv")
ontario_basin_daliy <- getGlerlData("daily/subdata_ont_basn.csv")
getRunOffData <- function(path) {
    data_frame <- read.csv(paste0("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/", path), na.strings = "-999.99", skip = 2)
    run_off <- matrix(dimnames = list(1908:2020, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), ncol = 12, nrow = 113)
    for (i in 1908:2020) {
        index <- as.character(i)
        run_off[index, ] <- data_frame[data_frame[, 1] == i, 3]
    }
    return(run_off)
}
superior_mean_run_off <- getRunOffData("runoff_sup_arm.csv")
michigan_mean_run_off <- getRunOffData("runoff_mic_arm.csv")
huron_mean_run_off <- getRunOffData("runoff_hur_arm.csv")
erie_mean_run_off <- getRunOffData("runoff_eri_arm.csv")
ontario_mean_run_off <- getRunOffData("runoff_ont_arm.csv")
