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
# TODO: 日数据
data_frame <- read.csv("https://www.glerl.noaa.gov/pubs/tech_reports/glerl-083/UpdatedFiles/daily/subdata_eri_basn.csv", skip = 8)
data_frame <- cbind(as.integer(format(as.Date(data_frame[, 1]), "%Y")), data_frame)

for (i in 1940:2021) {
    data_frame_per_year <- data_frame[data_frame[, 1] == i, ]
    if (sum(data_frame[, 1] == i) == 365) {
        data_frame_per_year <- rbind(data_frame[1:59, ], NA, data_frame[60:365, ])
    }
    rbind(erie_basin_mean_air_temperature, data_frame_per_year[, 3])
}
superior_basin_min_air_temperature <- getOfficalData("Superior Min Air Temp")