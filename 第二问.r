library(pheatmap)
load("第二问数据处理.RData")
dailyToMonthly <- function(daily_data, fn = function(x) rowMeans(x, na.rm = TRUE)) {
    monthly_data <- cbind(fn(daily_data[, 1:31]), fn(daily_data[, 32:60]), fn(daily_data[, 61:91]), fn(daily_data[, 92:121]), fn(daily_data[, 122:152]), fn(daily_data[, 153:182]), fn(daily_data[, 183:213]), fn(daily_data[, 214:244]), fn(daily_data[, 245:274]), fn(daily_data[, 275:305]), fn(daily_data[, 306:335]), fn(daily_data[, 336:366]))
    colnames(monthly_data) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    return(monthly_data)
}
superior_basin_min_air_temperature <- dailyToMonthly(superior_basin_daliy[[1]])
superior_basin_max_air_temperature <- dailyToMonthly(superior_basin_daliy[[2]])
superior_basin_mean_precipitation <- dailyToMonthly(superior_basin_daliy[[3]], function(x) rowSums(x, na.rm = TRUE))
superior_basin_mean_air_temperature <- dailyToMonthly(superior_basin_daliy[[4]])
superior_basin_mean_dewpoint <- dailyToMonthly(superior_basin_daliy[[5]])
superior_basin_mean_wind_speed <- dailyToMonthly(superior_basin_daliy[[6]])
superior_basin_mean_cloud_cover <- dailyToMonthly(superior_basin_daliy[[7]])
superior_mean_surface_water_temperature <- dailyToMonthly(superior_mean_surface_water_temperature)
michigan_basin_min_air_temperature <- dailyToMonthly(michigan_basin_daliy[[1]])
michigan_basin_max_air_temperature <- dailyToMonthly(michigan_basin_daliy[[2]])
michigan_basin_mean_precipitation <- dailyToMonthly(michigan_basin_daliy[[3]], function(x) rowSums(x, na.rm = TRUE))
michigan_basin_mean_air_temperature <- dailyToMonthly(michigan_basin_daliy[[4]])
michigan_basin_mean_dewpoint <- dailyToMonthly(michigan_basin_daliy[[5]])
michigan_basin_mean_wind_speed <- dailyToMonthly(michigan_basin_daliy[[6]])
michigan_basin_mean_cloud_cover <- dailyToMonthly(michigan_basin_daliy[[7]])
michigan_mean_surface_water_temperature <- dailyToMonthly(michigan_huron_mean_surface_water_temperature)
huron_basin_min_air_temperature <- dailyToMonthly(huron_basin_daliy[[1]])
huron_basin_max_air_temperature <- dailyToMonthly(huron_basin_daliy[[2]])
huron_basin_mean_precipitation <- dailyToMonthly(huron_basin_daliy[[3]], function(x) rowSums(x, na.rm = TRUE))
huron_basin_mean_air_temperature <- dailyToMonthly(huron_basin_daliy[[4]])
huron_basin_mean_dewpoint <- dailyToMonthly(huron_basin_daliy[[5]])
huron_basin_mean_wind_speed <- dailyToMonthly(huron_basin_daliy[[6]])
huron_basin_mean_cloud_cover <- dailyToMonthly(huron_basin_daliy[[7]])
huron_mean_surface_water_temperature <- dailyToMonthly(michigan_huron_mean_surface_water_temperature)
erie_basin_min_air_temperature <- dailyToMonthly(erie_basin_daliy[[1]])
erie_basin_max_air_temperature <- dailyToMonthly(erie_basin_daliy[[2]])
erie_basin_mean_precipitation <- dailyToMonthly(erie_basin_daliy[[3]], function(x) rowSums(x, na.rm = TRUE))
erie_basin_mean_air_temperature <- dailyToMonthly(erie_basin_daliy[[4]])
erie_basin_mean_dewpoint <- dailyToMonthly(erie_basin_daliy[[5]])
erie_basin_mean_wind_speed <- dailyToMonthly(erie_basin_daliy[[6]])
erie_basin_mean_cloud_cover <- dailyToMonthly(erie_basin_daliy[[7]])
erie_mean_surface_water_temperature <- dailyToMonthly(erie_mean_surface_water_temperature)
ontario_basin_min_air_temperature <- dailyToMonthly(ontario_basin_daliy[[1]])
ontario_basin_max_air_temperature <- dailyToMonthly(ontario_basin_daliy[[2]])
ontario_basin_mean_precipitation <- dailyToMonthly(ontario_basin_daliy[[3]], function(x) rowSums(x, na.rm = TRUE))
ontario_basin_mean_air_temperature <- dailyToMonthly(ontario_basin_daliy[[4]])
ontario_basin_mean_dewpoint <- dailyToMonthly(ontario_basin_daliy[[5]])
ontario_basin_mean_wind_speed <- dailyToMonthly(ontario_basin_daliy[[6]])
ontario_basin_mean_cloud_cover <- dailyToMonthly(ontario_basin_daliy[[7]])
ontario_mean_surface_water_temperature <- dailyToMonthly(ontario_mean_surface_water_temperature)
matrixToRow <- function(matrix_data) {
    return(unlist(as.data.frame(t(matrix_data[as.character(2000:2020), ]))))
}
superior_cor <- cbind(matrixToRow(superior_basin_min_air_temperature), matrixToRow(superior_basin_max_air_temperature), matrixToRow(superior_basin_mean_precipitation), matrixToRow(superior_basin_mean_air_temperature), matrixToRow(superior_basin_mean_dewpoint), matrixToRow(superior_basin_mean_wind_speed), matrixToRow(superior_basin_mean_cloud_cover), matrixToRow(superior_mean_surface_water_temperature), matrixToRow(superior_mean_evaporation), matrixToRow(superior_mean_run_off), matrixToRow(superior_mean_water_level))
colnames(superior_cor) <- c("低温", "高温", "降水", "均温", "露点", "风速", "云盖", "水温", "蒸发", "径流", "水位")
pheatmap(cor(superior_cor, use = "complete.obs"), display_numbers = TRUE)
michigan_cor <- cbind(matrixToRow(michigan_basin_min_air_temperature), matrixToRow(michigan_basin_max_air_temperature), matrixToRow(michigan_basin_mean_precipitation), matrixToRow(michigan_basin_mean_air_temperature), matrixToRow(michigan_basin_mean_dewpoint), matrixToRow(michigan_basin_mean_wind_speed), matrixToRow(michigan_basin_mean_cloud_cover), matrixToRow(michigan_mean_surface_water_temperature), matrixToRow(michigan_mean_evaporation), matrixToRow(michigan_mean_run_off), matrixToRow(michigan_huron_mean_water_level))
colnames(michigan_cor) <- c("低温", "高温", "降水", "均温", "露点", "风速", "云盖", "水温", "蒸发", "径流", "水位")
pheatmap(cor(michigan_cor, use = "complete.obs"), display_numbers = TRUE)
huron_cor <- cbind(matrixToRow(huron_basin_min_air_temperature), matrixToRow(huron_basin_max_air_temperature), matrixToRow(huron_basin_mean_precipitation), matrixToRow(huron_basin_mean_air_temperature), matrixToRow(huron_basin_mean_dewpoint), matrixToRow(huron_basin_mean_wind_speed), matrixToRow(huron_basin_mean_cloud_cover), matrixToRow(huron_mean_surface_water_temperature), matrixToRow(huron_mean_evaporation), matrixToRow(huron_mean_run_off), matrixToRow(michigan_huron_mean_water_level))
colnames(huron_cor) <- c("低温", "高温", "降水", "均温", "露点", "风速", "云盖", "水温", "蒸发", "径流", "水位")
pheatmap(cor(huron_cor, use = "complete.obs"), display_numbers = TRUE)
erie_cor <- cbind(matrixToRow(erie_basin_min_air_temperature), matrixToRow(erie_basin_max_air_temperature), matrixToRow(erie_basin_mean_precipitation), matrixToRow(erie_basin_mean_air_temperature), matrixToRow(erie_basin_mean_dewpoint), matrixToRow(erie_basin_mean_wind_speed), matrixToRow(erie_basin_mean_cloud_cover), matrixToRow(erie_mean_surface_water_temperature), matrixToRow(erie_mean_evaporation), matrixToRow(erie_mean_run_off), matrixToRow(erie_mean_water_level))
colnames(erie_cor) <- c("低温", "高温", "降水", "均温", "露点", "风速", "云盖", "水温", "蒸发", "径流", "水位")
pheatmap(cor(erie_cor, use = "complete.obs"), display_numbers = TRUE)
ontario_cor <- cbind(matrixToRow(ontario_basin_min_air_temperature), matrixToRow(ontario_basin_max_air_temperature), matrixToRow(ontario_basin_mean_precipitation), matrixToRow(ontario_basin_mean_air_temperature), matrixToRow(ontario_basin_mean_dewpoint), matrixToRow(ontario_basin_mean_wind_speed), matrixToRow(ontario_basin_mean_cloud_cover), matrixToRow(ontario_mean_surface_water_temperature), matrixToRow(ontario_mean_evaporation), matrixToRow(ontario_mean_run_off), matrixToRow(ontario_mean_water_level))
colnames(ontario_cor) <- c("低温", "高温", "降水", "均温", "露点", "风速", "云盖", "水温", "蒸发", "径流", "水位")
pheatmap(cor(ontario_cor, use = "complete.obs"), display_numbers = TRUE)
write.csv(cor(superior_cor, use = "complete.obs"), "第二问/superior.csv")
write.csv(cor(michigan_cor, use = "complete.obs"), "第二问/michigan.csv")
write.csv(cor(huron_cor, use = "complete.obs"), "第二问/huron.csv")
write.csv(cor(erie_cor, use = "complete.obs"), "第二问/erie.csv")
write.csv(cor(ontario_cor, use = "complete.obs"), "第二问/ontario.csv")

# 河流
st_mary_flow <- getOfficalData("St. Mary's River")
st_clair_flow <- getOfficalData("St. Clair River")
detroit_flow <- getOfficalData("Detroit River")
niagara_flow <- getOfficalData("Niagara River")
st_lawrence_flow <- getOfficalData("St. Lawrence River")

# 网络
getSeasonCor <- function(month) {
    matrixToRow <- function(matrix_data) {
        return(unlist(as.data.frame(t(matrix_data[as.character(2000:2020), month]))))
    }
    superior_effctive <- cbind(matrixToRow(superior_basin_mean_air_temperature), matrixToRow(superior_basin_mean_dewpoint), matrixToRow(superior_basin_mean_wind_speed), matrixToRow(superior_basin_mean_cloud_cover), matrixToRow(superior_mean_surface_water_temperature), matrixToRow(st_mary_flow))
    colnames(superior_effctive) <- c("S均温", "S露点", "S风速", "S云盖", "S水温", "S出MH入")
    michigan_effctive <- cbind(matrixToRow(michigan_basin_mean_air_temperature), matrixToRow(michigan_basin_mean_dewpoint), matrixToRow(michigan_basin_mean_wind_speed), matrixToRow(michigan_basin_mean_cloud_cover), matrixToRow(michigan_mean_surface_water_temperature))
    colnames(michigan_effctive) <- c("M均温", "M露点", "M风速", "M云盖", "M水温")
    huron_effctive <- cbind(matrixToRow(huron_basin_mean_air_temperature), matrixToRow(huron_basin_mean_dewpoint), matrixToRow(huron_basin_mean_cloud_cover), matrixToRow(huron_mean_surface_water_temperature), matrixToRow(st_clair_flow))
    colnames(huron_effctive) <- c("H均温", "H露点", "H云盖", "H水温", "MH出")
    erie_effctive <- cbind(matrixToRow(detroit_flow), matrixToRow(erie_basin_mean_air_temperature), matrixToRow(erie_basin_mean_dewpoint), matrixToRow(erie_basin_mean_wind_speed), matrixToRow(erie_basin_mean_cloud_cover), matrixToRow(erie_mean_surface_water_temperature), matrixToRow(niagara_flow))
    colnames(erie_effctive) <- c("E入", "E均温", "E露点", "E风速", "E云盖", "E水温", "E出O入")
    ontario_effctive <- cbind(matrixToRow(ontario_basin_mean_air_temperature), matrixToRow(ontario_basin_mean_dewpoint), matrixToRow(ontario_basin_mean_wind_speed), matrixToRow(ontario_basin_mean_cloud_cover), matrixToRow(ontario_mean_evaporation), matrixToRow(st_lawrence_flow)) # nolint: object_usage_linter.
    colnames(ontario_effctive) <- c("O均温", "O露点", "O风速", "O云盖", "O蒸发", "O出")
    effctive_cor <- cbind(superior_effctive, michigan_effctive, huron_effctive, erie_effctive, ontario_effctive)
    return(cor(effctive_cor, use = "complete.obs"))
}
cor_spring <- getSeasonCor(3:5)
pheatmap(cor_spring, display_numbers = TRUE)
write.csv(cor_spring, "第二问/春.csv")
cor_summer <- getSeasonCor(6:8)
pheatmap(cor_summer, display_numbers = TRUE)
write.csv(cor_summer, "第二问/夏.csv")
cor_fall <- getSeasonCor(9:11)
pheatmap(cor_fall, display_numbers = TRUE)
write.csv(cor_fall, "第二问/秋.csv")
cor_winter <- getSeasonCor(c(12, 1, 2))
pheatmap(cor_winter, display_numbers = TRUE)
write.csv(cor_winter, "第二问/冬.csv")

# 变异系数
nor_min_max <- function(x) {
  y <- na.omit(x)
  return((x - min(y)) / (max(y) - min(y)))
}
superior_name <- c("S均温", "S露点", "S风速", "S云盖", "S水温", "S出MH入")
michigan_name <- c("S出MH入", "M均温", "M露点", "M风速", "M云盖", "M水温", "MH出")
huron_name <- c("S出MH入", "H均温", "H露点", "H云盖", "H水温", "MH出")
erie_name <- c("E入", "E均温", "E露点", "E风速", "E云盖", "E水温", "E出O入")
ontario_name <- c("E出O入", "O均温", "O露点", "O风速", "O云盖", "O蒸发", "O出")
superior_variations <- matrix(dimnames = list(superior_name, c("春", "夏", "秋", "冬")), ncol = 4, nrow = 6)
michigan_variations <- matrix(dimnames = list(michigan_name, c("春", "夏", "秋", "冬")), ncol = 4, nrow = 7)
huron_variations <- matrix(dimnames = list(huron_name, c("春", "夏", "秋", "冬")), ncol = 4, nrow = 6)
erie_variations <- matrix(dimnames = list(erie_name, c("春", "夏", "秋", "冬")), ncol = 4, nrow = 7)
ontario_variations <- matrix(dimnames = list(ontario_name, c("春", "夏", "秋", "冬")), ncol = 4, nrow = 7)
getSuperiorVariation <- function(month) {
    matrixToRow <- function(matrix_data) {
        return(unlist(as.data.frame(t(matrix_data[as.character(2000:2020), month]))))
    }
    effctive_variations <- apply(cbind(matrixToRow(superior_basin_mean_air_temperature), matrixToRow(superior_basin_mean_dewpoint), matrixToRow(superior_basin_mean_wind_speed), matrixToRow(superior_basin_mean_cloud_cover), matrixToRow(superior_mean_surface_water_temperature), matrixToRow(st_mary_flow)), 2, nor_min_max)
    colnames(effctive_variations) <- c(superior_name)
    return(apply(effctive_variations, 2, sd, TRUE) / colMeans(effctive_variations, TRUE))
}
getMichiganVariation <- function(month) {
    matrixToRow <- function(matrix_data) {
        return(unlist(as.data.frame(t(matrix_data[as.character(2000:2020), month]))))
    }
    effctive_variations <- apply(cbind(matrixToRow(st_mary_flow), matrixToRow(michigan_basin_mean_air_temperature), matrixToRow(michigan_basin_mean_dewpoint), matrixToRow(michigan_basin_mean_wind_speed), matrixToRow(michigan_basin_mean_cloud_cover), matrixToRow(michigan_mean_surface_water_temperature), matrixToRow(st_clair_flow)), 2, nor_min_max)
    colnames(effctive_variations) <- michigan_name
    return(apply(effctive_variations, 2, sd, TRUE) / colMeans(effctive_variations, TRUE))
}
getHuronVariation <- function(month) {
    matrixToRow <- function(matrix_data) {
        return(unlist(as.data.frame(t(matrix_data[as.character(2000:2020), month]))))
    }
    effctive_variations <- apply(cbind(matrixToRow(st_mary_flow), matrixToRow(huron_basin_mean_air_temperature), matrixToRow(huron_basin_mean_dewpoint), matrixToRow(huron_basin_mean_cloud_cover), matrixToRow(huron_mean_surface_water_temperature), matrixToRow(st_clair_flow)), 2, nor_min_max)
    colnames(effctive_variations) <- huron_name
    return(apply(effctive_variations, 2, sd, TRUE) / colMeans(effctive_variations, TRUE))
}
getErieVariation <- function(month) {
    matrixToRow <- function(matrix_data) {
        return(unlist(as.data.frame(t(matrix_data[as.character(2000:2020), month]))))
    }
    effctive_variations <- apply(cbind(matrixToRow(detroit_flow), matrixToRow(erie_basin_mean_air_temperature), matrixToRow(erie_basin_mean_dewpoint), matrixToRow(erie_basin_mean_wind_speed), matrixToRow(erie_basin_mean_cloud_cover), matrixToRow(erie_mean_surface_water_temperature), matrixToRow(niagara_flow)), 2, nor_min_max)
    colnames(effctive_variations) <- erie_name
    return(apply(effctive_variations, 2, sd, TRUE) / colMeans(effctive_variations, TRUE))
}
getOntarioVariation <- function(month) {
    matrixToRow <- function(matrix_data) {
        return(unlist(as.data.frame(t(matrix_data[as.character(2000:2020), month]))))
    }
    effctive_variations <- apply(cbind(matrixToRow(niagara_flow), matrixToRow(ontario_basin_mean_air_temperature), matrixToRow(ontario_basin_mean_dewpoint), matrixToRow(ontario_basin_mean_wind_speed), matrixToRow(ontario_basin_mean_cloud_cover), matrixToRow(ontario_mean_evaporation), matrixToRow(st_lawrence_flow)), 2, nor_min_max) # nolint: object_usage_linter.
    colnames(effctive_variations) <- ontario_name
    return(apply(effctive_variations, 2, sd, TRUE) / colMeans(effctive_variations, TRUE))
}
superior_variations[, "春"] <- getSuperiorVariation(3:5)
superior_variations[, "夏"] <- getSuperiorVariation(6:8)
superior_variations[, "秋"] <- getSuperiorVariation(9:11)
superior_variations[, "冬"] <- getSuperiorVariation(c(12, 1, 2))
michigan_variations[, "春"] <- getMichiganVariation(3:5)
michigan_variations[, "夏"] <- getMichiganVariation(6:8)
michigan_variations[, "秋"] <- getMichiganVariation(9:11)
michigan_variations[, "冬"] <- getMichiganVariation(c(12, 1, 2))
huron_variations[, "春"] <- getHuronVariation(3:5)
huron_variations[, "夏"] <- getHuronVariation(6:8)
huron_variations[, "秋"] <- getHuronVariation(9:11)
huron_variations[, "冬"] <- getHuronVariation(c(12, 1, 2))
erie_variations[, "春"] <- getErieVariation(3:5)
erie_variations[, "夏"] <- getErieVariation(6:8)
erie_variations[, "秋"] <- getErieVariation(9:11)
erie_variations[, "冬"] <- getErieVariation(c(12, 1, 2))
ontario_variations[, "春"] <- getOntarioVariation(3:5)
ontario_variations[, "夏"] <- getOntarioVariation(6:8)
ontario_variations[, "秋"] <- getOntarioVariation(9:11)
ontario_variations[, "冬"] <- getOntarioVariation(c(12, 1, 2))
write.csv(superior_variations, "第二问/superior变异系数.csv")
write.csv(michigan_variations, "第二问/michigan变异系数.csv")
write.csv(huron_variations, "第二问/huron变异系数.csv")
write.csv(erie_variations, "第二问/erie变异系数.csv")
write.csv(ontario_variations, "第二问/ontario变异系数.csv")
superior_variations <- superior_variations / colSums(superior_variations)
michigan_variations <- michigan_variations / colSums(michigan_variations)
huron_variations <- huron_variations / colSums(huron_variations)
erie_variations <- erie_variations / colSums(erie_variations)
ontario_variations <- ontario_variations / colSums(ontario_variations)
superior_interactivity <- matrix(dimnames = list(unique(c(michigan_name, huron_name, erie_name, ontario_name)), c("春", "夏", "秋", "冬")), ncol = 4, nrow = 24)
