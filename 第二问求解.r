library(readxl)
interactivity <- as.data.frame(read_excel("交互性.xlsx", 1))
row.names(interactivity) <- apply(interactivity[, 1:2], 1, function(x) {
    paste0(x[1], "_", x[2])
})
ideal <- as.data.frame(read_excel("交互性.xlsx", 2))
row.names(ideal) <- ideal$Year
getWaterLevel <- function(month, conefficient = 0.01, superior = NA, michigan = NA, huron = NA, erie = NA, ontario = NA) {
    ideal_filtered <- ideal[, month + 1]
    interactivity_filtered <- interactivity[, c(1, 2, c(4, 4, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4)[month] + 2)]
    if (is.na(superior)) {
        michigan_high <- (michigan - ideal_filtered[2]) * interactivity_filtered[interactivity_filtered[, 1] == "superior" & interactivity_filtered[, 2] == "michigan", 3]
        huron_high <- (huron - ideal_filtered[3]) * interactivity_filtered[interactivity_filtered[, 1] == "superior" & interactivity_filtered[, 2] == "huron", 3]
        erie_high <- (erie - ideal_filtered[4]) * interactivity_filtered[interactivity_filtered[, 1] == "superior" & interactivity_filtered[, 2] == "erie", 3]
        ontario_high <- (ontario - ideal_filtered[5]) * interactivity_filtered[interactivity_filtered[, 1] == "superior" & interactivity_filtered[, 2] == "ontario", 3]
        return(ideal_filtered[1] - (michigan_high + huron_high + erie_high + ontario_high) * conefficient)
    }
    if (is.na(michigan)) {
        superior_high <- (superior - ideal_filtered[1]) * interactivity_filtered[interactivity_filtered[, 1] == "michigan" & interactivity_filtered[, 2] == "superior", 3]
        erie_high <- (erie - ideal_filtered[4]) * interactivity_filtered[interactivity_filtered[, 1] == "michigan" & interactivity_filtered[, 2] == "erie", 3]
        ontario_high <- (ontario - ideal_filtered[5]) * interactivity_filtered[interactivity_filtered[, 1] == "michigan" & interactivity_filtered[, 2] == "ontario", 3]
        return(ideal_filtered[2] - (superior_high + erie_high + ontario_high) * conefficient)
    }
    if (is.na(huron)) {
        superior_high <- (superior - ideal_filtered[1]) * interactivity_filtered[interactivity_filtered[, 1] == "huron" & interactivity_filtered[, 2] == "superior", 3]
        erie_high <- (erie - ideal_filtered[4]) * interactivity_filtered[interactivity_filtered[, 1] == "huron" & interactivity_filtered[, 2] == "erie", 3]
        ontario_high <- (ontario - ideal_filtered[5]) * interactivity_filtered[interactivity_filtered[, 1] == "huron" & interactivity_filtered[, 2] == "ontario", 3]
        return(ideal_filtered[3] - (superior_high + erie_high + ontario_high) * conefficient)
    }
    if (is.na(erie)) {
        superior_high <- (superior - ideal_filtered[1]) * interactivity_filtered[interactivity_filtered[, 1] == "erie" & interactivity_filtered[, 2] == "superior", 3]
        michigan_high <- (michigan - ideal_filtered[2]) * interactivity_filtered[interactivity_filtered[, 1] == "erie" & interactivity_filtered[, 2] == "michigan", 3]
        huron_high <- (huron - ideal_filtered[3]) * interactivity_filtered[interactivity_filtered[, 1] == "erie" & interactivity_filtered[, 2] == "huron", 3]
        ontario_high <- (ontario - ideal_filtered[5]) * interactivity_filtered[interactivity_filtered[, 1] == "erie" & interactivity_filtered[, 2] == "ontario", 3]
        return(ideal_filtered[4] - (superior_high + michigan_high + huron_high + ontario_high) * conefficient)
    }
    superior_high <- (superior - ideal_filtered[1]) * interactivity_filtered[interactivity_filtered[, 1] == "ontario" & interactivity_filtered[, 2] == "superior", 3]
    michigan_high <- (michigan - ideal_filtered[2]) * interactivity_filtered[interactivity_filtered[, 1] == "ontario" & interactivity_filtered[, 2] == "michigan", 3]
    huron_high <- (huron - ideal_filtered[3]) * interactivity_filtered[interactivity_filtered[, 1] == "ontario" & interactivity_filtered[, 2] == "huron", 3]
    erie_high <- (erie - ideal_filtered[4]) * interactivity_filtered[interactivity_filtered[, 1] == "ontario" & interactivity_filtered[, 2] == "erie", 3]
    return(ideal_filtered[5] - (superior_high + michigan_high + huron_high + erie_high) * conefficient)
}
getWaterLevel(1, 0.01, superior = 183.71, michigan = 177.26, huron = 177.26, erie = 174.8)

getOfficalData <- function(sheet) {
    data_frame <- as.data.frame(read_excel("D/Problem_D_Great_Lakes.xlsx", sheet, na = '---', skip = 6))
    row.names(data_frame) <- data_frame$Year
    return(data_frame[, -1])
}
superior_mean_water_level <- getOfficalData("Lake Superior")
michigan_huron_mean_water_level <- getOfficalData("Lake Michigan and Lake Huron")
erie_mean_water_level <- getOfficalData("Lake Erie")
ontario_mean_water_level <- getOfficalData("Lake Ontario")

superior_predict_water_level <- c()
for (i in 1:12) {
    superior_predict_water_level <- c(superior_predict_water_level, getWaterLevel(i, michigan = michigan_huron_mean_water_level["2019", i], huron = michigan_huron_mean_water_level["2019", i], erie = erie_mean_water_level["2019", i], ontario = ontario_mean_water_level["2019", i]))
}
michigan_predict_water_level <- c()
for (i in 1:12) {
    michigan_predict_water_level <- c(michigan_predict_water_level, getWaterLevel(i, superior = superior_mean_water_level["2017", i], huron = NaN, erie = erie_mean_water_level["2017", i], ontario = ontario_mean_water_level["2017", i]))
}
huron_predict_water_level <- c()
for (i in 1:12) {
    huron_predict_water_level <- c(huron_predict_water_level, getWaterLevel(i, superior = superior_mean_water_level["2019", i], michigan = NaN, erie = erie_mean_water_level["2019", i], ontario = ontario_mean_water_level["2019", i]))
}
erie_predict_water_level <- c()
for (i in 1:12) {
    erie_predict_water_level <- c(erie_predict_water_level, getWaterLevel(i, superior = superior_mean_water_level["2019", i], michigan = michigan_huron_mean_water_level["2019", i], huron = michigan_huron_mean_water_level["2019", i], ontario = ontario_mean_water_level["2019", i]))
}
ontario_predict_water_level <- c()
for (i in 1:12) {
    ontario_predict_water_level <- c(ontario_predict_water_level, getWaterLevel(i, superior = superior_mean_water_level["2020", i], michigan = michigan_huron_mean_water_level["2020", i], huron = michigan_huron_mean_water_level["2020", i], erie = erie_mean_water_level["2020", i]))
}
predict_water_level <- rbind(superior_predict_water_level, michigan_predict_water_level, huron_predict_water_level, erie_predict_water_level, ontario_predict_water_level)
colnames(predict_water_level) <- month.abb
row.names(predict_water_level) <- c("Superior", "Michigan", "Huron", "Erie", "Ontario")
write.csv(predict_water_level, "第二问/第二问预测.csv")

# 第三问
# interactivity <- read.csv("交互性/测试.csv", fileEncoding = "UTF-8-BOM")
interactivity <- read_excel("交互性/测试.xlsx")
write.csv(predict_water_level, "交互性/St.Marys+100.csv")
write.csv(predict_water_level, "交互性/St.Marys+200.csv")
write.csv(predict_water_level, "交互性/St.Lawrence+400.csv")
write.csv(predict_water_level, "交互性/St.Lawrence+800.csv")
write.csv(predict_water_level, "交互性/均温+0.5.csv")
write.csv(predict_water_level, "交互性/均温+1.csv")
write.csv(predict_water_level, "交互性/风速+0.2.csv")
write.csv(predict_water_level, "交互性/风速+0.4.csv")
write.csv(predict_water_level, "交互性/降雨+5%.csv")
write.csv(predict_water_level, "交互性/降雨+10%.csv")
