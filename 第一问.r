library("plyr")
# 数据下载
superior_use <- NA
for (i in 2012:2022) {
    data_use <- read.csv(paste0("https://waterusedata.glc.org/export_summary.php?basin=1&type=basin_total&units=2&year=", i), FALSE, row.names = 1)$V5
    if (all(is.na(superior_use))) {
        superior_use <- data_use
    } else {
        superior_use <- cbind(superior_use, data_use)
    }
}
colnames(superior_use) <- 2012:2022
michigan_use <- NA
for (i in 2012:2022) {
    data_use <- read.csv(paste0("https://waterusedata.glc.org/export_summary.php?basin=2&type=basin_total&units=2&year=", i), FALSE, row.names = 1)$V5
    if (all(is.na(michigan_use))) {
        michigan_use <- data_use
    } else {
        michigan_use <- cbind(michigan_use, data_use)
    }
}
colnames(michigan_use) <- 2012:2022
huron_use <- NA
for (i in 2012:2022) {
    data_use <- read.csv(paste0("https://waterusedata.glc.org/export_summary.php?basin=3&type=basin_total&units=2&year=", i), FALSE, row.names = 1)$V5
    if (all(is.na(huron_use))) {
        huron_use <- data_use
    } else {
        huron_use <- cbind(huron_use, data_use)
    }
}
colnames(huron_use) <- 2012:2022
erie_use <- NA
for (i in 2012:2022) {
    data_use <- read.csv(paste0("https://waterusedata.glc.org/export_summary.php?basin=4&type=basin_total&units=2&year=", i), FALSE, row.names = 1)$V5
    if (all(is.na(erie_use))) {
        erie_use <- data_use
    } else {
        erie_use <- cbind(erie_use, data_use)
    }
}
colnames(erie_use) <- 2012:2022
ontario_use <- NA
for (i in 2012:2022) {
    data_use <- read.csv(paste0("https://waterusedata.glc.org/export_summary.php?basin=5&type=basin_total&units=2&year=", i), FALSE, row.names = 1)$V5
    if (all(is.na(ontario_use))) {
        ontario_use <- data_use
    } else {
        ontario_use <- cbind(ontario_use, data_use)
    }
}
colnames(ontario_use) <- 2012:2022
# 熵权法
entropy_positive <- function(x) {
    x <- unlist(x)
    y <- (max(x) - x) / (max(x) - min(x))
    p <- y / sum(y)
    entropy <- -1 / log(length(x)) * sum(ifelse(p == 0, 0, p * log(p)))
    return(entropy)
}
superior_entropy <- colwise(entropy_positive)(as.data.frame(superior_use[rowSums(superior_use) > 0, ]))
superior_weight <- (1 - superior_entropy) / sum(1 - superior_entropy)
michigan_entropy <- colwise(entropy_positive)(as.data.frame(michigan_use[rowSums(michigan_use) > 0, ]))
michigan_weight <- (1 - michigan_entropy) / sum(1 - michigan_entropy)
huron_entropy <- colwise(entropy_positive)(as.data.frame(huron_use[rowSums(huron_use) > 0, ]))
huron_weight <- (1 - huron_entropy) / sum(1 - huron_entropy)
erie_entropy <- colwise(entropy_positive)(as.data.frame(erie_use[rowSums(erie_use) > 0, ]))
erie_weight <- (1 - erie_entropy) / sum(1 - erie_entropy)
ontario_entropy <- colwise(entropy_positive)(as.data.frame(ontario_use[rowSums(ontario_use) > 0, ]))
ontario_weight <- (1 - ontario_entropy) / sum(1 - ontario_entropy)
max.col(rbind(superior_weight, michigan_weight, huron_weight, erie_weight, ontario_weight))
write.csv(rbind(superior_weight, michigan_weight, huron_weight, erie_weight, ontario_weight), "第一问.csv", row.names = c("Superior", "Michigan", "Huron", "Erie", "Ontario"))
