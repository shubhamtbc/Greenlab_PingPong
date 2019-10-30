
# list of power consumption values for low quality youtube 
# copy pasted from python output
#

you_low<-c(  12.89,
             19.16,
             8.41,
             8.6,
             11.9,
             7.87,
             11.99,
             45.27,
             38.66,
             8.5,
             38.73,
             156.22,
             13.3,
             12.69,
             11.23,
             24.23,
             7.98,
             28.5,
             12.89,
             164.19,
             9.5,
             5.51,
             134.34,
             117.26,
             9.48,
             8.46,
             13.1,
             8.31,
             13.81,
             8.3,
             268.49,
             27.86,
             8.6,
             13.58,
             8.16,
             57.77,
             239.89,
             41.81,
             23.35,
             7.86,
             17.18,
             187.98,
             8.24,
             46.77,
             8.96,
             13.93,
             8.32,
             12.89,
             8.45,
             13.46)

you_high<-c(76.88,
            101.91,
            39.22,
            89.88,
            475.81,
            39.09,
            8.73,
            8.47,
            35.1,
            56.36,
            60.42,
            9.63,
            78.62,
            22.55,
            12.47,
            58.19,
            12.79,
            21.73,
            10.46,
            49.54,
            23.62,
            43.21,
            12.94,
            8.19,
            8.29,
            57.17,
            10.09,
            16.68,
            8.97,
            14.61,
            23.66,
            207.44,
            40.9,
            185.62,
            47.95,
            8.39,
            10.85,
            164.9,
            8.12,
            32.92,
            107.1,
            8.6,
            14.55,
            10.75,
            17.15,
            27.84,
            284.94,
            8.85,
            82.58,
            167.28)
# list of power consumption values for low quality spotify 
spot_low<-c(8.65,
            8.04,
            8.04,
            8.01,
            8.21,
            469.23,
            8.33,
            8.26,
            8.29,
            8.32,
            8.04,
            8.38,
            8.05,
            7.9,
            8.04,
            8.53,
            7.99,
            11.66,
            10.27,
            7.8,
            7.64,
            7.75,
            8.16,
            8.38,
            8.22,
            8.04,
            8.39,
            8.04,
            8.04,
            8.07,
            8.74,
            8.04,
            8.04,
            7.69,
            8.04,
            7.85,
            8.69,
            8.12,
            8.46,
            8.03,
            8.26,
            8.25,
            7.95,
            8.04,
            8.01,
            8.04,
            8.04,
            8.61,
            7.8,
            8.38)

spot_high<-c(3.85,
             7.63,
             11.46,
             7.93,
             8.49,
             25.43,
             7.69,
             10.34,
             7.8,
             8.16,
             8.1,
             7.99,
             7.63,
             8.27,
             7.84,
             8.18,
             7.48,
             7.62,
             7.44,
             8.29,
             7.93,
             11.79,
             7.9,
             7.99,
             7.77,
             8.82,
             7.8,
             12.38,
             8.13,
             7.89,
             7.43,
             7.46,
             208.56,
             8.54,
             7.76,
             7.89,
             7.81,
             7.66,
             7.65,
             8.25,
             11.13,
             7.91,
             8.31,
             8.1,
             7.95,
             7.93,
             8.22,
             8.2,
             8.77,
             7.8)
total_low<-c(you_low_kws, spot_high_kws)
#kilowatt-sec: multiply by time and divide by 1000
you_low_kws<-(you_low*220)/1000
qqnorm(you_low_kws, main = "LQ youtube")
qqline(you_low_kws)

you_high_kws<-(you_high*220)/1000
qqnorm(you_high_kws, main = "HQ youtube")
qqline(you_high_kws)

spot_low_kws<-(spot_low*210)/1000
qqnorm(spot_low_kws, main = "LQ spotify")
qqline(spot_low_kws)

spot_high_kws<-(spot_high*210)/1000
qqnorm(spot_high_kws, main = "HQ spotify")
qqline(spot_high_kws)

#combining LQ data and HQ data separately
total_low<-c(you_low_kws, spot_low_kws)
total_high<-c(you_high_kws, spot_high_kws)

qqnorm(total_low, main = "LQ energy")
qqline(total_low)

qqnorm(total_high, main = "HQ energy")
qqline(total_high)

# null hypothesis: sample is normally distributed
shapiro.test(you_low_kws) 
shapiro.test(you_high_kws)
shapiro.test(spot_low_kws)
shapiro.test(spot_high_kws)

shapiro.test(total_low)
shapiro.test(total_high)

# transformation of the data
you_low_kws_log<-log(you_low_kws)
qqnorm(you_low_kws_log)
qqline(you_low_kws_log)

you_high_kws_log<-log(you_high_kws)
qqnorm(you_high_kws_log)
qqline(you_high_kws_log)

spot_low_kws_log<-log(spot_low_kws)
qqnorm(spot_low_kws_log)
qqline(spot_low_kws_log)

total_low_log<-log(total_low)
qqnorm(total_low_log, main = "Log transformation on LQ data")
qqline(total_low_log)

total_high_log<-log(total_high)
qqnorm(total_high_log, main = "Log transformation on HQ data")
qqline(total_high_log)

# null hypothesis: sample is normally distributed
shapiro.test(you_low_kws_log) 
shapiro.test(you_high_kws_log)
shapiro.test(spot_low_kws_log)

shapiro.test(total_high_log)
shapiro.test(total_low_log)


#hstograms
hist(you_low_kws, col = "red", main = "Energy of LQ stream on youtube", 
     xlab = "Energy in kWsec")
hist(you_high_kws, col = "blue", main = "Energy of HQ stream on youtube", 
     xlab = "Energy in kWsec")
hist(spot_high_kws, col = "blue", main = "Energy of HQ stream on spotify", 
     xlab = "Energy in kWsec")
hist(spot_low_kws, col = "red", main = "Energy of LQ stream on spotify", 
     xlab = "Energy in kWsec")

hist(total_high, col = "blue", main = "Energy of HQ stream", 
     xlab = "Energy in kWsec")
hist(total_low, col = "red", main = "Energy of LQ stream", 
     xlab = "Energy in kWsec")

hist(total_high_log, col = "blue", main = "log(Energy of HQ stream)", 
     xlab = "Energy in kWsec")
hist(total_low_log, col = "red", main = "log(Energy of LQ stream)", 
     xlab = "Energy in kWsec")



#boxplots
boxplot(you_low_kws, col = "blue", main = "Energy of LQ stream on youtube", 
     xlab = "Energy in kWsec", horizontal = TRUE)
boxplot(you_high_kws, col = "red", main = "Energy of HQ stream on youtube", 
     xlab = "Energy in kWsec", horizontal = TRUE)
boxplot(spot_low_kws, col = "blue", main = "Energy of LQ stream on spotify", 
     xlab = "Energy in kWsec", horizontal = TRUE)

boxplot(total_high, col = "blue", main = "Energy of HQ", 
        xlab = "Energy in kWsec", horizontal = TRUE)
boxplot(total_low, col = "red", main = "Energy of LQ", 
        xlab = "Energy in kWsec", horizontal = TRUE)


# null hypothesis: the medians are not different
wilcox.test(total_high, total_low, paired = TRUE)

wilcox.test(spot_high_kws, spot_low_kws, paired = TRUE)

wilcox.test(you_high_kws, you_low_kws, paired = TRUE)
