library(mgcv)

d <- read.csv2(paste0("data/data_clean_tot.csv"), na.strings = "")
info_especes <- read.csv2("library/info_especes.csv", na.strings = "")
info_especes <- dplyr::select(info_especes, -c("NAME_SPECIES"))
d <- merge(d, info_especes)

dates <- read.csv2("library/liste_comptages.csv", na.strings = "")
dates$DATE <- as.character(as.Date.character(dates$DATE, "%d/%m/%Y"))
d$DATE <- as.character(as.Date.character(d$DATE, "%d/%m/%Y"))

nat <- c("Limosa lapponica", "Calidris canutus", "Calidris minuta", "Anas clypeata", "Tringa nebularia", "Tringa erythropus",
         "Tringa totanus", "Actitis hypoleucos", "Numenius arquata", "Numenius phaeopus", "Pluvialis squatarola", "Calidris alpina")
internat <- c("Recurvirostra avosetta", "Limosa limosa", "Anas acuta", "Charadrius hiaticula", "Platalea leucorodia", "Calidris alpina")

species <- c("Anas crecca", "Anas platyrhynchos", "Egretta garzetta", "Fulica atra", "Numenius arquata", "Calidris alpina", "Tringa totanus",
         "Limosa limosa", "Pluvialis squatarola", "Recurvirostra avosetta")


d <- subset(d, d$COMPTE == "TRUE" & (d$SP %in% nat | d$SP %in% internat) & (d$DATE %in% dates$DATE | d$YEAR <= 2006))
dcompt <- aggregate(TOTAL_COUNT ~ DATE + MONTH + YEAR + SP, data = d, FUN = sum)

species <- sort(unique(dcompt$SP))
s = "Tringa totanus"

dcalalp <- subset(dcompt, dcompt$SP == s)
gg <- dcalalp %>% filter(YEAR >= 2016) %>%
  ggplot(aes(x = DATE, y = TOTAL_COUNT)) +
  geom_line()
#geom_point(aes(y = TOTAL_COUNT)) +
#geom_ribbon(alpha = 0.3, fill = 'royalblue1')
print(gg)

dcalalp$NDAY <- yday(dcalalp$DATE)
gg <- dcalalp %>% filter(YEAR >= 2012) %>%
  ggplot(aes(x = NDAY, y = TOTAL_COUNT)) +
  #geom_line() +
  geom_line(aes(y = TOTAL_COUNT, color = YEAR, group = YEAR)) +
  labs(title = "Résultats des comptages de Tringa totanus entre 2012 et 2021", y = "Abondance", x = "Quantième annuel")
#geom_ribbon(alpha = 0.3, fill = 'royalblue1')
print(gg)


gam1 <- gam(TOTAL_COUNT~s(NDAY), family = quasipoisson(), data = dcalalp, method = 'REML')
summary(gam1)
#plot(gam1, residuals = TRUE, pch = 1, se = T)

gg <- ggplot(dcalalp, aes(x = NDAY, y = TOTAL_COUNT)) +
  geom_point() +
  geom_line(aes(y = fitted(gam1)), colour = "blue", size = 1.2)
gg

p <- predict(gam1, dcalalp, type = "link", se.fit = TRUE)

upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)

upr <- gam1$family$linkinv(upr)
lwr <- gam1$family$linkinv(lwr)

gg <- ggplot(dcalalp, aes(x = NDAY, y = TOTAL_COUNT)) +
  geom_point() +
  geom_line(aes(y = fitted(gam1)), colour = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = 'blue', alpha = 0.25)
gg


plot(gam1, se = T)

linear_model <- gam(TOTAL_COUNT~NDAY, data = dcalalp)
smooth_model <- gam(TOTAL_COUNT~s(NDAY), data = dcalalp)
AIC(linear_model, smooth_model)




species <- sort(unique(c(nat, internat)))

dcompt <- subset(d, d$SP %in% species & (d$DATE %in% dates$DATE | (d$YEAR >= 1990 & d$YEAR <= 2009)))
dcompt$DATE <- as.Date.character(dcompt$DATE, format = "%Y-%m-%d")
dcompt <- aggregate(TOTAL_COUNT ~ DATE + MONTH + YEAR + SP, data = dcompt, FUN = sum)
dcompt <- complete(dcompt, DATE, SP)
dcompt$TOTAL_COUNT[is.na(dcompt$TOTAL_COUNT)] <- 0
dcompt$YEAR <- year(dcompt$DATE)
dcompt$MONTH <- month(dcompt$DATE)
dcompt$GRP <- ifelse(dcompt$YEAR <= 2007, 'pré-2007', 'post-2007')

for(s in species){
  ds <- subset(dcompt, dcompt$SP == s)
  
  #gg <- ds %>%
  #  ggplot(aes(x = DATE, y = TOTAL_COUNT)) +
  #  geom_line(aes(color = GRP)) +
  #  labs(title = s)
  #  #geom_point(aes(y = TOTAL_COUNT)) +
  #  #geom_ribbon(alpha = 0.3, fill = 'royalblue1')
  #print(gg)
  
  ds$NDAY <- yday(ds$DATE)
  #gg <- ds %>% filter(YEAR >= 2009) %>%
  #  ggplot(aes(x = NDAY, y = TOTAL_COUNT)) +
  #  #geom_line() +
  #  geom_line(aes(y = TOTAL_COUNT, color = YEAR, group = YEAR)) +
  #  labs(title = s)
  ##geom_ribbon(alpha = 0.3, fill = 'royalblue1')
  #print(gg)
    
  #gam1 <- gam(TOTAL_COUNT~s(NDAY), family = quasipoisson(), data = filter(ds, YEAR <= 2006), method = 'REML')
  #summary(gam1)
  gam2 <- gam(TOTAL_COUNT~s(NDAY), family = quasipoisson(), data = filter(ds, YEAR >= 2012), method = 'REML')
  summary(gam2)
  
  #p1 <- predict(gam1, filter(ds, YEAR <= 2006), type = "link", se.fit = TRUE)
    
  #upr1 <- p1$fit + (2 * p1$se.fit)
  #lwr1 <- p1$fit - (2 * p1$se.fit)
    
  #upr1 <- gam1$family$linkinv(upr1)
  #lwr1 <- gam1$family$linkinv(lwr1)
  
  p2 <- predict(gam2, filter(ds, YEAR >= 2012), type = "link", se.fit = TRUE, unconditional = TRUE)
  
  upr2 <- p2$fit + (2 * p2$se.fit)
  lwr2 <- p2$fit - (2 * p2$se.fit)
  
  upr2 <- gam2$family$linkinv(upr2)
  lwr2 <- gam2$family$linkinv(lwr2)
    
  gg <- ggplot(filter(ds, YEAR >=2012), aes(x = NDAY, y = TOTAL_COUNT)) +
    geom_point() +
    #geom_line(data = filter(ds, YEAR <= 2006), aes(y = fitted(gam1)), colour = "blue", size = 1.2) +
    #geom_ribbon(data = filter(ds, YEAR <= 2006), aes(ymin = lwr1, ymax = upr1), fill = 'blue', alpha = 0.25)
    geom_line(data = filter(ds, YEAR >= 2012), aes(y = fitted(gam2)), colour = "red", size = 1.2) +
    geom_ribbon(data = filter(ds, YEAR >= 2012), aes(ymin = lwr2, ymax = upr2), fill = 'red', alpha = 0.25) +
    labs(title = paste0("Phénologie saisonnière de l'espèce ", s), y = "Abondance", x = "Quantième annuel")
  print(gg)
  
  ggsave(paste0("output/Tendances/gam_", gsub(" ", "_", s), ".png"))
}




modlin_1 <- lm(TOTAL_COUNT ~ NDAY + YEAR, data = dcalalp)
summary(modlin_1)
confint(modlin_1, level = 0.95)

predict(modlin_1)

res_df <- data.frame(NDAY = dcalalp$NDAY,
                     residus_lm = residuals(modlin_1),
                     residus_calcul = dcalalp$TOTAL_COUNT - predict(modlin_1))

ggplot(res_df, aes(x = NDAY, y = residus_lm)) +
  geom_point() +
  labs(x = "Num du jour", y = "Résidus") +
  geom_hline(yintercept = 0, col = "red", size = 1)

ggplot(res_df, aes(x = residus_lm)) +
  geom_histogram(binwidth = 2, color = "blue") +
  labs(x = "Residual")

shapiro.test(res_df$residus_lm[1:5000])


modglm_1 <- glm(TOTAL_COUNT ~ NDAY, dcalalp, family = stats::quasipoisson(link="log"))
summary(modglm_1)


library(forecast)
library(cowplot)

dcalalp <- dcalalp %>% ungroup() %>% subset(dcalalp$YEAR >= 1982) %>%
  complete(DATE = seq(as.Date("1982-01-01"), as.Date("2021-12-31"), by = "1 day"))

calalp_ts <- ts(dcalalp %>% dplyr::select(-DATE),
              start = c(dcalalp$DATE[1] %>% year(), 1),
              frequency = 365.25)
autoplot(calalp_ts, facets = TRUE) +
  scale_x_continuous(breaks = 1982:2021)


calalp_monthly <- dcalalp %>%
  mutate(YearMonth = ymd(paste0(YEAR, "-", MONTH, "-01"))) %>%
  group_by(YEAR, YearMonth) %>%
  dplyr::summarise(TOTAL_COUNT = mean(TOTAL_COUNT, na.rm = TRUE)) # moyenne
calalp_monthly <- calalp_monthly %>% ungroup() %>% subset(calalp_monthly$YEAR >= 1982) %>% dplyr::select(-YEAR) %>%
                complete(YearMonth = seq(as.Date("1982-01-01"), as.Date("2021-12-01"), by = "1 month"))
calalp_monthly_ts <- ts(calalp_monthly %>% ungroup() %>%
                            dplyr::select(TOTAL_COUNT), start = c(1982, 1),
                            frequency = 12)
autoplot(calalp_monthly_ts, facets = TRUE) +
  scale_x_continuous(breaks = 1982:2021)

gg <- ggplot(data = dcalalp, aes(x = DATE, y = TOTAL_COUNT)) +
  geom_line(aes(x = YearMonth, y = TOTAL_COUNT), data = calalp_monthly) +
  geom_point()
print(gg)

plot_grid(autoplot(diff(calalp_monthly_ts)) + ggtitle("Restauration: Série temporelle"),
          ggAcf(diff(calalp_monthly_ts)) + ggtitle("Restauration: Autocorrélation"),
          gglagplot(diff(calalp_monthly_ts)) + ggtitle("Restauration: Lag plot"),
          ncol = 3)
Box.test(diff(calalp_monthly_ts), lag = 16, type = "Ljung-Box")

cm_ts <- calalp_monthly_ts[, 1]
cm_ts_train <- window(cm_ts, start = 1982, end = 2009.999)
cm_ts_test <- window(cm_ts, start = 2010)

hm_naive <- snaive(cm_ts_train, h = 120)
autoplot(hm_naive) +
  autolayer(fitted(hm_naive)) +
  autolayer(cm_ts_test, color = rgb(0, 0, 0, 0.6)) +
  labs(x = "Année", y = "")

checkresiduals(hm_naive)

forecast::accuracy(hm_naive, cm_ts)

loti_ses <- ses(cm_ts_train, h = 120, alpha = 0.5)
autoplot(loti_ses) + autolayer(fitted(loti_ses))

loti_holt_dF <- holt(cm_ts_train, damped = FALSE, h = 100)
loti_holt_dT <- holt(cm_ts_train, damped = TRUE, h = 100)
plot_grid(autoplot(loti_holt_dF), autoplot(loti_holt_dT))

flow_hw <- hw(cm_ts_train, damped = TRUE, h = 12*3, seasonal = "additive")
autoplot(flow_hw) + autolayer(fitted(flow_hw))


ggA <- ggseasonplot(window(calalp_monthly_ts[, "TOTAL_COUNT"], 2010, 2021-1/365.25)) + ggtitle("")
ggB <- ggseasonplot(window(calalp_monthly_ts[, "TOTAL_COUNT"], 2010, 2021-1/365.25), polar = TRUE) + ggtitle("")
ggC <- ggsubseriesplot(window(calalp_monthly_ts[, "TOTAL_COUNT"], 2010, 2021-1/365.25), polar = TRUE) + ggtitle("") + labs(y="Flow")
plot_grid(ggA, ggB, ggC, ncol = 3, labels = c("A", "B", "C"))


bruit_blanc <- ts(runif(114, 0, 6000), start = c(1982, 1), frequency = 12)

plot_grid(autoplot(calalp_monthly_ts) + ggtitle("Calidris alpina : série calalporelle"),
          ggAcf(calalp_monthly_ts) + ggtitle("Calidris alpina : Autocorrélation"),
          gglagplot(calalp_monthly_ts, do.lines = FALSE) + ggtitle("Calidris alpina : Lag plot"),
          autoplot(bruit_blanc) + ggtitle("Bruit blanc : Série calalporelle"),
          ggAcf(bruit_blanc) + ggtitle("Bruit blanc : Autocorrélation"),
          gglagplot(bruit_blanc) + ggtitle("Bruit blanc : Lag plot"),
          ncol = 3)

ggAcf(calalp_monthly_ts, ci = 0.95) + ggtitle("Calidris alpina : Autocorrélation")
Box.test(calalp_monthly_ts, lag = 24, type = "Ljung-Box")




library("tidyverse") # évidemment
library("tidymodels")
library("caret")


mod <- lm(calalp_monthly$TOTAL_COUNT~1)
mod$residuals
par(mfrow=c(1,2))
hist(mod$residuals, main="residuals")
hist(mod$residuals, main="residuals")
