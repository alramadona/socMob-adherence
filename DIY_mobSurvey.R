#install.packages("googlesheets4")
library(googlesheets4)
library(dplyr)

sheets_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = TRUE,
  token = NULL
)

survMob <- read_sheet("https://docs.google.com/spreadsheets/d/1Wh-pHFM0qZF66ea27mWB6NwEojM2R21A-HJyNCfdS5w/edit?usp=sharing")
names(survMob) <- c("t", "DIY", 
                    "a1_mobil", "a2_taxi", "a3_motor", "a4_ojek", "a5_sepeda", "a6_bus", "a7_KA", 
                    "tujuan_jarak", "tujuan_jenis", "tujuan_point", "tujuan_orang", "pekerjaan", "usia", "sex", "t4tinggal")
survMob$jarak_km <- survMob$tujuan_jarak
survMob <- select(survMob, 
                  t,DIY,t4tinggal,
                  a1_mobil:tujuan_jarak,jarak_km,tujuan_jenis:sex)

survMob$jarak_km <- gsub("[^0-9.-]", "", survMob$jarak_km)
survMob$jarak_km <- as.numeric(survMob$jarak_km)

#str(survMob)
hist(survMob$jarak_km, 40)

survMob2 <- filter(survMob,  survMob$DIY=="Ya")
survMob2 <- filter(survMob2,  survMob2$jarak_km>0)
survMob2 <- filter(survMob2,  survMob2$jarak_km < 100)
hist(survMob2$jarak_km, 40)

survMob2 %>%
  group_by(usia) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))

sex <- survMob2 %>%
  group_by(sex) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))%>%
  arrange(desc(n))%>%
  mutate(pct = round(n / sum(n)*100,2))

usia <- survMob2 %>%
  group_by(usia) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))%>%
  arrange(desc(n))%>%
  mutate(pct = round(n / sum(n)*100,2))

usiaSex <- survMob2 %>%
  group_by(usia, sex) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))%>%
  arrange(usia, sex)%>%
  mutate(pct = round(n / sum(n)*100,2))

pekerjaan <- survMob2 %>%
  group_by(pekerjaan) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE)) %>%
  filter(n >= 4) %>%
  arrange(desc(median))%>%
  mutate(pct = round(n / sum(n)*100,2))

tujuan_point <- survMob2 %>%
  group_by(tujuan_point) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE)) %>%
  filter(n >= 4) %>%
  arrange(desc(median))%>%
  mutate(pct = round(n / sum(n)*100,2))

tujuan_orang <- survMob2 %>%
  group_by(tujuan_orang) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE)) %>%
  filter(n >= 4) %>%
  arrange(desc(median))%>%
  mutate(pct = round(n / sum(n)*100,2))

tujuan_jenis <- survMob2 %>%
  group_by(tujuan_jenis) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE)) %>%
  filter(n >= 4) %>%
  arrange(desc(median))%>%
  mutate(pct = round(n / sum(n)*100,2))

a1_mobil <- survMob2 %>%
  group_by(a1_mobil) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))%>%
  arrange(desc(n)) %>%
  mutate(pct = round(n / sum(n)*100,2))

a2_taxi <- survMob2 %>%
  group_by(a2_taxi) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))%>%
  arrange(desc(n))

a3_motor <- survMob2 %>%
  group_by(a3_motor) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))%>%
  arrange(desc(n))

a4_ojek <- survMob2 %>%
  group_by(a4_ojek) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))%>%
  arrange(desc(n))

a5_sepeda <- survMob2 %>%
  group_by(a5_sepeda) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))%>%
  arrange(desc(n))

a6_bus <- survMob2 %>%
  group_by(a6_bus) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))%>%
  arrange(desc(n))

a7_KA <- survMob2 %>%
  group_by(a7_KA) %>%
  summarize(n = n(),
            mean = mean(jarak_km, na.rm = TRUE),
            median = median(jarak_km, na.rm = TRUE))%>%
  arrange(desc(n))

survMob2 %>%
  group_by(tujuan_jenis) %>%
  summarize(n = n())  %>%
  arrange(desc(n))

survMob3 <- filter(survMob2, usia == "18 - 60")
hist(survMob3$jarak_km, 40)
hist(log(survMob3$jarak_km), 40)
