library(lubridate)
library(tidyverse)
library(dplyr)

data_residency = read_csv('Data/Acoustic/residency_calc.csv')

head(data_residency)

data_residency$day <- ymd(substr(data_residency$datetime, start = 1, stop = 10))

#residen     = tapply(data_residency$day, data_residency$animal_id_pk, FUN= unique)
#day_residen = as.data.frame(n_days = sort(sapply(residen,length)))
#day_residen$animal_id = row.names(day_residen)
#write_csv(day_residen,'day_residence.csv')

#x11()
#barplot(day_residen$`sort(sapply(residen, length))`, names.arg=day_residen$animal_id, las=2,xlab = 'Animal ID', ylab= 'Days', ylim=c(0,220))
#box()

#save.image('resind_day.png',plot=last_plot)

# site fidelity - Reubens

data_residency = read_csv('Data/Acoustic/residency_calc.csv')
data_residency$hour <- hour(data_residency$datetime)
data_residency$datetime_1 <- ymd(substr(data_residency$datetime, start = 1, stop = 10))

data_residency_1 = unique(data_residency[c('datetime_1','hour','animal_id_pk')])

hour_id_an     = tapply(data_residency_1$hour, data_residency_1$animal_id_pk, FUN= length)

data_residency$datetime <- ymd_hm(data_residency$datetime)
dif_time = data_residency %>% group_by(animal_id_pk) %>% summarise(dif_time = difftime(max(datetime),min(datetime),units = 'hours'))

site_fidelity_r = as.numeric(hour_id_an)/as.numeric(dif_time$dif_time)*100
names(site_fidelity_r) = names(hour_id_an)

a = !is.infinite(site_fidelity_r)

site_fidelity_r = site_fidelity_r[!is.infinite(site_fidelity_r)]

site_fidelity_r = sort(site_fidelity_r)

write.csv(site_fidelity_r,'site_fidelity_r.csv')

x1 = barplot(as.numeric(site_fidelity_r), names.arg = names(site_fidelity_r), las=2,xlab = 'Animal ID', ylab= 'Percentage of fidelity', ylim=c(0,100))
text(x = x1, y = as.numeric(site_fidelity_r)+5, label = round(as.numeric(site_fidelity_r),1), pos = 3, cex = 0.65, col = "darkblue")
text(x = x1, y = -1, label = as.numeric(hour_id_an)[a], pos = 3, cex = 0.7, col = "darkred")
grid()
box()

# Residence  - Reubens

data_residency = read_csv('Data/Acoustic/residency_calc.csv')
data_residency$day <- ymd(substr(data_residency$datetime, start = 1, stop = 10))

data_residency_2 = unique(data_residency[c('day','animal_id_pk')])

day_id_an     = tapply(data_residency_2$day, data_residency_2$animal_id_pk, FUN= length)

#data_residency$datetime <- ymd_hm(data_residency$datetime)

dif_time = data_residency %>% group_by(animal_id_pk) %>% summarise(dif_time = difftime(max(day),min(day),units = 'days')+1)

n_ind = dif_time$dif_time >30

residence_r = as.numeric(day_id_an[n_ind])/as.numeric(dif_time$dif_time[n_ind])*100
names(residence_r) = names(day_id_an[n_ind])

#residence_r = as.numeric(day_id_an)/as.numeric(dif_time$dif_time)*100
#names(residence_r) = names(day_id_an)

residence_r = residence_r[!is.infinite(residence_r)]

residence_r = sort(residence_r)

#write.csv(residence_r,'residence_r.csv')
x11()
xx= barplot(as.numeric(residence_r), names.arg = names(residence_r), las=2,xlab = 'Animal ID', ylab= 'Percentage of residency', ylim=c(0,110))
text(x = xx, y = residence_r, label = round(residence_r,1), pos = 3, cex = 0.65, col = "darkblue")
text(x = xx, y = -1, label = as.numeric(day_id_an[n_ind]), pos = 3, cex = 0.65, col = "darkred")
grid()
box()

# Residence  - Doyle

data_residency = read_csv('Data/Acoustic/residency_calc.csv')
data_residency$day = ymd(substr(data_residency$datetime, start = 1, stop = 10))

runchecker = function(data, days){
  data %>% arrange(day) %>%
    group_by(animal_id_pk) %>%
    mutate(diff = c(0, diff(day)),
           periodID = 1 + cumsum(diff > days)) %>%
    group_by(animal_id_pk, periodID) %>%
    summarise(days = last(day) - first(day))
}

seq_day    = runchecker(data_residency,1)

seq_day_id = sort(tapply(seq_day$days, seq_day$animal_id_pk, FUN= max))

x2 = barplot(as.numeric(seq_day_id), names.arg = names(seq_day_id), las=2,xlab = 'Animal ID', ylab= 'Number of uninterrupted occupancy days', ylim=c(0,85))
text(x = x2, y = as.numeric(seq_day_id), label = as.numeric(seq_day_id), pos = 3, cex = 0.65, col = "blue")
grid()
box()





           