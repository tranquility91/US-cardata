library(tidyverse)
install.packages("carData")
library(carData)
#part 1
indicators=carData::UN98%>%select(-region)
ind.m=list()
for(i in 1:ncol(indicators)){
    ind.m[i]=mean(UN98[,i+1],na.rm=T)  
}
ind.m <- indicators %>% 
    map_dbl(~mean(., na.rm = TRUE))

#part 2
library(readxl)
pop <- list()
for (i in 2012:2017) {  
    j <- i-2011
    pop[[j]] <- read.csv(paste0("C:/Users/USER/Documents/R/us_percent_age_data_", i, ".csv"), sep = ",", skip = 2, nrows = 53) %>%
        select(-Total,-Footnotes) %>%
        mutate(year = i)
}

pop_merged <- bind_rows(pop)

ggplot(data = pop_merged, aes(x = year, y = X65.)) +
    geom_line() +
    facet_wrap(~Location, ncol = 6) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("pop_by_location.jpg", width = 10, height = 8)