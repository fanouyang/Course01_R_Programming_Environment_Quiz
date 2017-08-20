library(readr)

data <- read_csv("data/daily_SPEC_2014.csv.bz2")

colnames(data) <- gsub(" +", ".", colnames(data))

#01What is average Arithmetic.Mean for “Bromine PM2.5 LC” in the state of Wisconsin 
#in this dataset?
data %>% select(State.Name, Parameter.Name, Arithmetic.Mean)%>%
         group_by(State.Name) %>%
         filter(State.Name=="Wisconsin") %>% 
         group_by(Parameter.Name) %>%
         filter(Parameter.Name=="Bromine PM2.5 LC")%>%
         summarize(mean=mean(Arithmetic.Mean,na.rm = TRUE))


#02Calculate the average of each chemical constituent across all states, 
#monitoring sites and all time points.
data %>% select(State.Name, Site.Num,Parameter.Name, Arithmetic.Mean,Date.Local)%>%
  group_by(State.Name, Site.Num, Date.Local,Parameter.Name) %>%
  summarize(mean=mean(Arithmetic.Mean,na.rm = TRUE)) %>%
  arrange(desc(mean))

#03Which monitoring site has the highest average level of “Sulfate PM2.5 LC” across all time?
data %>% filter(Parameter.Name=="Sulfate PM2.5 LC")%>%
  group_by(State.Code, County.Code,Site.Num) %>%
  summarize(mean=mean(Arithmetic.Mean,na.rm = TRUE)) %>%
  arrange(desc(mean))

#04What is the absolute difference in the average levels of “EC PM2.5 LC TOR” between 
#the states California and Arizona, across all time and all monitoring sites?
data %>% filter(State.Name %in% c("California","Arizona") & Parameter.Name == "EC PM2.5 LC TOR") %>%
  group_by(State.Name) %>%
  summarize(mean=mean(Arithmetic.Mean,na.rm = TRUE))%>%
  spread(State.Name, mean) %>%
  mutate(diff = Arizona - California)

#05What is the median level of “OC PM2.5 LC TOR” in the western United States, across all time? 
#Define western as any monitoring location that has a Longitude LESS THAN -100
data %>% filter(Parameter.Name == ("OC PM2.5 LC TOR")) %>%
  mutate(region=ifelse(Longitude < -100, "west","east"))%>%
  group_by(Parameter.Name,region)%>%
  summarize(median=median(Arithmetic.Mean,na.rm = TRUE))%>%
  spread(region, median)

library(readxl)

site <- read_excel("data/aqs_sites.xlsx")
colnames(site) <- gsub(" +", ".", colnames(site))

#06How many monitoring sites are labelled as both RESIDENTIAL for "Land Use" 
#and SUBURBAN for "Location Setting"?
site %>% filter(Land.Use=="RESIDENTIAL" &  Location.Setting=="SUBURBAN")%>%
  summarize(N=n())

with(site, table(Land.Use, Location.Setting))

#07What is the median level of “EC PM2.5 LC TOR” amongst monitoring sites that are 
#labelled as both “RESIDENTIAL” and “SUBURBAN” in the eastern U.S., 
#where eastern is defined as Longitude greater than or equal to -100?
site <- rename(site, Site.Num = Site.Number) %>%
  select(State.Code, County.Code, Site.Num, Longitude, Land.Use, 
         Location.Setting)
str(site)

subdata <- mutate(data, State.Code = as.numeric(State.Code),
               County.Code = as.numeric(County.Code),
               Site.Num = as.numeric(Site.Num)) %>%
        select(State.Code, County.Code, Site.Num, Parameter.Name, Arithmetic.Mean, Date.Local)
str(subdata) # make sure variables are in the same class

m <- left_join(subdata, site, by = c("State.Code", "County.Code", "Site.Num"))
str(m)

m %>% filter(Parameter.Name == "EC PM2.5 LC TOR" & Land.Use == "RESIDENTIAL" &  
            Location.Setting == "SUBURBAN" & Longitude >= -100) %>%
      group_by(Parameter.Name)%>%
      summarize(median=median(Arithmetic.Mean,na.rm = TRUE))

#08 Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use", 
#which month of the year has the highest average levels of "Sulfate PM2.5 LC"?

subdata %>% left_join(site,by = c("State.Code", "County.Code", "Site.Num")) %>%
  filter(Land.Use == "COMMERCIAL",
         Parameter.Name == "Sulfate PM2.5 LC") %>%
  mutate(month = lubridate::month(Date.Local,label=TRUE)) %>%
  group_by(month) %>%
  summarise(mean = mean(Arithmetic.Mean, na.rm = TRUE)) %>%
  arrange(desc(mean))

#09Take a look at the data for the monitoring site identified by State Code 6, 
#County Code 65, and Site Number 8001 (this monitor is in California). 
#At this monitor, for how many days is the sum of "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" greater than 10?
data %>% filter(State.Code == "06" & County.Code == "065" & Site.Num == "8001" &
                Parameter.Name %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC"))%>%
         group_by(Parameter.Name, Date.Local) %>%
         select(State.Code, County.Code, Site.Num, Date.Local, Parameter.Name,
                Arithmetic.Mean) %>%
         summarise(mean = mean(Arithmetic.Mean, na.rm = TRUE)) %>%
         group_by(Date.Local) %>%
         summarise(Total = sum(mean, na.rm = TRUE)) %>%
         filter(Total > 10)

#10Which monitoring site in the dataset has the highest correlation between 
#"Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" across all dates? Identify 
#the monitoring site by it's State, County, and Site Number code.
data %>%
  filter(Parameter.Name %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")) %>%
  group_by(State.Code, County.Code, Site.Num, Parameter.Name, Date.Local) %>%
  select(State.Code, County.Code, Site.Num, Date.Local, Parameter.Name,
         Arithmetic.Mean) %>%
  summarise(mean = mean(Arithmetic.Mean, na.rm = TRUE)) %>%
  spread(Parameter.Name, mean) %>%
  group_by(State.Code, County.Code, Site.Num) %>%
  summarise(correlation = cor(`Sulfate PM2.5 LC`, `Total Nitrate PM2.5 LC`)) %>%
  arrange(desc(correlation))
 