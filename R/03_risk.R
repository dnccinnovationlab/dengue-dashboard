###reading mosquito and larval data

map <- st_read("DNCC_Ward_Boundary_poly_urp_f.shp", quiet = TRUE) %>%
  st_transform(4326)

##adding the zones and wards to the shape file and save it
zn_ward<-read_sheet("164-CIeR5mSutgIUPVS2EteQy1lGMSEzFhHj7iCLqzqg",sheet="zone_ward_area_list")

map_filtered<-map%>%
mutate(wardfc = paste0("Ward No-",Ward_no))%>%
mutate(ward = as.numeric(Ward_no))%>%
left_join(.,zn_ward,by="ward")

data_mosquito <- read_sheet("164-CIeR5mSutgIUPVS2EteQy1lGMSEzFhHj7iCLqzqg",sheet="ju_adult_mosquito")%>%
group_by(zone,week_ju)%>%
summarise(Total_mosquito=sum(Ae.aegypti,na.rm=TRUE))

data_larvae <- read_sheet("164-CIeR5mSutgIUPVS2EteQy1lGMSEzFhHj7iCLqzqg",sheet="ju_bi_ci")%>%
dplyr::select(zone,week_ju,Total_House,Positive_House,Total_Wet_container,Positive_Wet_Container)

###joining the dates

##creating a dataframe with date and month and week
from=as.Date("05/24/2024",format="%m/%d/%Y")
to=as.Date(end_date,format="%Y-%m-%d")+14
date2<-data.frame(Date=seq(from,to,by="days"))%>%
mutate(week_ju = cut(Date, breaks = "7 days", labels = FALSE))

from <- as.Date("01/01/2024", format = "%m/%d/%Y")
to  <-as.Date(end_date,format="%Y-%m-%d")+14

date1 <- data.frame(Date = seq(from, to, by = "days")) %>%
  mutate(
    month    = format(Date, "%B"),
    year = isoyear(Date),
    week = isoweek(Date)
  ) %>%
  mutate(min_date = floor_date(Date, unit = "week", week_start = 1),
         max_date = min_date + days(6),
         date_range = paste0(
                            "from ", format(min_date, "%d-%b-%y"),
                            " to ", format(max_date, "%d-%b-%y"))
         ) %>%
  as.data.frame()%>%
left_join(date2,by="Date")

###removing the rows with duplicated week number due to month

date2<-date1[!duplicated(date1[c('year', 'week')]),]


##joining mosquito and larvae data

data_mosq_larv<-data_mosquito%>%
full_join(.,data_larvae,by=c("zone","week_ju"))%>%
#mutate(week_ju_1=week_ju-1)%>%## to match with the week
left_join(.,date2,by="week_ju",relationship ="many-to-many")%>%
dplyr::select(zone,week,week_ju,date_range,month,year,Total_mosquito,Total_House,Positive_House,Total_Wet_container,Positive_Wet_Container)


##joining all data and calculating risk 

##summarising death data
zone_death<-readr::read_csv("https://docs.google.com/spreadsheets/d/1EMva389XPbDyJs80M3FZhgj8hGNCOouWzo4bz0cr0Ww/export?format=csv&gid=2133167595")%>% ##export csv reading is fast
mutate(Date=as.Date(Date,format="%Y-%m-%d"))%>%
mutate(Total_death=as.numeric(Total_death))%>%
group_by(Date,week,date_range,month,year,zone,ward,wardfc,area)%>%
summarise(Total_death=sum(Total_death))%>%
arrange(Date)

##week_ju cannot taken now as it will create problem, he will add this after summarising and joining with patient data

##summarising patient data

zone_patient<-readr::read_csv("https://docs.google.com/spreadsheets/d/1EMva389XPbDyJs80M3FZhgj8hGNCOouWzo4bz0cr0Ww/export?format=csv&gid=1472330177")%>%
#mutate(Date=mdy(Date))%>%
mutate(Date=as.Date(Date,format="%Y-%m-%d"))%>%
mutate(Total_patient=as.numeric(Total_patient))%>%
group_by(Date,week,date_range,month,year,zone,ward,wardfc,area)%>%
summarise(Total_patient=sum(Total_patient))%>%
arrange(Date)

map <- list.files(path = "DNCC_Ward_Boundary_poly_urp_f.shp") %>%
  lapply(st_read) %>%
  purrr::reduce(st_combine) %>% #for multiple map areas
  st_transform(crs = 4326)  #projects to WGS84


##extracting the centroid lat long of the wards
map_filtered <- map %>%
  mutate(centroid=st_centroid(geometry))%>%
  tidyr::extract(centroid, c('lat', 'long'), '\\(([^,]+),\\s*([^\\)]+)\\)')%>%
  mutate(zone=as.numeric(Zone_no)) %>% 
  mutate(wardfc = paste0("Ward No-",Ward_no))%>%
  mutate(ward = as.numeric(Ward_no))%>%
  dplyr::select(zone,wardfc,area_sqkm,lat,long)%>%  
  st_drop_geometry()

##creating compund growth for popualtion in the zone ward data for each year
current_year<-max(year(end_date))
growth_rate <- 0.0379   # 3.79%
years_to_create <- 2022:current_year

###to add the population of the wards
zn_ward <- read_sheet("164-CIeR5mSutgIUPVS2EteQy1lGMSEzFhHj7iCLqzqg",sheet="zone_ward_area_list")%>%
slice(rep(1:n(), each = length(years_to_create))) %>%
  group_by(zone, zone_bn, ward, ward_bn, area_bn, area) %>%   # adjust if your grouping columns differ
  mutate(
    year = years_to_create,
    population = round(
      population * (1 + growth_rate)^(year - 2022)
    )
  ) %>%
  ungroup()

##joining patient and death data
zonal_patient_death<-zone_patient%>%
mutate(zone=as.numeric(zone))%>%
mutate(ward=as.numeric(ward))%>%
mutate(week=as.numeric(week))%>%
mutate(year=as.numeric(year))%>%
full_join(.,zone_death,by=c("Date","month","week","date_range","year","zone","ward","wardfc","area"),suffix=c("",".death"))%>%
arrange(Date)%>%
left_join(.,map_filtered,by=c("zone","wardfc"))%>%
left_join(.,zn_ward,by=c("year","zone","ward"))%>%
mutate(area=area.y)%>%
mutate(Total_patient=ifelse(is.na(Total_patient)==TRUE,0,Total_patient))%>%
mutate(Total_death=ifelse(is.na(Total_death)==TRUE,0,Total_death))%>%
dplyr::select(Date,week,date_range,month,year,zone,zone_bn,ward,ward_bn,wardfc,area,area_bn,area_sqkm,Total_patient,Total_death,lat,long,population)

##aggregate patient and death data by week
zonal_patient_death_week<-zonal_patient_death%>%
group_by(year,week,zone,ward)%>%
summarise_at(c("Total_patient","Total_death"),sum,na.rm=TRUE)%>%
arrange(year,week)

##joining larvae and mosquito data with patient and death data

zonal_ward_patient_death_lar_mosq_week<-data_mosq_larv%>%
full_join(.,zonal_patient_death_week,by=c("year","week","zone"),suffix=c(".larvae_mosq",".patient_death"))%>%
left_join(.,date2,by=c("year","week"))%>%
left_join(.,zn_ward,by=c("year","zone","ward"))%>%
mutate(wardf=formatC(ward,width=2,flag="0"))%>%
mutate(wardfc = paste0("Ward No-",wardf))%>%
arrange(Date)%>%
left_join(.,map_filtered,by="wardfc")%>%
mutate(month=month.y,
       week_ju=week_ju.y,
       date_range=date_range.y,
       zone=zone.y)%>%
mutate(population_density=population/area_sqkm)%>%
mutate(population_density=ifelse(is.na(population_density)==TRUE,0,population_density))%>%
mutate(Total_patient=ifelse(is.na(Total_patient)==TRUE,0,Total_patient))%>%
mutate(Total_death=ifelse(is.na(Total_death)==TRUE,0,Total_death))%>%
dplyr::select(Date,year,month,week,week_ju,date_range,zone,ward,area,wardfc,population,population_density,Total_patient,Total_death,Total_mosquito,Total_House,Positive_House,Total_Wet_container,Positive_Wet_Container,lat,long)

##computing the risk
##Sort and compute 3-week rolling sums
df <- zonal_ward_patient_death_lar_mosq_week %>%
  mutate(Total_mosquito=ifelse(is.na(Total_mosquito)==TRUE & zone<=5,0,Total_mosquito),
         Positive_Wet_Container=ifelse(is.na(Positive_Wet_Container)==TRUE & zone<=5,0,Positive_Wet_Container),
         Positive_House=ifelse(is.na(Positive_House)==TRUE & zone<=5,0,Positive_House),
         Total_Wet_container=ifelse(is.na(Total_Wet_container)==TRUE & zone<=5,0,Total_Wet_container),
         Total_House=ifelse(is.na(Total_House)==TRUE & zone<=5,0,Total_House)
        )%>%
  arrange(ward, year, week) %>%
  group_by(ward) %>%
  mutate(
    roll_total_patients = rollapply(Total_patient, width = 3, FUN = sum, align = "right", fill = NA, partial = TRUE),
    roll_total_deaths = rollapply(Total_death, width = 3, FUN = sum, align = "right", fill = NA, partial = TRUE),
    roll_mosquito = rollapply(Total_mosquito, width = 3, FUN = sum, align = "right", fill = NA, partial = TRUE),
    roll_pos_wet_cont = rollapply(Positive_Wet_Container, width = 3, FUN = sum, align = "right", fill = NA, partial = TRUE),
    roll_pos_house = rollapply(Positive_House, width = 3, FUN = sum, align = "right", fill = NA, partial = TRUE),
    roll_tot_wet_cont = rollapply(Total_Wet_container, width = 3, FUN = sum, align = "right", fill = NA, partial = TRUE),
    roll_tot_house = rollapply(Total_House, width = 3, FUN = sum, align = "right", fill = NA, partial = TRUE)
   ) %>%
  mutate(roll_avg_mosq_per_trap=roll_mosquito/2,
         roll_bi=roll_pos_wet_cont/roll_tot_house,
         roll_ci=roll_pos_wet_cont/roll_tot_wet_cont,
         roll_hi=roll_pos_house/roll_tot_house)%>%
  mutate(roll_total_patients=ifelse(year>2023 & is.na(ward)==TRUE,0,roll_total_patients),
         roll_total_deaths=ifelse(year>2023 & is.na(ward)==TRUE,0,roll_total_deaths))%>%
ungroup()%>%
arrange(Date)



# Step 3: Compute min/max values (excluding 0 and NA) for normalization
# Get all years in the dataset
all_years <- sort(unique(df$year))

# Filter only rows where ward is NA and year > 2023
df_filtered <- df %>%
  filter(!(is.na(ward) & year > 2023))  # keep 2022 and 2023 even if NA in indices

# Summarize per year
non_zero_values <- df_filtered %>%
mutate(month= factor(month, 
                        levels = month.name, 
                        ordered = TRUE))%>%
  group_by(year,week) %>%
  summarise(
    min_patients = min(roll_total_patients, na.rm = TRUE),
    max_patients = max(roll_total_patients, na.rm = TRUE),

    min_deaths = min(roll_total_deaths, na.rm = TRUE),
    max_deaths = max(roll_total_deaths, na.rm = TRUE),

    min_mosquito = if (all(is.na(roll_avg_mosq_per_trap))) NA else min(roll_avg_mosq_per_trap, na.rm = TRUE),
    max_mosquito = if (all(is.na(roll_avg_mosq_per_trap))) NA else max(roll_avg_mosq_per_trap, na.rm = TRUE),

    min_BI = if (all(is.na(roll_bi))) NA else min(roll_bi, na.rm = TRUE),
    max_BI = if (all(is.na(roll_bi))) NA else max(roll_bi, na.rm = TRUE),

    min_CI = if (all(is.na(roll_ci))) NA else min(roll_ci, na.rm = TRUE),
    max_CI = if (all(is.na(roll_ci))) NA else max(roll_ci, na.rm = TRUE),

    min_HI = if (all(is.na(roll_hi))) NA else min(roll_hi, na.rm = TRUE),
    max_HI = if (all(is.na(roll_hi))) NA else max(roll_hi, na.rm = TRUE),

    min_pop_den = min(population_density, na.rm = TRUE),
    max_pop_den = max(population_density, na.rm = TRUE)
  ) %>%
  ungroup()

	
# Step 4: Normalize each risk indicator
df <- df %>%
left_join(.,non_zero_values,by=c("year","week"))%>%
group_by(year,week)%>%
  mutate(
    risk_patients = ifelse((max_patients - min_patients)==0,0,(roll_total_patients - min_patients) / (max_patients - min_patients)),
    risk_deaths = ifelse((max_deaths - min_deaths)==0,0,(roll_total_deaths - min_deaths) / (max_deaths - min_deaths)),
    risk_adult_mosquito = ifelse((max_mosquito - min_mosquito)==0,0,(roll_avg_mosq_per_trap - min_mosquito) / (max_mosquito - min_mosquito)),
    risk_bi = ifelse((max_BI - min_BI)==0,0,(roll_bi - min_BI) / (max_BI - min_BI)),
    risk_ci = ifelse((max_CI - min_CI)==0,0,(roll_ci - min_CI) / (max_CI - min_CI)),
    risk_hi =  ifelse((max_HI - min_HI)==0,0,(roll_hi - min_HI) / (max_HI - min_HI)),
    risk_pop_den =  ifelse(year<2024,(population_density/max_pop_den),(population_density - min_pop_den) / (max_pop_den - min_pop_den)))%>%
ungroup()


# Compute composite risk (weighted average of available risk values)
df <- df %>%
  rowwise() %>%
  mutate(
    composite_risk = ifelse(is.na(risk_bi)==TRUE,(0.6)*risk_patients+(0.3)*risk_deaths+(0.1)*risk_pop_den,
                     (0.4)*risk_patients+(0.3)*risk_deaths+(0.05)*risk_adult_mosquito+(0.05)*risk_bi+(0.05)*risk_ci+(0.05)*risk_hi+0.1*risk_pop_den))%>%
  mutate(breeding_risk = (1/3)*risk_bi+(1/3)*risk_ci+(1/3)*risk_hi) %>%  
ungroup()

# Step 5: Compute each year 3 equal spaced cutoffs for each indicator

cut_with_zero_category <- function(x) {
  if (length(x) == 0) {
    return(factor(rep("Not Available", length(x)),
                  levels = c("Zero", "Low", "Moderate", "High", "Not Available")))
  }
  # Calculate cut width excluding zeros
  cut <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / 3
  
  # Classify
  result <- ifelse(
    is.na(x), "Not Available",
      ifelse(x == 1, "High",  # PRIORITIZE value 1 as "High"
        ifelse(
          x >= 0 & x <= (min(x, na.rm = TRUE) + cut), "Low",
          ifelse(
            x > (min(x, na.rm = TRUE) + cut) & x <= (min(x, na.rm = TRUE) + 2 * cut), "Moderate",
            "High"
          )
        )
      )
    )
  
  return(factor(result, levels = c("Low", "Moderate", "High", "Not Available")))
}

cut_with_zero_category_bn <- function(x) {
  if (length(x) == 0) {
    return(factor(rep("", length(x)),
                  levels = c("কম", "মাঝারি", "বেশি", "এর তথ্য নেই")))
  }
  # Calculate cut width excluding zeros
  cut <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / 3
  
  # Classify
  result <- ifelse(
    is.na(x), "এর তথ্য নেই",
      ifelse(x == 1, "বেশি",  # PRIORITIZE value 1 as "High"
        ifelse(
          x >= 0 & x <= (min(x, na.rm = TRUE) + cut), "কম",
          ifelse(
            x > (min(x, na.rm = TRUE) + cut) & x <= (min(x, na.rm = TRUE) + 2 * cut), "মাঝারি",
            "বেশি"
          )
        )
      )
    )
  
  return(factor(result, levels = c("কম", "মাঝারি", "বেশি", "এর তথ্য নেই")))
}

# Step 6: Categorize each indicator

df<-df %>%
group_by(year,week)%>%
mutate(composite_risk_category=cut_with_zero_category(composite_risk),
       category_patients=cut_with_zero_category(risk_patients),
       category_deaths=cut_with_zero_category(risk_deaths),
       category_mosquito=cut_with_zero_category(risk_adult_mosquito),
       category_BI=cut_with_zero_category(risk_bi),
       category_CI=cut_with_zero_category(risk_ci),
       category_HI=cut_with_zero_category(risk_hi),
       category_pop_den=cut_with_zero_category(risk_pop_den),
       category_breed_site = cut_with_zero_category(breeding_risk) )%>%
mutate(composite_risk_category_bn=cut_with_zero_category_bn(composite_risk),
       category_patients_bn=cut_with_zero_category_bn(risk_patients),
       category_deaths_bn=cut_with_zero_category_bn(risk_deaths),
       category_mosquito_bn=cut_with_zero_category_bn(risk_adult_mosquito),
       category_BI_bn=cut_with_zero_category_bn(risk_bi),
       category_CI_bn=cut_with_zero_category_bn(risk_ci),
       category_HI_bn=cut_with_zero_category_bn(risk_hi),
       category_pop_den_bn=cut_with_zero_category_bn(risk_pop_den),
       category_breed_site_bn = cut_with_zero_category_bn(breeding_risk) )%>%
mutate(summary=paste0("dengue patient number is ",tolower(category_patients),", ","dengue death number is ",tolower(category_deaths),", ","dengue mosquito number is ",tolower(category_mosquito),", ","dengue mosquito breeding site number is ",tolower(category_breed_site)," and ","population density is ",tolower(category_pop_den)))%>%
mutate(summary_bn=paste0("ডেঙ্গু রোগীর সংখ্যা ",category_patients_bn,", ","ডেঙ্গুর কারণে মৃত্যুর সংখ্যা ",category_deaths_bn,", ","ডেঙ্গু মশার সংখ্যা ",category_mosquito_bn,", ","ডেঙ্গু মশার প্রজননস্থলের সংখ্যা ",category_breed_site_bn," এবং ","জনসংখ্যার ঘনত্ব ",category_pop_den_bn))%>%
mutate(Avg_mosq_per_trap=Total_mosquito/2,
              BI=ifelse(Total_House==0,0,Positive_Wet_Container/Total_House*100),
              CI=ifelse(Total_Wet_container==0,0,Positive_Wet_Container/Total_Wet_container*100),
              HI=ifelse(Total_House==0,0,Positive_House/Total_House*100))%>%      
arrange(Date)%>%
filter(is.na(Date)==FALSE)
#dplyr::select(Date,year,month,week,date_range,week_ju,zone.y,ward.y,wardf,wardfc,area,long,lat,Total,Total_death,Total_mosquito,Total_larvae,population,patient_rate,patient_risk_level,Patient_Growth,CFR,death_risk_level,Death_Growth,adult_mosq_density,adult_mosq_risk_level,Mosquito_Growth,larvae_density,larvae_risk_level,Larvae_Growth,composite_risk,composite_risk_level)

##joining daily data with weekly risks
zonal_patient_death_mosq_risk<-zonal_patient_death%>%
left_join(.,df,by=c("year","week","zone","ward","wardfc","area"))%>%
mutate(Date=Date.x,
       Total_patient=Total_patient.x,
       Total_death=Total_death.x,
       lat=lat.x,
       long=long.x,
       date_range=date_range.x,
       month=month.x,
       population=population.x,
       population_at_risk=population*composite_risk)%>%
       mutate(Positive_wet_container=ifelse(is.na(Positive_Wet_Container)==TRUE,0,Positive_Wet_Container),
              Positive_House=ifelse(is.na(Positive_House)==TRUE,0,Positive_House),
              Total_Wet_container=ifelse(is.na(Total_Wet_container)==TRUE,0,Total_Wet_container),
              Total_House=ifelse(is.na(Total_House)==TRUE,0,Total_House),
              Total_mosquito=ifelse(is.na(Total_mosquito)==TRUE,0,Total_mosquito))%>%    
dplyr::select(Date,year,month,week,week_ju,date_range,zone,ward,wardfc,area,area_bn,population,population_density,Total_patient,Total_death,Total_mosquito,Avg_mosq_per_trap,BI,CI,HI,roll_total_patients,roll_total_deaths,roll_avg_mosq_per_trap,roll_bi,roll_ci,roll_hi)%>%
filter(Date>=start_date)

OUTPUT_SHEET_ID  <- "1EMva389XPbDyJs80M3FZhgj8hGNCOouWzo4bz0cr0Ww"
OUTPUT_SHEET_TAB <- "zone_ward_patient_death_larv_mosq_risk"

sheet_append(
  ss    = OUTPUT_SHEET_ID,
  sheet = OUTPUT_SHEET_TAB,
  data  = zonal_patient_death_mosq_risk
)

##splitting the columns to data sets due to tableau loading problem

zonal_patient_death_mosq_risk1<-zonal_patient_death%>%
left_join(.,df,by=c("year","week","zone","ward","wardfc","area"))%>%
mutate(Date=Date.x,
       Total_patient=Total_patient.x,
       Total_death=Total_death.x,
       lat=lat.x,
       long=long.x,
       date_range=date_range.x,
       month=month.x,
       population=population.x,
       population_at_risk=population*composite_risk)%>%
       mutate(Positive_wet_container=ifelse(is.na(Positive_Wet_Container)==TRUE,0,Positive_Wet_Container),
              Positive_House=ifelse(is.na(Positive_House)==TRUE,0,Positive_House),
              Total_Wet_container=ifelse(is.na(Total_Wet_container)==TRUE,0,Total_Wet_container),
              Total_House=ifelse(is.na(Total_House)==TRUE,0,Total_House),
              Total_mosquito=ifelse(is.na(Total_mosquito)==TRUE,0,Total_mosquito))%>%  
dplyr::select(Date,year,month,week,week_ju,date_range,zone,ward,wardfc,area,area_bn,risk_patients,risk_deaths,risk_adult_mosquito,risk_bi,risk_ci,risk_hi,risk_pop_den,composite_risk,category_patients,category_patients_bn,category_deaths,category_deaths_bn,category_mosquito,category_mosquito_bn,category_BI,category_BI_bn,category_CI,category_CI_bn,category_HI,category_HI_bn,category_pop_den,category_pop_den_bn,area_sqkm,composite_risk_category,composite_risk_category_bn,summary,summary_bn)%>%
filter(Date>=start_date)

OUTPUT_SHEET_ID  <- "1zEaxK5CyBCa_TxmFYjjcnsAQ3bxxrecfMGiKJ7gqXAU"
OUTPUT_SHEET_TAB <- "zone_ward_patient_death_larv_mosq_risk"

sheet_append(
  ss    = OUTPUT_SHEET_ID,
  sheet = OUTPUT_SHEET_TAB,
  data  = zonal_patient_death_mosq_risk1
)
