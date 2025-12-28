##reading patient's data

dengue_data<-function(start_date,end_date,format){
# Register Google API key (if required)
register_google(key = Sys.getenv("GMAPS_API_KEY"))


# Get latitude and longitude from address
raw_data<-read_sheet("164-CIeR5mSutgIUPVS2EteQy1lGMSEzFhHj7iCLqzqg",sheet="patient")%>%
mutate(Date=as.Date(Date,format=format))%>%
filter(Date>=start_date & Date<=end_date)%>%
mutate(Address1=paste0(Address," ","Dhaka, Bangladesh"))
location <- geocode(raw_data$Address1)
#print(location)
data_lat_long<-data.frame(raw_data,location)

##reading shapefile
map <- list.files(path = "DNCC_Ward_Boundary_poly_urp_f.shp") %>%
  lapply(st_read) %>%
  purrr::reduce(st_combine) %>% #for multiple map areas
  st_transform(crs = 4326)  #projects to WGS84

current_year<-max(year(end_date))

##adding the zones and wards to the shape file and save it
zn_ward<-read_sheet("164-CIeR5mSutgIUPVS2EteQy1lGMSEzFhHj7iCLqzqg",sheet="zone_ward_area_list")%>%
mutate(Year=current_year)

map_filtered<-map%>%
mutate(wardfc = paste0("Ward No-",Ward_no))%>%
mutate(ward = as.numeric(Ward_no))%>%
left_join(.,zn_ward,by="ward")

#st_write(map_filtered, "C:/Users/sangita.paul/OneDrive - United Nations Development Programme/DNCC innovation lab/Dengue management/Data from DNCC/Dhaka_north_city_updated.shp")
#write.csv(map_filtered, "C:/Users/sangita.paul/OneDrive - United Nations Development Programme/DNCC innovation lab/Dengue management/Data from DNCC/Dhaka_north_city_updated.csv")
#st_write(map_filtered, "C:/Users/sangita.paul/OneDrive - United Nations Development Programme/DNCC innovation lab/Dengue management/Data from DNCC/Dhaka_north_city_updated.geojson")


##reading the zone and wards lat long csv created previously
zone_patient<-data_lat_long

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
left_join(date2,by="Date")%>%
filter(Date>=start_date & Date<=end_date)

date_zone_ward<-crossing(date1,zn_ward)%>%
filter(Date>=start_date & Date<=end_date)%>%
mutate(Date=as.Date(Date,format="%m/%d/%Y"))%>%
mutate(zone=as.character(zone),
       ward=as.character(ward))

tmp <- zone_patient %>%
  #extract lat/longs 
  mutate_if(is.numeric, ~replace_na(., 0))%>%
  mutate(lon=as.numeric(lon))%>%
  mutate(lat=as.numeric(lat))%>%
  mutate(coord.x=as.character(lon))%>%
  mutate(coord.y=as.character(lat))%>%
    #map start points to polygons in map area
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(map)) %>%
  st_join(map_filtered)%>%
  st_drop_geometry()%>%
  mutate(Date=as.Date(Date,format=format))%>%
  left_join(.,date1,"Date", suffix = c("", "_ref"))%>% ##to add the week, year, month and date range
  mutate(ward = ifelse(is.na(Ward) == TRUE|Ward==0,ward,Ward))%>%
  mutate(ward=ifelse(ward == 63,53,ward))%>% 
  mutate(ward = as.character(ifelse(Address1 == "NA Dhaka, Bangladesh",NA,ward)))%>%
  mutate(zone = as.character(ifelse(Address1 == "NA Dhaka, Bangladesh",NA,zone)))%>%
  full_join(.,date_zone_ward,by=c("Date","zone","ward"))%>% ###to add the other zones and ward which contain no patient
  ##replacing NAs with week, month, year, date_range, week_ju with date1 values
  mutate(week  = coalesce(week.x,  week.y),
       month = coalesce(month.x, month.y),
       year  = coalesce(year.x,  year.y),
       date_range  = coalesce(date_range.x,  date_range.y),
       week_ju = as.character(coalesce(week_ju.x, week_ju.y)))%>%
  mutate(wardf=formatC(as.numeric(ward),width=2,flag="0"))%>%
  mutate(wardfc = paste0("Ward No-",wardf))%>%
  mutate(area=area.y)%>%
  mutate(Total_patient=ifelse(is.na(Total)==TRUE,0,Total))%>%
  mutate(Zone=as.character(Zone),
         Ward=as.character(Ward),
         wardf=as.character(wardf))%>%
  as.data.frame()%>%
  dplyr::select(Date,week,date_range,week_ju,month,year,Zone,Ward,Address1,zone,ward,wardf,wardfc,area,coord.x,coord.y,Total_patient)


OUTPUT_SHEET_ID  <- "1EMva389XPbDyJs80M3FZhgj8hGNCOouWzo4bz0cr0Ww"
OUTPUT_SHEET_TAB <- "dengue_patient_output"

sheet_append(
  ss    = OUTPUT_SHEET_ID,
  sheet = OUTPUT_SHEET_TAB,
  data  = tmp
)

}

##running the function with the input of starting date and ending date

###taking the date from last updated data
last_updated_data<-read_sheet(
  "1EMva389XPbDyJs80M3FZhgj8hGNCOouWzo4bz0cr0Ww",
  sheet = "zone_ward_patient_death_larv_mosq_risk",
  range = "A:A",        # <-- Date column only
  col_types = "D"
) %>%
  pull(Date) %>%
  max(na.rm = TRUE)

data_for_end_date<-read_sheet(
  "164-CIeR5mSutgIUPVS2EteQy1lGMSEzFhHj7iCLqzqg",
  sheet = "patient",
  range = "A:A",
  col_types = "D"
) %>%
  pull(Date) %>%
  max(na.rm = TRUE)

start_date=last_updated_data+1 ### from the date after the last updated date
end_date=data_for_end_date ##till the last 24 hours
format="%Y-%m-%d"
dengue_data(start_date,end_date,format)
