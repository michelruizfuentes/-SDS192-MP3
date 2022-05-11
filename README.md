# Deaths from Opioid Overdoses in New Jersey From 2012 to 2015

- Brianna Mateo, Michel Ruiz-Fuentes
- SDS192 (02) Introduction to Data Science
- 23 Nov. 2021

## Introduction

Our map of New Jersey from 2012 to 2015 displays deaths from opioid usage. The choropleth shows a range of death by county, and the dotted plots are the locations for medical assistance like rehabilitation services or hospitals. The choropleth helps 1the audience distinguish the number of deaths where the darker regions represent higher numbers of deaths, and there were fewer deaths in lighter regions. Above the gradient of colors, there is another layer of data. As we explored the aftermath of overdosing, our team was fascinated in investigating the state’s methods to combating drug-related deaths. Were there services available to assist people struggling with addiction? 2 Access to rehabilitation centers is crucial because their services are necessary for dire situations, and visibility and access to these centers have drastic consequences as serious as life or death. Our data support this claim because the map shows fewer deaths in areas populated with these services. In darker regions or southern New Jersey, there were more deaths from opioid overdose; there were fewer hospitals and rehab centers. In lighter-shaded areas, northern New Jersey, there are fewer deaths and more medical services. This demonstrates that there may be a relationship between deaths from opioid overdose and the lack of rehabilitative services. This analysis prompted more questions: (1) is it correlation or causation, (2) would the south have fewer deaths if they were more medical services? In the future, we would also explore if the average socio-economic status in counties is correlated with overdoses.

## Loading necessary package and csv files 

```{r, echo=FALSE}
# Load all packages here
library(tidyverse)
library(fivethirtyeight)
library(sf)
library(USAboundaries)
library(tidycensus)
library(tmaptools)
library(leaflet)
library(sf)
library(maps)

county_ods_16<- read_csv("Data/county-ods-16.csv")

AcuteCareFacilities<- read_csv("Data/Acute-Care_Facilities.csv")
NJ_roads<- "tl_2015_34_prisecroads" %>%
  read_sf()
  
```

## Your Map

```{r}

NJ_county_orig <- 
  tidycensus::get_acs(
    geography = "county", 
    variables = "B01003_001",
    state="NJ",
    geometry = TRUE
  ) %>% 
  # Add centroids to each region using purrr package (I have no idea how this 
  # works!)
  mutate(
    lon = purrr::map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = purrr::map_dbl(geometry, ~st_centroid(.x)[[2]])
  ) 
NJ_county_orig_coords<-NJ_county_orig%>% 
  separate(NAME,into=c('COUNTY','state'),sep= ",")%>% 
separate(COUNTY, c("COUNTY","Fluff"), sep = " C")%>% 
select(-"Fluff")


NJcounties_joined<-county_ods_16%>% 
left_join(NJ_county_orig_coords, by ="COUNTY")


NJcounties_joined <-NJcounties_joined %>%
mutate(totaldeaths_12_15 = (Total_2012+Total_2013+Total_2014+Total_2015))

NJ_AcuteCareFacilities<-AcuteCareFacilities%>% 
  rename(Facility_Type =`Facility Type`)%>% 
  filter(str_detect(Facility_Type ,"COMPREHENSIVE"))%>% 
  separate(Georeference,into=c('Point','lat',"lon"),sep=" ")%>% 
  mutate(lat = str_replace_all(lat, "\\*|\\(|\\)", ""))%>% 
  mutate(lon = str_replace_all(lon, "\\*|\\(|\\)", ""))%>% 
separate(Facility_Type,c("comp","type"), sep = "COMPREHENSIVE")%>% 
select(-"comp")



NJ_AcuteCareFacilities_sf <- NJ_AcuteCareFacilities%>% 
st_as_sf(coords = c("lat", "lon"), crs = 4326)

 
ggplot()+
geom_sf(data =NJcounties_joined, aes(fill = totaldeaths_12_15, geometry = geometry),
color="black",size = 0.5)+
scale_fill_continuous(low = "#fff7bc",high = "#f03b20", breaks=c(650,550,450,350,250,150,0),
label = scales::comma)+
geom_sf(data = NJ_roads,size=0.2,col="#3690c0")+
geom_sf(data = NJ_AcuteCareFacilities_sf, aes(col=type),size = 2)+
scale_color_manual(values=c("navyblue","#1a9850","#ffeda0"))+
labs(title="Deaths from Opioid Overdoses in New Jersey From 2012 to 2015",
     subtitle="Are there medical services to assist dire situations of drug use?",
     fill = "Overdose Deaths",
     color = "Acute-Care Rehab Facilities",x="Longitude",y="Latitude")
```

## Additional Analysis

Our map shows three pivotal layers of data that contribute to the conversation of deaths from opioid usage. The first layer of data shows the gradient colors displaying the number of deaths, in other words, the consequences of opioid use. The second layer of data is the dotted plots showing the medical services available or the proactive solution to combat overdoses. After identifying the overall trend that darker regions had fewer medical services, we furthered our studies on accessibility. In addition to assessing if the state had sufficient rehabilitation centers and hospitals in each county, we plotted a third layer of data using a map of prominent New Jersey roads. This would show how accessible it is to commute to each Acute Care Facility. Like our first map, we see that streets are more clustered in lighter regions meaning there are more roads to the facilities in locations with fewer overdose deaths. Alternatively, if we look at the county on the right near the bottom shaded with the darkest orange, we see fewer roads for the Rehabilitation Hospital. This data prompts further investigation about the relationship between roads to medical services and deaths from an opioid overdose. For example, are there fewer deaths in northern New Jersey because there are more medical centers and roads to access those services? Conversely, are there more deaths in some southern states because there are fewer hospitals and rehabilitation centers and fewer roads to get to those locations?
