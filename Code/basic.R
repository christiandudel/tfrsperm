### Packages ###################################################################

  library(readxl)
  library(tidyverse)
  library(httr)
  library(data.table)


### Load sperm data ############################################################

  sperm <- read_xlsx(path="U:/Data/spermquality/sperm_count_data.xlsx")

  
### Edit sperm quality data ####################################################  
  
  # Get correct rows (drop infertile samples, drop subgroups)
  sperm <- sperm |> filter(fertility_group!="Infertile" & level_2==0)
  
  # Force selected variables to be numeric
  sperm <- sperm |> mutate(year_collection_start=as.numeric(year_collection_start),
                           year_collection_end=as.numeric(year_collection_end),
                           year_publication=as.numeric(year_publication),
                           sc_mean=as.numeric(sc_mean))
  
  # Generate year
  sperm$year <- NA
  sperm <- sperm |> mutate(year=ifelse(is.na(year_collection_start)|is.na(year_collection_end),year_publication,year),
                           year=ifelse(!is.na(year_collection_start)&is.na(year_collection_end),median(year_collection_end:year_collection_start),year))
  
  # Select interesting variables
  sperm <- sperm |> select(country,year,sc_mean)
  
  # Rename
  names(sperm) <- c("Country","Year","Spermcount")
  
  # Drop missings
  sperm <- na.omit(sperm)


### Get TFR data from UN #######################################################

  # File on the WEB
  url <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"
  
  # Where to save it?
  xlsx <- "Data/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"

  # Download
  if(!file.exists(xlsx)) GET(url, write_disk(xlsx, overwrite = TRUE), progress() )
  
  # Load
  fert <- read_xlsx(path=xlsx,skip=16)
  
  
### Edit UN data ###############################################################
  
  # Select relevant variables
  fert <- fert |> select("Region, subregion, country or area *",
                         "Year",
                         "Total Fertility Rate (live births per woman)")
  
  # Rename
  names(fert) <- c("Country","Year","TFR")
  
  # Force numeric
  fert <- fert |> mutate(Year=as.numeric(Year),
                         TFR=as.numeric(TFR))
  

### Merge ######################################################################

  merged <- merge(sperm,fert)
  
  
### Regression #################################################################
  
  # Naive plot
  merged |> ggplot(aes(x=Spermcount,y=TFR)) + 
              geom_point() + 
              geom_smooth(method="lm")

  # Naive regression
  fit1 <- lm(TFR~Spermcount,data=merged)
  summary(fit1)
  
  # Slightly more complex
  merged <- merged |> mutate(Sm2=Spermcount^2)
  fit2 <- lm(TFR~Spermcount+Sm2,data=merged) 
  
  # Marginal effect 
  baseline <- 100
  change <- -50
  b1 <- sum(coef(fit2)*c(1,baseline,baseline^2))
  b2 <- sum(coef(fit2)*c(1,(baseline+change),(baseline+change)^2))
  b2-b1  
  # Drop from sperm count of 100 to 50: -0.194408
  # But drop from sperm count 75 to 25: +0.5616161
  