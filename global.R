#library(dhis2Pull)
library(tidyr)
library(dplyr)
library(plyr)
library(httr)
library(leaflet)
library(ggplot2)
library(rlist)
library(gplots)

source("GetRequest.r")

# anc1 coverage resp
anc1_data_frame <- function(){
  anc1_resp <- dhis2_rapi("https://play.dhis2.org/release1/api/25/analytics.json?dimension=ou:O6uvpzGd5pu;PMa2VCrupOd;jUb8gELQApl;kJq2mPyFEHo;lc3eMKXaEfw&dimension=pe:LAST_12_MONTHS&filter=dx:dwEq7wi6nXV&displayProperty=NAME&skipMeta=true")
  anc1_parsed <- anc1_resp$content$rows
  orgID<- sapply(anc1_parsed,"[[",1)
  period <- sapply(anc1_parsed,"[[",2)
  anc1_coverage<- sapply(anc1_parsed,"[[",3)
  anc1_data_frame <-data.frame(orgID,period,anc1_coverage,stringsAsFactors =F)
  return(anc1_data_frame)
}


# Extract orgUnits
org_data_frame <- function(){
  org_resp <- dhis2_rapi("https://play.dhis2.org/release1/api/organisationUnits?paging=FALSE")
  org_parsed <- org_resp$content$organisationUnits
  orgID<- sapply(org_parsed,"[[",1)
  orgName <- sapply(org_parsed,"[[",2)
  org_data_frame <- data.frame(orgID,orgName,stringsAsFactors = F)
  return(org_data_frame)
}

# Get org ID and name that matches the selected orgunits
filtered_org_data_frame <- org_data_frame() %>% filter(orgID %in% c("O6uvpzGd5pu", "PMa2VCrupOd", "jUb8gELQApl", "kJq2mPyFEHo", "lc3eMKXaEfw"))

# documented anc1_data-fame with org_Name
documeneted_anc1_data_frame <- function(){
  left_join(filtered_org_data_frame,anc1_data_frame(),by="orgID")
}


pregnancy_comp_df <- function(){
  resp <- dhis2_rapi("https://play.dhis2.org/release1/api/25/analytics.json?dimension=ou:O6uvpzGd5pu;PMa2VCrupOd;jUb8gELQApl;kJq2mPyFEHo;lc3eMKXaEfw&dimension=pe:LAST_12_MONTHS&filter=dx:h8vtacmZL5j&displayProperty=NAME&skipMeta=true")
  parsed_resp <- resp$content$rows
  #Extract elements from the list
  orgID <- sapply(parsed_resp,"[[",1)
  period <- sapply(parsed_resp,"[[",2)
  complications <- sapply(parsed_resp,"[[",3)
  pregnancy_comp_df <- data.frame(orgID,period,complications,stringsAsFactors = F)
  return(pregnancy_comp_df)
}

# Join pregnancy_comp_df with documented_anc1_data_frame for comparion
pregnancy_comp_anc1_df <- function(){
  left_join(documeneted_anc1_data_frame(),pregnancy_comp_df(),by=c("orgID","period"))
}


map_ANC <- function(){
  # Load jsonfile with coordinates into R
  anc_shapefile <- geojsonio::geojson_read("www\\merged.json",what="sp")
  
  # basic map
  m <-leaflet(anc_shapefile) %>% addTiles()
  
  # set the number of bins - arrange of values for color
  bins <- c(90,100,110,120,130,200)
  pal <- colorBin("YlOrRd",domain = anc_shapefile$average,bins = bins)
  # customize the labels using HTML
  labels <- sprintf(
    "<strong>%s</strong><br/>Average ANC1: %g",
    anc_shapefile$name,anc_shapefile$average
  ) %>% lapply(htmltools::HTML) # pass the label to HTML so that leaflet understands
  
  m <-m %>% addPolygons( # add some color and set a few properites
    fillColor = ~pal(average), 
    weight=2,
    opacity=1,
    color="white",
    dashArray="3",
    fillOpacity = 0.7,
    highlight=highlightOptions( # Add some interaction that will highligh the polygon as the mouses hoovers
      weight=5,
      color="#666",
      dashArray="",
      fillOpacity = 0.7,
      bringToFront=TRUE
    ),
    label=labels, # add the label
    labelOptions=labelOptions(
      style=list("font-weight"="normal",padding="3px 8px"),
      textsize="15px",
      direction="auto"
    )
  )
  # add legend
  m %>% addLegend(pal=pal,values=~average,opacity=0.7,title="Average ANC1",position="bottomright")
}

map_preg <- function(){
  shapefile <- geojsonio::geojson_read("www\\merged.json",what="sp")
  # get average pregnancy complications from pregnacy_comp diff
  complications <- pregnancy_comp_df() %>% spread(orgID,complications) %>% summarise(jUb8gELQApl=mean(as.numeric(jUb8gELQApl),na.rm=T),kJq2mPyFEHo=mean(as.numeric(kJq2mPyFEHo),na.rm=T),lc3eMKXaEfw=mean(as.numeric(lc3eMKXaEfw),na.rm=T),O6uvpzGd5pu=mean(as.numeric(O6uvpzGd5pu),na.rm=T),PMa2VCrupOd=mean(as.numeric(PMa2VCrupOd),na.rm=T)) %>% gather(id,complications)
  # merge complications to the shape file
  merged_shape <- merge(shapefile,complications,by="id")
  
  #basic map
  m <- leaflet(merged_shape) %>% addTiles() 
  # Set the number of bins
  bins <- c(100,500,1000,1500,2000,2500)
  pal <- colorBin("YlOrRd",domain = merged_shape$complications,bins = bins)
  # customize the labels using HTML
  labels <- sprintf(
    "<strong>%s</strong><br/>Pregnancy-related complications: %g",
    merged_shape$name,merged_shape$complications
  ) %>% lapply(htmltools::HTML) # pass the label to HTML so that leaflet understands
  
  m <-m %>% addPolygons( # add some color and set a few properites
    fillColor = ~pal(complications), 
    weight=2,
    opacity=1,
    color="white",
    dashArray="3",
    fillOpacity = 0.7,
    highlight=highlightOptions( # Add some interaction that will highligh the polygon as the mouses hoovers
      weight=5,
      color="#666",
      dashArray="",
      fillOpacity = 0.7,
      bringToFront=TRUE
    ),
    label=labels, # add the label
    labelOptions=labelOptions(
      style=list("font-weight"="normal",padding="3px 8px"),
      textsize="15px",
      direction="auto"
    )
  )
  m %>% addLegend(pal=pal,values=~complications,opacity=0.7,title="Average pregnacy-related complications",position="bottomright")
  
}


# Anc data set wide
anc1_visit_dataset <- function(){
  documeneted_anc1_data_frame() %>% 
    select(-orgID) %>%
    spread(orgName,anc1_coverage) %>% mutate(month = 1:length(period)) %>%
    summarise(month=period,Bo=as.numeric(Bo),Bonthe=as.numeric(Bonthe),Kailahun=as.numeric(Kailahun),Kambia=as.numeric(Kambia),Kenema=as.numeric(Kenema))
}

# Pregnancy related complications 
preg_comp_dataset <- function(){
  pregnancy_comp_anc1_df() %>%
    select(-orgID,-anc1_coverage) %>% 
    spread(orgName,complications) %>% mutate(month = 1:length(period)) %>%
    summarise(month=period,Bo=as.numeric(Bo),Bonthe=as.numeric(Bonthe),Kailahun=as.numeric(Kailahun),Kambia=as.numeric(Kambia),Kenema=as.numeric(Kenema))
    
}


#'
#' Heatmap code
#' 
heatmap_anc <- function(){
  dataset <- anc1_visit_dataset() # Get anc data
  rnames <- dataset[,1] # Get the period as row names
  hm_matrix <- data.matrix(dataset[,2:ncol(dataset)]) # transform the other rows into a matrix
  row.names(hm_matrix) <- rnames # assign row names to the matrix
  
  # create a color palette
  my_palette <- colorRampPalette(c("red","yellow","green"))(n=299)
  # Define color breaks
  #png("anc_heatmap.png", # create a png file for the heatmap
  #    width = 5*300, # 5*300px
  #    height = 5*300, #5*300px
  #    res = 300, #300px per inch
  #    pointsize = 8
  #    )
  
  

heatmap.2(hm_matrix,
          cellnote = hm_matrix,  # same data set for cell labels
          main = "Distribution of ANC", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          #breaks=bins,    # enable color transition at specified limits
          dendrogram="row",     # only draw a row dendrogram
          Colv="NA")            # turn off column clustering
#dev.off()

}

heatmap_preg <- function(){
  dataset <- preg_comp_dataset() # Get pregancy complications data
  rnames <- dataset[,1] # Get the period as row names
  hm_matrix <- data.matrix(dataset[,2:ncol(dataset)]) # transform the other rows into a matrix
  row.names(hm_matrix) <- rnames # assign row names to the matrix
  
  # create a color palette
  my_palette <- colorRampPalette(c("red","yellow","green"))(n=299)
  # Define color breaks
  #png("anc_heatmap.png", # create a png file for the heatmap
  #    width = 5*300, # 5*300px
  #    height = 5*300, #5*300px
  #    res = 300, #300px per inch
  #    pointsize = 8
  #    )
 
  heatmap.2(hm_matrix,
            cellnote = hm_matrix,  # same data set for cell labels
            main = "Distribution of pregnancy-related complications", # heat map title
            notecol="black",      # change font color of cell labels to black
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            margins =c(12,9),     # widens margins around plot
            col=my_palette,       # use on color palette defined earlier
            #breaks=bins,    # enable color transition at specified limits
            dendrogram="row",     # only draw a row dendrogram
            Colv="NA")            # turn off column clustering
  #dev.off()
}


# ========================================================================================
# compute performance of the 5 org units bsed on the variance of the difference of ANC and pregnancy 
# related complications 
# Get customized performance data
compute_performance <- function(){
  # Get different data 
  ANC1_visit <- documeneted_anc1_data_frame() %>% 
    select(-orgID) %>%
    spread(orgName,anc1_coverage) %>% mutate(month = 1:length(period))
  Pregnancy_Comp <- pregnancy_comp_anc1_df() %>%
    select(-orgID,-anc1_coverage) %>% 
    spread(orgName,complications) %>% mutate(month = 1:length(period))
  # merge   ```````````````the data
  ANC1_Pregnancy <- left_join(ANC1_visit,Pregnancy_Comp,by="month")
  # Compute difference of ANC1 and pregancy comp
  ANC1_Pregnancy_diff <- ANC1_Pregnancy %>% mutate(Bo.diff=as.numeric(Bo.x)-as.numeric(Bo.y)) %>%
    mutate(Bonthe.diff=as.numeric(Bonthe.x)-as.numeric(Bonthe.y)) %>%
    mutate(Kailahun.diff=as.numeric(Kailahun.x)-as.numeric(Kailahun.y)) %>%
    mutate(Kambia.diff=as.numeric(Kambia.x)-as.numeric(Kambia.y)) %>%
    mutate(Kenema.diff=as.numeric(Kenema.x)-as.numeric(Kenema.y))
  # Get the variance
  ANC1_Variance <- ANC1_Pregnancy_diff %>% mutate(Bo.var=(Bo.diff-mean(Bo.diff,na.rm=T))^2) %>%
    mutate(Bonthe.var=(Bonthe.diff-mean(Bonthe.diff,na.rm=T))^2) %>%
    mutate(Kailahun.var=(Kailahun.diff-mean(Kailahun.diff,na.rm=T))^2) %>%
    mutate(Kambia.var=(Kambia.diff-mean(Kambia.diff,na.rm=T))^2) %>%
    mutate(Kenema.var=(Kenema.diff-mean(Kenema.diff,na.rm=T))^2)
  # Select org unit variance and rename to org-units and month to reconstract the dataframe and make it long
  performance_long <- ANC1_Variance %>% select(month,Bo.var,Bonthe.var,Kailahun.var,Kambia.var,Kenema.var) %>%
    mutate(Bo=Bo.var) %>% mutate(Bonthe=Bonthe.var) %>% mutate(Kailahun=Kailahun.var) %>% mutate(Kambia=Kambia.var)%>% mutate(Kenema=Kenema.var) %>%
    select(month,Bo,Bonthe,Kailahun,Kambia,Kenema) 
  return(performance_long)
}


## === ANC1 data for prediction==============================================
anc1_predict <- function(){
  # Get different data 
  ANC1_visit <- documeneted_anc1_data_frame() %>% 
    select(-orgID) %>%
    spread(orgName,anc1_coverage) %>% mutate(month = 1:length(period)) %>% 
    summarise(period=period,Bo=as.numeric(Bo),Bonthe=as.numeric(Bonthe),Kailahun=as.numeric(Kailahun),Kambia=as.numeric(Kambia),Kenema=as.numeric(Kenema))
  return(ANC1_visit)
}

preg_predict <- function(){
  Pregnancy_Comp <- pregnancy_comp_anc1_df() %>%
    select(-orgID,-anc1_coverage) %>% 
    spread(orgName,complications) %>% mutate(month = 1:length(period)) %>%
    summarise(period=period,Bo=as.numeric(Bo),Bonthe=as.numeric(Bonthe),Kailahun=as.numeric(Kailahun),Kambia=as.numeric(Kambia),Kenema=as.numeric(Kenema))
  return(Pregnancy_Comp)
}



