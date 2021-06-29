library(shiny)
library(graphics)
library(sf)
library(leaflet)
library(magrittr)
library(htmlwidgets)
library(spdep)
library(psych)
library(colorRamps)
library(pheatmap)


# Import data
data <- read.csv("PElection5.csv")
data2 <- read.csv("CountyElection.csv")

# Shapefile data: Taiwan town
Taiwan_T <- st_read("C:\\Users\\user\\Desktop\\B06607058\\CEDA\\TOWN\\Island.shp")
Taiwan_C <- st_read("C:\\Users\\user\\Desktop\\B06607058\\CEDA\\TOWN\\County.shp")
#Merge data
Taiwan_T <- merge(Taiwan_T, data, by.x="TOWNCODE", by.y="TOWNCODE")
Taiwan_C <- merge(Taiwan_C, data2, by.x="COUNTYNAME", by.y="COUNTYNAME")
# Adjacent area
nb <- poly2nb(Taiwan_T)

# Index 2004
for(i in 1:349){
  Taiwan_T$nb_2004[i] <- NA
  if(Taiwan_T$final_win_P2004[i] == 1){
    Taiwan_T$nb_2004[i] <- sum(Taiwan_T$final_win_P2004[as.numeric(nb[[i]])], na.rm = TRUE)
  }else if(Taiwan_T$final_win_P2004[i] == 0){
    Taiwan_T$nb_2004[i] <- length(nb[[i]]) - sum(is.na(Taiwan_T$final_win_P2004[nb[[i]]])) - sum(Taiwan_T$final_win_P2004[as.numeric(nb[[i]])], na.rm = TRUE)
  }
  Taiwan_T$nb_2004[i] <- Taiwan_T$nb_2004[i]/(length(nb[[i]]) - sum(is.na(Taiwan_T$final_win_P2004[nb[[i]]])))
}

# Index 2008
for(i in 1:349){
  Taiwan_T$nb_2008[i] <- NA
  if(Taiwan_T$final_win_P2008[i] == 1){
    Taiwan_T$nb_2008[i] <- sum(Taiwan_T$final_win_P2008[as.numeric(nb[[i]])], na.rm = TRUE)
  }else if(Taiwan_T$final_win_P2008[i] == 0){
    Taiwan_T$nb_2008[i] <- length(nb[[i]]) - sum(is.na(Taiwan_T$final_win_P2008[nb[[i]]])) - sum(Taiwan_T$final_win_P2008[as.numeric(nb[[i]])], na.rm = TRUE)
  }
  Taiwan_T$nb_2008[i] <- Taiwan_T$nb_2008[i]/(length(nb[[i]]) - sum(is.na(Taiwan_T$final_win_P2008[nb[[i]]])))
}

# Index 2012
for(i in 1:349){
  Taiwan_T$nb_2012[i] <- NA
  if(Taiwan_T$final_win_P2012[i] == 1){
    Taiwan_T$nb_2012[i] <- sum(Taiwan_T$final_win_P2012[as.numeric(nb[[i]])], na.rm = TRUE)
  }else if(Taiwan_T$final_win_P2012[i] == 0){
    Taiwan_T$nb_2012[i] <- length(nb[[i]]) - sum(is.na(Taiwan_T$final_win_P2012[nb[[i]]])) - sum(Taiwan_T$final_win_P2012[as.numeric(nb[[i]])], na.rm = TRUE)
  }
  Taiwan_T$nb_2012[i] <- Taiwan_T$nb_2012[i]/(length(nb[[i]]) - sum(is.na(Taiwan_T$final_win_P2012[nb[[i]]])))
}

# Index 2016
for(i in 1:349){
  Taiwan_T$nb_2016[i] <- NA
  if(Taiwan_T$final_win_P2016[i] == 1){
    Taiwan_T$nb_2016[i] <- sum(Taiwan_T$final_win_P2016[as.numeric(nb[[i]])], na.rm = TRUE)
  }else if(Taiwan_T$final_win_P2016[i] == 0){
    Taiwan_T$nb_2016[i] <- length(nb[[i]]) - sum(is.na(Taiwan_T$final_win_P2016[nb[[i]]])) - sum(Taiwan_T$final_win_P2016[as.numeric(nb[[i]])], na.rm = TRUE)
  }
  Taiwan_T$nb_2016[i] <- Taiwan_T$nb_2016[i]/(length(nb[[i]]) - sum(is.na(Taiwan_T$final_win_P2016[nb[[i]]])))
}

# Index 2020
for(i in 1:349){
  Taiwan_T$nb_2020[i] <- NA
  if(Taiwan_T$final_win_P2020[i] == 1){
    Taiwan_T$nb_2020[i] <- sum(Taiwan_T$final_win_P2020[as.numeric(nb[[i]])], na.rm = TRUE)
  }else if(Taiwan_T$final_win_P2020[i] == 0){
    Taiwan_T$nb_2020[i] <- length(nb[[i]]) - sum(is.na(Taiwan_T$final_win_P2020[nb[[i]]])) - sum(Taiwan_T$final_win_P2020[as.numeric(nb[[i]])], na.rm = TRUE)
  }
  Taiwan_T$nb_2020[i] <- Taiwan_T$nb_2020[i]/(length(nb[[i]]) - sum(is.na(Taiwan_T$final_win_P2020[nb[[i]]])))
}

# Geometric
geom <- function(x){
  y <- x + 1
  gm <- geometric.mean(y) - 1
  return(gm)
}

geo_2004 <- tapply(Taiwan_T$nb_2004, Taiwan_T$COUNTYNAME, geom)
geo_2008 <- tapply(Taiwan_T$nb_2008, Taiwan_T$COUNTYNAME, geom)
geo_2012 <- tapply(Taiwan_T$nb_2012, Taiwan_T$COUNTYNAME, geom)
geo_2016 <- tapply(Taiwan_T$nb_2016, Taiwan_T$COUNTYNAME, geom)
geo_2020 <- tapply(Taiwan_T$nb_2020, Taiwan_T$COUNTYNAME, geom)

for(i in 1:19){
  Taiwan_C$Corrected_2004[i] <- NA
  if(Taiwan_C$COUNTYNAME[i] == names(geo_2004[i])){
    Taiwan_C$Corrected_2004[i] <- Taiwan_C$Top_2004[i] * (1 - geo_2004 / 2)
    }
}

for(i in 1:19){
  Taiwan_C$Corrected_2008[i] <- NA
  if(Taiwan_C$COUNTYNAME[i] == names(geo_2008[i])){
    Taiwan_C$Corrected_2008[i] <- Taiwan_C$Top_2008[i] * (1 - geo_2008 / 2)
  }
}

for(i in 1:19){
  Taiwan_C$Corrected_2012[i] <- NA
  if(Taiwan_C$COUNTYNAME[i] == names(geo_2012[i])){
    Taiwan_C$Corrected_2012[i] <- Taiwan_C$Top_2012[i] * (1 - geo_2012 / 2)
  }
}

for(i in 1:19){
  Taiwan_C$Corrected_2016[i] <- NA
  if(Taiwan_C$COUNTYNAME[i] == names(geo_2016[i])){
    Taiwan_C$Corrected_2016[i] <- Taiwan_C$Top_2016[i] * (1 - geo_2016 / 2)
  }
}

for(i in 1:19){
  Taiwan_C$Corrected_2020[i] <- NA
  if(Taiwan_C$COUNTYNAME[i] == names(geo_2020[i])){
    Taiwan_C$Corrected_2020[i] <- Taiwan_C$Top_2020[i] * (1 - geo_2020 / 2)
  }
}

my.entropy <-  function(x) {
  e = sum(-x * log(x), na.rm=TRUE) 
  return(e)
}

tab <- cbind(abs(Taiwan_C$Top_2004),geo_2004,
             abs(Taiwan_C$Top_2008),geo_2008,
             abs(Taiwan_C$Top_2012),geo_2012,
             abs(Taiwan_C$Top_2016),geo_2016,
             abs(Taiwan_C$Top_2020),geo_2020)

colnames(tab) <- list(colnames(Taiwan_C)[5],"geo_2004",
                      colnames(Taiwan_C)[7],"geo_2008",
                      colnames(Taiwan_C)[9],"geo_2012",
                      colnames(Taiwan_C)[11],"geo_2016",
                      colnames(Taiwan_C)[13],"geo_2020")

as.out = matrix(0, 10, 10, 
                dimnames=list(colnames(tab)[1:10], colnames(tab)[1:10]))
for (c1 in 1:(10-1) ){
  for (c2 in (c1+1):10){
    cl1 = cutree(hclust(dist(tab[,c1])), 2)
    cl2 = cutree(hclust(dist(tab[,c2])), 2)
    t1 = table(cl1)
    t2 = table(cl2)
    tt = table(cl1, cl2)
    
    p1 = t1/sum(t1)
    p2 = t2/sum(t2)
    p2.1 = apply(tt, 1, function(x){x/sum(x)}) # 5x8
    p1.2 = apply(tt, 2, function(x){x/sum(x)}) # 8x5
    
    e2.1 = sum(p1 * apply(p2.1, 2, my.entropy), na.rm=TRUE)
    e1.2 = sum(p2 * apply(p1.2, 2, my.entropy), na.rm=TRUE)
    e1 = my.entropy(p1)
    e2 = my.entropy(p2)
    
    # As[row, column]
    as.out[c1,c2] = as.out[c2,c1] = (e1.2/e1 + e2.1/e2)/2
  }
}

pheatmap(1-as.out)

# Map----
# Define cut points for the colorbins
cuts <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
cuts2 <- c(seq(-0.8, 0.8, by = 0.1))

# Choose a color palette and assign it to the values
origin_04 <- colorBin(blue2green(16), domain = Taiwan_C$Top_2004, bins = cuts2)
origin_08 <- colorBin(blue2green(16), domain = Taiwan_C$Top_2008, bins = cuts2)
origin_12 <- colorBin(blue2green(16), domain = Taiwan_C$Top_2012, bins = cuts2)
origin_16 <- colorBin(blue2green(16), domain = Taiwan_C$Top_2016, bins = cuts2)
origin_20 <- colorBin(blue2green(16), domain = Taiwan_C$Top_2020, bins = cuts2)

index_04 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_T$nb_2004, bins = cuts)
index_08 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_T$nb_2008, bins = cuts)
index_12 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_T$nb_2012, bins = cuts)
index_16 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_T$nb_2016, bins = cuts)
index_20 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_T$nb_2020, bins = cuts)

correct_04 <- colorBin(blue2green(16), domain = Taiwan_C$Corrected_2004, bins = cuts2)
correct_08 <- colorBin(blue2green(16), domain = Taiwan_C$Corrected_2008, bins = cuts2)
correct_12 <- colorBin(blue2green(16), domain = Taiwan_C$Corrected_2012, bins = cuts2)
correct_16 <- colorBin(blue2green(16), domain = Taiwan_C$Corrected_2016, bins = cuts2)
correct_20 <- colorBin(blue2green(16), domain = Taiwan_C$Corrected_2020, bins = cuts2)

# Create HTML labels for tooltip
tooltip_origin_2004 <- sprintf("The win party in %s is %s, with <br>%.3f"
                              ,Taiwan_C$COUNTYNAME
                              ,Taiwan_C$WinP_2004
                              ,abs(Taiwan_C$Top_2004)
) %>% lapply(htmltools::HTML)

tooltip_origin_2008 <- sprintf("The win party in %s is %s, with <br>%.3f"
                               ,Taiwan_C$COUNTYNAME
                               ,Taiwan_C$WinP_2008
                               ,abs(Taiwan_C$Top_2008)
) %>% lapply(htmltools::HTML)

tooltip_origin_2012 <- sprintf("The win party in %s is %s, with <br>%.3f"
                               ,Taiwan_C$COUNTYNAME
                               ,Taiwan_C$WinP_2012
                               ,abs(Taiwan_C$Top_2012)
) %>% lapply(htmltools::HTML)

tooltip_origin_2016 <- sprintf("The win party in %s is %s, with <br>%.3f"
                               ,Taiwan_C$COUNTYNAME
                               ,Taiwan_C$WinP_2016
                               ,abs(Taiwan_C$Top_2016)
) %>% lapply(htmltools::HTML)

tooltip_origin_2020 <- sprintf("The win party in %s is %s, with <br>%.3f"
                               ,Taiwan_C$COUNTYNAME
                               ,Taiwan_C$WinP_2020
                               ,abs(Taiwan_C$Top_2020)
) %>% lapply(htmltools::HTML)

tooltip_index_2004 <- sprintf("The index for <strong>%s</strong> is <br>%.4f"
                              ,Taiwan_T$TOWNNAME
                              ,Taiwan_T$nb_2004
) %>% lapply(htmltools::HTML)

tooltip_index_2008 <- sprintf("The index for <strong>%s</strong> is <br>%.4f"
                              ,Taiwan_T$TOWNNAME
                              ,Taiwan_T$nb_2008
) %>% lapply(htmltools::HTML)

tooltip_index_2012 <- sprintf("The index for <strong>%s</strong> is <br>%.4f"
                              ,Taiwan_T$TOWNNAME
                              ,Taiwan_T$nb_2012
) %>% lapply(htmltools::HTML)

tooltip_index_2016 <- sprintf("The index for <strong>%s</strong> is <br>%.4f"
                              ,Taiwan_T$TOWNNAME
                              ,Taiwan_T$nb_2016
) %>% lapply(htmltools::HTML)

tooltip_index_2020 <- sprintf("The index for <strong>%s</strong> is <br>%.4f"
                              ,Taiwan_T$TOWNNAME
                              ,Taiwan_T$nb_2020
) %>% lapply(htmltools::HTML)

tooltip_correct_2004 <- sprintf("The win party in %s is %s, with adjuncted <br>%.3f"
                               ,Taiwan_C$COUNTYNAME
                               ,Taiwan_C$WinP_2004
                               ,abs(Taiwan_C$Corrected_2004)
) %>% lapply(htmltools::HTML)

tooltip_correct_2008 <- sprintf("The win party in %s is %s, with adjuncted <br>%.3f"
                                ,Taiwan_C$COUNTYNAME
                                ,Taiwan_C$WinP_2008
                                ,abs(Taiwan_C$Corrected_2008)
) %>% lapply(htmltools::HTML)

tooltip_correct_2012 <- sprintf("The win party in %s is %s, with adjuncted <br>%.3f"
                                ,Taiwan_C$COUNTYNAME
                                ,Taiwan_C$WinP_2012
                                ,abs(Taiwan_C$Corrected_2012)
) %>% lapply(htmltools::HTML)

tooltip_correct_2016 <- sprintf("The win party in %s is %s, with adjuncted <br>%.3f"
                                ,Taiwan_C$COUNTYNAME
                                ,Taiwan_C$WinP_2016
                                ,abs(Taiwan_C$Corrected_2016)
) %>% lapply(htmltools::HTML)

tooltip_correct_2020 <- sprintf("The win party in %s is %s, with adjuncted <br>%.3f"
                                ,Taiwan_C$COUNTYNAME
                                ,Taiwan_C$WinP_2020
                                ,abs(Taiwan_C$Corrected_2020)
) %>% lapply(htmltools::HTML)

# Origin vote map
map_origin_2004 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~origin_04(Taiwan_C$Top_2004)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~origin_04(Taiwan_C$Top_2004),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_origin_2004)

map_origin_2008 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~origin_08(Taiwan_C$Top_2008)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~origin_08(Taiwan_C$Top_2008),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_origin_2008)

map_origin_2012 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~origin_12(Taiwan_C$Top_2012)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~origin_12(Taiwan_C$Top_2012),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_origin_2012)

map_origin_2016 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~origin_16(Taiwan_C$Top_2016)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~origin_16(Taiwan_C$Top_2016),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_origin_2016)

map_origin_2020 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~origin_20(Taiwan_C$Top_2020)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~origin_20(Taiwan_C$Top_2020),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_origin_2020)

# Index map 
map_Index_2004 <- leaflet(Taiwan_T) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~index_04(Taiwan_T$nb_2004)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = index_04, values = Taiwan_T$nb_2004, opacity = 0.7,
            title = "Times of right", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~index_04(Taiwan_T$nb_2004),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_index_2004)

map_Index_2008 <- leaflet(Taiwan_T) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~index_08(Taiwan_T$nb_2008)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = index_08, values = Taiwan_T$nb_2008, opacity = 0.7,
            title = "Times of right", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7, 
              fillColor = ~index_08(Taiwan_T$nb_2008), 
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_index_2008)

map_Index_2012 <- leaflet(Taiwan_T) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~index_12(Taiwan_T$nb_2012)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = index_12, values = Taiwan_T$nb_2012, opacity = 0.7,
            title = "Times of right", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7, 
              fillColor = ~index_12(Taiwan_T$nb_2012), 
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_index_2012)

map_Index_2016 <- leaflet(Taiwan_T) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~index_16(Taiwan_T$nb_2016)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = index_16, values = Taiwan_T$nb_2016, opacity = 0.7,
            title = "Times of right", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7, 
              fillColor = ~index_16(Taiwan_T$nb_2016), 
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_index_2016)

map_Index_2020 <- leaflet(Taiwan_T) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~index_20(Taiwan_T$nb_2020)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = index_20, values = Taiwan_T$nb_2020, opacity = 0.7,
            title = "Times of right", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7, 
              fillColor = ~index_20(Taiwan_T$nb_2020), 
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_index_2020)

# Corrected vote map
map_corrected_2004 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~correct_04(Taiwan_C$Corrected_2004)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~correct_04(Taiwan_C$Corrected_2004),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_correct_2004)

map_corrected_2008 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~correct_08(Taiwan_C$Corrected_2008)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~correct_08(Taiwan_C$Corrected_2008),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_correct_2008)

map_corrected_2012 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~correct_12(Taiwan_C$Corrected_2012)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~correct_12(Taiwan_C$Corrected_2012),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_correct_2012)

map_corrected_2016 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~correct_16(Taiwan_C$Corrected_2016)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~correct_16(Taiwan_C$Corrected_2016),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_correct_2016)

map_corrected_2020 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~correct_20(Taiwan_C$Corrected_2020)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~correct_20(Taiwan_C$Corrected_2020),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_correct_2020)
