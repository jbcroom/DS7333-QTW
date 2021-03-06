library(data.table)
library(RCurl)
library(XML)
library(stringr)
library(tidyr)
library(urltools)
library(rvest)
library(dplyr)
library(XML)
library(ggplot2)


ubase = "http://www.cherryblossom.org/"

womenURLs = 
  c("results/1999/cb99f.html", 
    "results/2000/Cb003f.htm", 
    "results/2001/oof_f.html",
    "results/2002/ooff.htm", 
    "results/2003/CB03-F.HTM",
    "results/2004/women.htm", 
    "results/2005/CB05-F.htm", 
    "results/2006/women.htm", 
    "results/2007/women.htm", 
    "results/2008/women.htm", 
    "results/2009/09cucb-F.htm",
    "results/2010/2010cucb10m-F.htm", 
    "results/2011/2011cucb10m-F.htm",
    "results/2012/2012cucb10m-F.htm")

urls = paste(ubase, menURLs, sep = "")

urls[1:3]


extractResTable =
  # takes a list of websites from the cherry blossom race
  # a list of years corresponding to the year the result is for
  # and the gender of the participant
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  # returns a list of strings corrsponding to lines in the web url
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "male", file = NULL)
  {
    doc = htmlParse(url)
    
    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
      els = gsub("�", " ", els)
    }
    else if (year == 1999) {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]]   
    } 
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    } 
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }


years = 1999:2012
urls = paste(ubase, womenURLs, sep = "")
urls[1:3]
womenTables = mapply(extractResTable, url = urls, year = years, sex='female')
names(womenTables) = years
sapply(womenTables, length)

# 1999
women_1999_lst <- womenTables[1]
women_1999_lst <- lapply(women_1999_lst, function(x) {x[c(-1:-3)]})
lengths(women_1999_lst) # 2356

# Convert to dataframe
women_1999_t = as.data.frame(women_1999_lst)
women_1999_df <- data.frame(Place = substr(women_1999_t[1:2356,],1,5),
                            DivTot =substr(women_1999_t[1:2356,],7,15),
                            Name =substr(women_1999_t[1:2356,],17,37),
                            Ag =substr(women_1999_t[1:2356,],39,40),
                            Hometown =substr(women_1999_t[1:2356,],42,59),
                            Time =substr(women_1999_t[1:2356,],61,67),
                            Pace =substr(women_1999_t[1:2356,],69,73))

head(women_1999_df)
tail(women_1999_df)


# 2000
women_2000_lst <- womenTables[2]
women_2000_lst <- lapply(women_2000_lst, function(x) {x[c(-1:-2)]})
lengths(women_2000_lst) # 2167

# Convert to dataframe
women_2000_t = as.data.frame(women_2000_lst)
women_2000_df <- data.frame(Place = substr(women_2000_t[1:2166,],1,5),
                            DivTot =substr(women_2000_t[1:2166,],7,15),
                            Num =substr(women_2000_t[1:2166,],17,21),
                            Name =substr(women_2000_t[1:2166,],23,43),
                            Ag =substr(women_2000_t[1:2166,],45,46),
                            Hometown =substr(women_2000_t[1:2166,],48,65),
                            Gun =substr(women_2000_t[1:2166,],67,73),
                            Net =substr(women_2000_t[1:2166,],75,82))

head(women_2000_df)
tail(women_2000_df)

# 2001
women_2001_lst <- womenTables[3]
women_2001_lst <- lapply(women_2001_lst, function(x) {x[c(-1:-3)]})
lengths(women_2001_lst) # 2973

# Convert to dataframe
women_2001_t = as.data.frame(women_2001_lst)
women_2001_df <- data.frame(Place = substr(women_2001_t[1:2972,],1,5),
                            Num =substr(women_2001_t[1:2972,],7,11),
                            Name =substr(women_2001_t[1:2972,],13,33),
                            Ag =substr(women_2001_t[1:2972,],35,36),
                            Hometown =substr(women_2001_t[1:2972,],38,55),
                            Net =substr(women_2001_t[1:2972,],57,63),
                            Gun =substr(women_2001_t[1:2972,],65,71))

head(women_2001_df)
tail(women_2001_df)

# 2002
women_2002_lst <- womenTables[4]
women_2002_lst <- lapply(women_2002_lst, function(x) {x[c(-1:-3)]})
lengths(women_2002_lst) # 3334

# Convert to dataframe
women_2002_t = as.data.frame(women_2002_lst)
women_2002_df <- data.frame(Place = substr(women_2002_t[1:3334,],1,5),
                            Num =substr(women_2002_t[1:3334,],7,11),
                            Name =substr(women_2002_t[1:3334,],13,33),
                            Ag =substr(women_2002_t[1:3334,],35,36),
                            Hometown =substr(women_2002_t[1:3334,],38,55),
                            Net =substr(women_2002_t[1:3334,],57,63),
                            Gun =substr(women_2002_t[1:3334,],65,71))

head(women_2002_df)
tail(women_2002_df)

# 2003
women_2003_lst <- womenTables[5]
women_2003_lst <- lapply(women_2003_lst, function(x) {x[c(-1:-3)]})
lengths(women_2003_lst) # 3543

# Convert to dataframe
women_2003_t = as.data.frame(women_2003_lst)
women_2003_df <- data.frame(Place = substr(women_2003_t[1:3542,],1,5),
                            DivTot =substr(women_2003_t[1:3542,],7,15),
                            Num =substr(women_2003_t[1:3542,],17,21),
                            Name =substr(women_2003_t[1:3542,],23,52),
                            Ag =substr(women_2003_t[1:3542,],54,55),
                            Hometown =substr(women_2003_t[1:3542,],57,75),
                            GunTime =substr(women_2003_t[1:3542,],77,83),
                            NetTime =substr(women_2003_t[1:3542,],85,92))

head(women_2003_df)
tail(women_2003_df)


# 2004
women_2004_lst <- womenTables[6]
women_2004_lst <- lapply(women_2004_lst, function(x) {x[c(-1:-4)]})
lengths(women_2004_lst) # 3903

# Convert to dataframe
women_2004_t = as.data.frame(women_2004_lst)
women_2004_df <- data.frame(Place = substr(women_2004_t[5:3899,],1,5),
                            DivTot =substr(women_2004_t[5:3899,],7,15),
                            Num =substr(women_2004_t[5:3899,],17,21),
                            Name =substr(women_2004_t[5:3899,],23,51),
                            Ag =substr(women_2004_t[5:3899,],53,54),
                            Hometown =substr(women_2004_t[5:3899,],56,74),
                            Net =substr(women_2004_t[5:3899,],76,82),
                            Gun =substr(women_2004_t[5:3899,],84,90))

head(women_2004_df)
tail(women_2004_df)

# 2005
women_2005_lst <- womenTables[7]
women_2005_lst <- lapply(women_2005_lst, function(x) {x[c(-1:-4)]})
lengths(women_2005_lst) # 4338

# Convert to dataframe
women_2005_t = as.data.frame(women_2005_lst)
women_2005_df <- data.frame(Place = substr(women_2005_t[5:4337,],1,5),
                            DivTot =substr(women_2005_t[5:4337,],7,15),
                            Name =substr(women_2005_t[5:4337,],17,38),
                            Ag =substr(women_2005_t[5:4337,],40,41),
                            Hometown =substr(women_2005_t[5:4337,],43,60),
                            Net =substr(women_2005_t[5:4337,],62,68),
                            Gun =substr(women_2005_t[5:4337,],70,76),
                            Pace =substr(women_2005_t[5:4337,],78,82))

head(women_2005_df)
tail(women_2005_df)


# 2006
women_2006_lst <- womenTables[8]
women_2006_lst <- lapply(women_2006_lst, function(x) {x[c(-1:-4)]})
lengths(women_2006_lst) # 5441

# Convert to dataframe
women_2006_t = as.data.frame(women_2006_lst)
women_2006_df <- data.frame(Place = substr(women_2006_t[5:5439,],1,5),
                            DivTot =substr(women_2006_t[5:5439,],7,14),
                            Num =substr(women_2006_t[5:5439,],16,21),
                            Name =substr(women_2006_t[5:5439,],23,44),
                            Ag =substr(women_2006_t[5:5439,],46,47),
                            Hometown =substr(women_2006_t[5:5439,],49,64),
                            NetTime =substr(women_2006_t[5:5439,],65,72),
                            GunTime =substr(women_2006_t[5:5439,],73,80),
                            Pace =substr(women_2006_t[5:5439,],82,87),
                            S =substr(women_2006_t[5:5439,],88,88))

head(women_2006_df)
tail(women_2006_df)


# 2007
women_2007_lst <- womenTables[9]
women_2007_lst <- lapply(women_2007_lst, function(x) {x[c(-1:-3)]})
lengths(women_2007_lst) # 5695

women_2007_t = as.data.frame(women_2007_lst)
women_2007_df <- data.frame(Place = substr(women_2007_t[5:5694,],1,5),
                            DivTot =substr(women_2007_t[5:5694,],7,17),
                            Num =substr(women_2007_t[5:5694,],19,24),
                            Name =substr(women_2007_t[5:5694,],26,47),
                            Ag =substr(women_2007_t[5:5694,],49,50),
                            Hometown =substr(women_2007_t[5:5694,],52,69),
                            Time =substr(women_2007_t[5:5694,],71,77),
                            Pace =substr(women_2007_t[5:5694,],79,84),
                            S =substr(women_2007_t[5:5694,],86,86),
                            Split =substr(women_2007_t[5:5694,],88,94))

head(women_2007_df)
tail(women_2007_df)


# 2008
women_2008_lst <- womenTables[10]
women_2008_lst <- lapply(women_2008_lst, function(x) {x[c(-1:-4)]})
lengths(women_2008_lst) # 6401

women_2008_t = as.data.frame(women_2008_lst)
women_2008_df <- data.frame(Place = substr(women_2008_t[5:6401,],1,5),
                            DivTot =substr(women_2008_t[5:6401,],7,17),
                            Num =substr(women_2008_t[5:6401,],19,24),
                            Name =substr(women_2008_t[5:6401,],26,47),
                            Ag =substr(women_2008_t[5:6401,],49,50),
                            Hometown =substr(women_2008_t[5:6401,],52,69),
                            FiveMile =substr(women_2008_t[5:6401,],71,77),
                            Pace =substr(women_2008_t[5:6401,],79,83),
                            TenKM =substr(women_2008_t[5:6401,],85,91),
                            Pace2 =substr(women_2008_t[5:6401,],94,97),
                            Time =substr(women_2008_t[5:6401,],99,105),
                            Pace3 =substr(women_2008_t[5:6401,],107,111))

head(women_2008_df)
tail(women_2008_df)




# 2009
women_2009_lst <- womenTables[11]
women_2009_lst <- lapply(women_2009_lst, function(x) {x[c(-1:-4)]})
lengths(women_2009_lst) # 8329

women_2009_t = as.data.frame(women_2009_lst)
women_2009_df <- data.frame(Place = substr(women_2009_t[5:8327,],1,5),
                            DivTot =substr(women_2009_t[5:8327,],7,17),
                            Num =substr(women_2009_t[5:8327,],19,24),
                            Name =substr(women_2009_t[5:8327,],26,47),
                            Ag =substr(women_2009_t[5:8327,],49,50),
                            Hometown =substr(women_2009_t[5:8327,],52,71),
                            Gun_Time =substr(women_2009_t[5:8327,],73,79),
                            NetTime =substr(women_2009_t[5:8327,],81,87),
                            Pace =substr(women_2009_t[5:8327,],89,94),
                            S =substr(women_2009_t[5:8327,],96,96))

head(women_2009_df)
tail(women_2009_df)



### 2010
women_2010_lst <- womenTables[12]
women_2010_lst <- lapply(women_2010_lst, function(x) {x[c(-1:-4)]})
lengths(women_2010_lst) # 8859

# Convert to dataframe
women_2010_t = as.data.frame(women_2010_lst)
women_2010_df <- data.frame(Place = substr(women_2010_t[5:8857,],1,5),
                            DivTot =substr(women_2010_t[5:8857,],7,17),
                            Num =substr(women_2010_t[5:8857,],19,24),
                            Name =substr(women_2010_t[5:8857,],26,47),
                            Ag =substr(women_2010_t[5:8857,],49,50),
                            Hometown =substr(women_2010_t[5:8857,],52,71),
                            Five_Mile =substr(women_2010_t[5:8857,],73,79),
                            GunTime =substr(women_2010_t[5:8857,],81,87),
                            NetTime =substr(women_2010_t[5:8857,],89,96),
                            Pace =substr(women_2010_t[5:8857,],98,102),
                            S =substr(women_2010_t[5:8857,],104,104))

head(women_2010_df)
tail(women_2010_df)

#### 2011
women_2011_lst <- womenTables[13]
women_2011_lst <- lapply(women_2011_lst, function(x) {x[c(-1:-4)]})
lengths(women_2011_lst) # 9034

# Convert to dataframe
women_2011_t = as.data.frame(women_2011_lst)
women_2011_df <- data.frame(Place = substr(women_2011_t[5:9034,],1,5),
                            DivTot =substr(women_2011_t[5:9034,],7,17),
                            Num =substr(women_2011_t[5:9034,],19,24),
                            Name =substr(women_2011_t[5:9034,],26,47),
                            Ag =substr(women_2011_t[5:9034,],49,50),
                            Hometown =substr(women_2011_t[5:9034,],52,71),
                            Five_Mile =substr(women_2011_t[5:9034,],73,79),
                            Time =substr(women_2011_t[5:9034,],81,87),
                            NetTime =substr(women_2011_t[5:9034,],89,96),
                            Pace =substr(women_2011_t[5:9034,],98,101),
                            S =substr(women_2011_t[5:9034,],103,103))

head(women_2011_df)
tail(women_2011_df)


### 2012
women_2012_lst <- womenTables[14]
women_2012_lst <- lapply(women_2012_lst, function(x) {x[c(-1,-2,-3,-4)]})
lengths(women_2012_lst) # 9733

# Convert to dataframe
women_2012_t = as.data.frame(women_2012_lst)
women_2012_df <- data.frame(Place = substr(women_2012_t[5:9733,],1,5),
                  DivTot =substr(women_2012_t[5:9733,],7,17),
                  Num =substr(women_2012_t[5:9733,],19,24),
                  Name =substr(women_2012_t[5:9733,],26,47),
                  Ag =substr(women_2012_t[5:9733,],49,50),
                  Hometown =substr(women_2012_t[5:9733,],52,71),
                  Five_Mile =substr(women_2012_t[5:9733,],73,79),
                  Time =substr(women_2012_t[5:9733,],81,87),
                  Pace =substr(women_2012_t[5:9733,],89,93),
                  S =substr(women_2012_t[5:9733,],95,95))

head(women_2012_df)
tail(women_2012_df)



head(women_1999_df)
head(women_2000_df)
head(women_2001_df)
head(women_2002_df)
head(women_2003_df)
head(women_2004_df)
head(women_2005_df)
head(women_2006_df)
head(women_2007_df)
head(women_2008_df)
head(women_2009_df)
head(women_2010_df)
head(women_2011_df)
head(women_2012_df)

# Function to convert time to a decimal so we can interpret it
convertTime = function(time) {
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  })
}


# Time / 10 = pace
# Combine all dataframes into a single common dataframe
women_1999_df[,"Year"] <- "1999"
women_1999_df[,"Net"] <- women_1999_df[,"Time"]
women_1999_df <- women_1999_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_1999_df$Ag <- as.numeric(women_1999_df$Ag)
# Time conversion
women_1999_df$Net <- as.character(women_1999_df$Net)
women_1999_df$Net_Conv = convertTime(women_1999_df$Net)
women_1999_df <- na.omit(women_1999_df)

women_2000_df[,"Year"] <- "2000"
women_2000_df[,"Pace"] <- "0"
women_2000_df <- women_2000_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2000_df$Ag <- as.numeric(women_2000_df$Ag)
# Time conversion
women_2000_df$Net <- as.character(women_2000_df$Net)
women_2000_df$Net_Conv = convertTime(women_2000_df$Net)
women_2000_df <- na.omit(women_2000_df)

women_2001_df[,"Year"] <- "2001"
women_2001_df[,"DivTot"] <- "0"
women_2001_df[,"Pace"] <- "0"
women_2001_df <- women_2001_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2001_df$Ag <- as.numeric(women_2001_df$Ag)
# Time conversion
women_2001_df$Net <- as.character(women_2001_df$Net)
women_2001_df$Net_Conv = convertTime(women_2001_df$Net)
women_2001_df <- na.omit(women_2001_df)

women_2002_df[,"Year"] <- "2002"
women_2002_df[,"DivTot"] <- "0"
women_2002_df[,"Pace"] <- "0"
women_2002_df <- women_2002_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2002_df$Ag <- as.numeric(women_2002_df$Ag)
# Time conversion
women_2002_df$Net <- as.character(women_2002_df$Net)
women_2002_df$Net_Conv = convertTime(women_2002_df$Net)
women_2002_df <- na.omit(women_2002_df)

women_2003_df[,"Year"] <- "2003"
women_2003_df[,"Pace"] <- "0"
women_2003_df[,"Net"] <- women_2003_df[,"NetTime"]
women_2003_df <- women_2003_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2003_df$Ag <- as.numeric(women_2003_df$Ag)
# Time conversion
women_2003_df$Net <- as.character(women_2003_df$Net)
women_2003_df$Net_Conv = convertTime(women_2003_df$Net)
women_2003_df <- na.omit(women_2003_df)

women_2004_df[,"Year"] <- "2004"
women_2004_df[,"Pace"] <- "0"
women_2004_df <- women_2004_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2004_df$Ag <- as.numeric(women_2004_df$Ag)
# Time conversion
women_2004_df$Net <- as.character(women_2004_df$Net)
women_2004_df$Net_Conv = convertTime(women_2004_df$Net)
women_2004_df <- na.omit(women_2004_df)

women_2005_df[,"Year"] <- "2005"
women_2005_df <- women_2005_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2005_df$Ag <- as.numeric(women_2005_df$Ag)
# Time conversion
women_2005_df$Net <- as.character(women_2005_df$Net)
women_2005_df$Net_Conv = convertTime(women_2005_df$Net)
women_2005_df <- na.omit(women_2005_df)

women_2006_df[,"Year"] <- "2006"
women_2006_df[,"Net"] <- women_2006_df[,"NetTime"]
women_2006_df <- women_2006_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2006_df$Ag <- as.numeric(women_2006_df$Ag)
# Time conversion
women_2006_df$Net <- as.character(women_2006_df$Net)
women_2006_df$Net_Conv = convertTime(women_2006_df$Net)
women_2006_df <- na.omit(women_2006_df)

women_2007_df[,"Year"] <- "2007"
women_2007_df[,"Net"] <- women_2007_df[,"Time"]
women_2007_df <- women_2007_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2007_df$Ag <- as.numeric(women_2007_df$Ag)
# Time conversion
women_2007_df$Net <- as.character(women_2007_df$Net)
women_2007_df$Net_Conv = convertTime(women_2007_df$Net)
women_2007_df <- na.omit(women_2007_df)

women_2008_df[,"Year"] <- "2008"
women_2008_df[,"Pace"] <- women_2008_df[,"Pace3"]
women_2008_df[,"Net"] <- women_2008_df[,"Time"]
women_2008_df <- women_2008_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2008_df$Ag <- as.numeric(women_2008_df$Ag)
# Time conversion
women_2008_df$Net <- as.character(women_2008_df$Net)
women_2008_df$Net_Conv = convertTime(women_2008_df$Net)
women_2008_df <- na.omit(women_2008_df)

women_2009_df[,"Year"] <- "2009"
women_2009_df[,"Net"] <- women_2009_df[,"NetTime"]
women_2009_df <- women_2009_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2009_df$Ag <- as.numeric(women_2009_df$Ag)
# Time conversion
women_2009_df$Net <- as.character(women_2009_df$Net)
women_2009_df$Net_Conv = convertTime(women_2009_df$Net)
women_2009_df <- na.omit(women_2009_df)

women_2010_df[,"Year"] <- "2010"
women_2010_df[,"Net"] <- women_2010_df[,"NetTime"]
women_2010_df <- women_2010_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2010_df$Ag <- as.numeric(women_2010_df$Ag)
# Time conversion
women_2010_df$Net <- as.character(women_2010_df$Net)
women_2010_df$Net_Conv = convertTime(women_2010_df$Net)
women_2010_df <- na.omit(women_2010_df)

women_2011_df[,"Year"] <- "2011"
women_2011_df[,"Net"] <- women_2011_df[,"Time"]
women_2011_df <- women_2011_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2011_df$Ag <- as.numeric(women_2011_df$Ag)
# Time conversion
women_2011_df$Net <- as.character(women_2011_df$Net)
women_2011_df$Net_Conv = convertTime(women_2011_df$Net)
women_2011_df <- na.omit(women_2011_df)

women_2012_df[,"Year"] <- "2012"
women_2012_df[,"Net"] <- women_2012_df[,"Time"]
women_2012_df <- women_2012_df[,c("Year","Place","DivTot","Name","Ag","Hometown","Net","Pace")]
women_2012_df$Ag <- as.numeric(women_2012_df$Ag)
# Time conversion
women_2012_df$Net <- as.character(women_2012_df$Net)
women_2012_df$Net_Conv = convertTime(women_2012_df$Net)
women_2012_df <- na.omit(women_2012_df)

# Combined women dataframe
women_combined_df <- bind_rows(women_1999_df
                              ,women_2000_df
                              ,women_2001_df
                              ,women_2002_df
                              ,women_2003_df
                              ,women_2004_df
                              ,women_2005_df
                              ,women_2006_df
                              ,women_2007_df
                              ,women_2008_df
                              ,women_2009_df
                              ,women_2010_df
                              ,women_2011_df
                              ,women_2012_df)

# Check the format of each column in the dataframe
sapply(women_combined_df, class)  

head(women_combined_df)
tail(women_combined_df)

# Box Plots
# A lot of younger kids in 2006 many on the tail end of race times
ggplot(women_combined_df, aes(x=Year, y=Ag)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  ggtitle("Women's Racers Ages - All Years")

# Distribution of ages - we can see a bimodal distribution here, with a large portion of younger runners primarily in the 2006 year
# We know it's in the 2006 year from the box plot
  ggplot(women_combined_df) + 
    geom_density(aes(x = Ag), alpha = 0.7,color="green", fill="lightgreen") + 
    ggtitle("Women's Racers Age Distribution - All Years") + 
    geom_vline(aes(xintercept=mean(Ag)), color="red", linetype="dashed", size=1)
  
  # Mean years of runners per race year
  # Total - 22.66 years old
  mean(women_combined_df$Ag)
  
  # 1999 - 24.84
  mean(women_1999_df$Ag)
  
  # 2000 - 22.54
  mean(women_2000_df$Ag)
  
  # 2001 - 23.80
  mean(women_2001_df$Ag)
  
  # 2002 - 24.12
  mean(women_2002_df$Ag)
  
  # 2003 - 23.04
  mean(women_2003_df$Ag)
  
  # 2004 - 22.54
  mean(women_2004_df$Ag)
  
  # 2005 - 24.12
  mean(women_2005_df$Ag)
  
  # 2006 - 14.46
  # Confirms the skew is from 2006 primarily
  mean(women_2006_df$Ag)
  
  # 2007 - 22.51
  mean(women_2007_df$Ag)
  
  # 2008 - 22.20
  mean(women_2008_df$Ag)
  
  # 2009 - 25.07
  mean(women_2009_df$Ag)
  
  # 2010 - 22.29
  mean(women_2010_df$Ag)
  
  # 2011 - 24.73
  mean(women_2011_df$Ag)
  
  # 2012 - 21.87
  mean(women_2012_df$Ag)

  # If we re-plot the age distribution and ignore all ages younger than 18 due to the 2006 outlier, we get this distribution
  # There is some evidence that racers' ages are getting younger, but take note of the two outlier years in 2007 and 2002 where
  # ages were a bit older
  Year_Sel <- c('1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012')
  women_combined_df %>% 
    filter(Ag > 18 & Year == Year_Sel) %>% 
    ggplot() + 
    geom_density(aes(x = Ag,group = factor(Year), fill=factor(Year), alpha = 0.5)) + 
    ggtitle("Women's Racers Age Distribution - All Years - Distribution") + 
    labs(fill = "Year") +
    geom_vline(aes(xintercept=mean(Ag)), color="red", linetype="dashed", size=1)

  # Scatter Plot - this plot, filtered for ages > 18, clearly shows a stronger concentration of ages in the 2009 - 2012 year range compared
  # to previous years
  women_combined_df %>% 
    filter(Ag > 18 & Year == Year_Sel) %>% 
    ggplot(map=aes(Year,Ag))+
    geom_point()+
    geom_jitter() + 
    ggtitle("Women's Racers Ages - All Years - Scatter Plot")
  
  ### Time
  
  # Box Plots - Net Race Times
  # Mean times are increasing - big outlier in 2011
  ggplot(women_combined_df, aes(x=Year, y=Net_Conv)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4) +
    ggtitle("Women's Racers Net Times - All Years")

  # Mean times of runners per race year
  # Total - 99.31 mins
  mean(women_combined_df$Net_Conv)
  
  # 1999 - 95.49
  mean(women_1999_df$Net_Conv)
  
  # 2000 - 93.91
  mean(women_2000_df$Net_Conv)
  
  # 2001 - 94.65
  mean(women_2001_df$Net_Conv)
  
  # 2002 - 95.94
  mean(women_2002_df$Net_Conv)
  
  # 2003 - 95.54
  mean(women_2003_df$Net_Conv)
  
  # 2004 - 96.70
  mean(women_2004_df$Net_Conv)
  
  # 2005 - 98.59
  mean(women_2005_df$Net_Conv)
  
  # 2006 - 98.15
  mean(women_2006_df$Net_Conv)
  
  # 2007 - 97.16
  mean(women_2007_df$Net_Conv)
  
  # 2008 - 98.77
  mean(women_2008_df$Net_Conv)
  
  # 2009 - 98.80
  mean(women_2009_df$Net_Conv)
  
  # 2010 - 99.92
  mean(women_2010_df$Net_Conv)
  
  # 2011 - 109.90
  mean(women_2011_df$Net_Conv)
  
  # 2012 - 99.02
  mean(women_2012_df$Net_Conv)
  
  # Women's race time distribution
  # We can see times are generally the same across years, except for the outlier year in 2011 which was slightly higher
  # Race times have not changed over time, with the exception of the 2011 year
  Year_Sel <- c('1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012')
  women_combined_df %>% 
    filter(Net_Conv > 18 & Year == Year_Sel) %>% 
    ggplot() + 
    geom_density(aes(x = Net_Conv,group = factor(Year), fill=factor(Year), alpha = 0.5)) + 
    ggtitle("Women's Race Time - All Years - Distribution") + 
    labs(fill = "Year") +
    geom_vline(aes(xintercept=mean(Ag)), color="red", linetype="dashed", size=1)
  
  # group women's ages into groups
  women_combined_df$Ag_grp <- ifelse(women_combined_df$Ag >= 0 & women_combined_df$Ag <= 17, '0-17',
                    ifelse(women_combined_df$Ag >=18 & women_combined_df$Ag <=29, '18-29',
                           ifelse(women_combined_df$Ag >=30, '30+', 'undefined')))
  
  # Age group data frames
  women_combined_df_17 <- women_combined_df[women_combined_df$Ag_grp == '0-17',]
  women_combined_df_29 <- women_combined_df[women_combined_df$Ag_grp == '18-29',]
  women_combined_df_30 <- women_combined_df[women_combined_df$Ag_grp == '30+',]
  women_combined_df_unf <- women_combined_df[women_combined_df$Ag_grp == 'undefined',]
  
  # Box Plots - Net Race Times - age buckets - 17
  ggplot(women_combined_df_17, aes(x=Year, y=Net_Conv)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4) +
    ggtitle("Women's Racers Net Times - 0-17 Years Old - All Years")
  mean(women_combined_df_17$Net_Conv)
  # Mean - 97.69
  
  # Box Plots - Net Race Times - age buckets - 29
  ggplot(women_combined_df_29, aes(x=Year, y=Net_Conv)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4) +
    ggtitle("Women's Racers Net Times - 18-29 Years Old - All Years")
  mean(women_combined_df_29$Net_Conv)
  # Mean - 99.15
  
  # Box Plots - Net Race Times - age buckets - 30
  ggplot(women_combined_df_30, aes(x=Year, y=Net_Conv)) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4) +
    ggtitle("Women's Racers Net Times - 30+ Years Old - All Years")
  mean(women_combined_df_30$Net_Conv)
  # Mean - 102.02
  
  # Two conclusions here:
  # 1 - as racers age, their times decrease
  # 2 - across all three age groups race times are increasing, gradually, year over year