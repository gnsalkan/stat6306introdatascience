# DataScienceHW5
Gunes Alkan  
October 14, 2015  


Now, let's read in data first data set GDP, and manipulate it:


```r
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileURL, destfile="gdp.csv")
gdp <- read.csv(file="gdp.csv", header=T)
names(gdp)
```

```
##  [1] "X"                           "Gross.domestic.product.2012"
##  [3] "X.1"                         "X.2"                        
##  [5] "X.3"                         "X.4"                        
##  [7] "X.5"                         "X.6"                        
##  [9] "X.7"                         "X.8"
```

```r
head(gdp)
```

```
##     X Gross.domestic.product.2012 X.1           X.2          X.3 X.4 X.5
## 1                                  NA                                 NA
## 2                                  NA               (millions of      NA
## 3                         Ranking  NA       Economy  US dollars)      NA
## 4                                  NA                                 NA
## 5 USA                           1  NA United States  16,244,600       NA
## 6 CHN                           2  NA         China   8,227,103       NA
##   X.6 X.7 X.8
## 1  NA  NA  NA
## 2  NA  NA  NA
## 3  NA  NA  NA
## 4  NA  NA  NA
## 5  NA  NA  NA
## 6  NA  NA  NA
```

```r
#skip first 4 lines
gdp <- read.csv(file="gdp.csv", skip=4) # or the next one works the same
head(gdp)
```

```
##     X X.1 X.2            X.3          X.4 X.5 X.6 X.7 X.8 X.9
## 1 USA   1  NA  United States  16,244,600       NA  NA  NA  NA
## 2 CHN   2  NA          China   8,227,103       NA  NA  NA  NA
## 3 JPN   3  NA          Japan   5,959,718       NA  NA  NA  NA
## 4 DEU   4  NA        Germany   3,428,131       NA  NA  NA  NA
## 5 FRA   5  NA         France   2,612,878       NA  NA  NA  NA
## 6 GBR   6  NA United Kingdom   2,471,784       NA  NA  NA  NA
```

```r
gdp <- read.csv(file="gdp.csv", skip=5, header=F)
head(gdp)
```

```
##    V1 V2 V3             V4           V5 V6 V7 V8 V9 V10
## 1 USA  1 NA  United States  16,244,600     NA NA NA  NA
## 2 CHN  2 NA          China   8,227,103     NA NA NA  NA
## 3 JPN  3 NA          Japan   5,959,718     NA NA NA  NA
## 4 DEU  4 NA        Germany   3,428,131     NA NA NA  NA
## 5 FRA  5 NA         France   2,612,878     NA NA NA  NA
## 6 GBR  6 NA United Kingdom   2,471,784     NA NA NA  NA
```

```r
# say R that when it sees these two .. and Not Available, R will put NA
gdp <- read.csv(file="gdp.csv", skip=5, header=F, na.strings=c("..", "Not Available."))
str(gdp)
```

```
## 'data.frame':	326 obs. of  10 variables:
##  $ V1 : Factor w/ 229 levels "","ABW","ADO",..: 215 38 102 51 68 72 28 174 99 93 ...
##  $ V2 : Factor w/ 194 levels "",".. Not available.  ",..: 3 104 115 126 137 148 159 170 181 4 ...
##  $ V3 : logi  NA NA NA NA NA NA ...
##  $ V4 : Factor w/ 229 levels "","  East Asia & Pacific",..: 218 51 107 82 77 217 37 172 105 98 ...
##  $ V5 : Factor w/ 204 levels ""," 1,008 "," 1,129 ",..: 40 178 143 100 66 63 61 58 57 16 ...
##  $ V6 : Factor w/ 7 levels "","a","b","c",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ V7 : logi  NA NA NA NA NA NA ...
##  $ V8 : logi  NA NA NA NA NA NA ...
##  $ V9 : logi  NA NA NA NA NA NA ...
##  $ V10: logi  NA NA NA NA NA NA ...
```

```r
#help(read.csv)
gdp <- read.csv(file="gdp.csv", skip=5, header=F, na.strings=c("..", "Not available.", "..Not available."), stringsAsFactors=FALSE)
str(gdp)
```

```
## 'data.frame':	326 obs. of  10 variables:
##  $ V1 : chr  "USA" "CHN" "JPN" "DEU" ...
##  $ V2 : chr  "1" "2" "3" "4" ...
##  $ V3 : logi  NA NA NA NA NA NA ...
##  $ V4 : chr  "United States" "China" "Japan" "Germany" ...
##  $ V5 : chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
##  $ V6 : chr  "" "" "" "" ...
##  $ V7 : logi  NA NA NA NA NA NA ...
##  $ V8 : logi  NA NA NA NA NA NA ...
##  $ V9 : logi  NA NA NA NA NA NA ...
##  $ V10: logi  NA NA NA NA NA NA ...
```

```r
#now everthing is in either logical or character not Factor
gdp <- dplyr::select(gdp, V1,V2,V4,V5)
str(gdp)
```

```
## 'data.frame':	326 obs. of  4 variables:
##  $ V1: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ V2: chr  "1" "2" "3" "4" ...
##  $ V4: chr  "United States" "China" "Japan" "Germany" ...
##  $ V5: chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
```

```r
names(gdp) <- c("CountryCode", "Ranking", "Economy", "GDP")
str(gdp)
```

```
## 'data.frame':	326 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : chr  "1" "2" "3" "4" ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
```

```r
#lets change ranking to numeric
gdp$Ranking <- as.numeric(gdp$Ranking)
```

```
## Warning: NAs introduced by coercion
```

```r
str(gdp)
```

```
## 'data.frame':	326 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
```

```r
summary(gdp)
```

```
##  CountryCode           Ranking         Economy              GDP           
##  Length:326         Min.   :  1.00   Length:326         Length:326        
##  Class :character   1st Qu.: 48.25   Class :character   Class :character  
##  Mode  :character   Median : 95.50   Mode  :character   Mode  :character  
##                     Mean   : 95.49                                        
##                     3rd Qu.:142.75                                        
##                     Max.   :190.00                                        
##                     NA's   :136
```

```r
tail(gdp, 40)
```

```
##     CountryCode Ranking Economy GDP
## 287                  NA            
## 288                  NA            
## 289                  NA            
## 290                  NA            
## 291                  NA            
## 292                  NA            
## 293                  NA            
## 294                  NA            
## 295                  NA            
## 296                  NA            
## 297                  NA            
## 298                  NA            
## 299                  NA            
## 300                  NA            
## 301                  NA            
## 302                  NA            
## 303                  NA            
## 304                  NA            
## 305                  NA            
## 306                  NA            
## 307                  NA            
## 308                  NA            
## 309                  NA            
## 310                  NA            
## 311                  NA            
## 312                  NA            
## 313                  NA            
## 314                  NA            
## 315                  NA            
## 316                  NA            
## 317                  NA            
## 318                  NA            
## 319                  NA            
## 320                  NA            
## 321                  NA            
## 322                  NA            
## 323                  NA            
## 324                  NA            
## 325                  NA            
## 326                  NA
```

```r
sum(is.na(gdp$Ranking))
```

```
## [1] 136
```

```r
dim(gdp)
```

```
## [1] 326   4
```

```r
326-136
```

```
## [1] 190
```

```r
summary (gdp[191:326,]) #to make sure it is alll NAs
```

```
##  CountryCode           Ranking      Economy              GDP           
##  Length:136         Min.   : NA   Length:136         Length:136        
##  Class :character   1st Qu.: NA   Class :character   Class :character  
##  Mode  :character   Median : NA   Mode  :character   Mode  :character  
##                     Mean   :NaN                                        
##                     3rd Qu.: NA                                        
##                     Max.   : NA                                        
##                     NA's   :136
```

```r
gdp <- gdp[1:190,]
str(gdp)
```

```
## 'data.frame':	190 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
```

```r
gdp2 <- gdp
gdp2$GDP <- as.numeric(gdp2$GDP)
```

```
## Warning: NAs introduced by coercion
```

```r
str(gdp2)
```

```
## 'data.frame':	190 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : num  NA NA NA NA NA NA NA NA NA NA ...
```

```r
#we had all NAs because of the commas between numbers, we need to remove them, try it on your test dataset gdp2
gdp2 <- gdp
gdp2$GDP <- gsub("," , "" ,gdp$GDP)
str(gdp2)
```

```
## 'data.frame':	190 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : chr  " 16244600 " " 8227103 " " 5959718 " " 3428131 " ...
```

```r
gdp2$GDP <- as.numeric(gdp2$GDP)
str(gdp2)
```

```
## 'data.frame':	190 obs. of  4 variables:
##  $ CountryCode: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ Ranking    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Economy    : chr  "United States" "China" "Japan" "Germany" ...
##  $ GDP        : num  16244600 8227103 5959718 3428131 2612878 ...
```

Now, let's manipulate education dataset:


```r
#read in data
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileURL, destfile="educ.csv")
educ <- read.csv(file="educ.csv", header=T)
names(educ)
```

```
##  [1] "CountryCode"                                      
##  [2] "Long.Name"                                        
##  [3] "Income.Group"                                     
##  [4] "Region"                                           
##  [5] "Lending.category"                                 
##  [6] "Other.groups"                                     
##  [7] "Currency.Unit"                                    
##  [8] "Latest.population.census"                         
##  [9] "Latest.household.survey"                          
## [10] "Special.Notes"                                    
## [11] "National.accounts.base.year"                      
## [12] "National.accounts.reference.year"                 
## [13] "System.of.National.Accounts"                      
## [14] "SNA.price.valuation"                              
## [15] "Alternative.conversion.factor"                    
## [16] "PPP.survey.year"                                  
## [17] "Balance.of.Payments.Manual.in.use"                
## [18] "External.debt.Reporting.status"                   
## [19] "System.of.trade"                                  
## [20] "Government.Accounting.concept"                    
## [21] "IMF.data.dissemination.standard"                  
## [22] "Source.of.most.recent.Income.and.expenditure.data"
## [23] "Vital.registration.complete"                      
## [24] "Latest.agricultural.census"                       
## [25] "Latest.industrial.data"                           
## [26] "Latest.trade.data"                                
## [27] "Latest.water.withdrawal.data"                     
## [28] "X2.alpha.code"                                    
## [29] "WB.2.code"                                        
## [30] "Table.Name"                                       
## [31] "Short.Name"
```

```r
str(educ)
```

```
## 'data.frame':	234 obs. of  31 variables:
##  $ CountryCode                                      : Factor w/ 234 levels "ABW","ADO","AFG",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ Long.Name                                        : Factor w/ 234 levels "American Samoa",..: 5 104 57 99 108 226 4 109 1 2 ...
##  $ Income.Group                                     : Factor w/ 6 levels "","High income: nonOECD",..: 2 2 4 5 6 2 6 5 6 6 ...
##  $ Region                                           : Factor w/ 8 levels "","East Asia & Pacific",..: 4 3 7 8 3 5 4 3 2 4 ...
##  $ Lending.category                                 : Factor w/ 4 levels "","Blend","IBRD",..: 1 1 4 4 3 1 3 2 1 3 ...
##  $ Other.groups                                     : Factor w/ 3 levels "","Euro area",..: 1 1 3 1 1 1 1 1 1 1 ...
##  $ Currency.Unit                                    : Factor w/ 155 levels "","Afghan afghani",..: 8 49 2 5 3 144 6 7 145 44 ...
##  $ Latest.population.census                         : Factor w/ 28 levels "","1970","1979",..: 17 28 3 2 18 22 18 18 17 18 ...
##  $ Latest.household.survey                          : Factor w/ 56 levels "","CPS (monthly)",..: 1 1 39 38 40 1 1 16 1 1 ...
##  $ Special.Notes                                    : Factor w/ 70 levels "","A simple multiplier is used to convert the national currencies of EMU members to euros. The following irrevocable euro conversi"| __truncated__,..: 1 1 27 1 1 1 1 1 1 63 ...
##  $ National.accounts.base.year                      : Factor w/ 44 levels "","1954","1973",..: 25 1 38 28 1 25 22 1 1 18 ...
##  $ National.accounts.reference.year                 : int  NA NA NA NA 1996 NA NA 1996 NA NA ...
##  $ System.of.National.Accounts                      : int  NA NA NA NA 1993 NA 1993 1993 NA NA ...
##  $ SNA.price.valuation                              : Factor w/ 3 levels "","VAB","VAP": 1 1 2 3 2 2 2 2 1 2 ...
##  $ Alternative.conversion.factor                    : Factor w/ 33 levels "","1960-85","1965-84",..: 1 1 1 24 1 1 6 21 1 1 ...
##  $ PPP.survey.year                                  : int  NA NA NA 2005 2005 NA 2005 2005 NA NA ...
##  $ Balance.of.Payments.Manual.in.use                : Factor w/ 3 levels "","BPM4","BPM5": 1 1 1 3 3 2 3 3 1 3 ...
##  $ External.debt.Reporting.status                   : Factor w/ 4 levels "","Actual","Estimate",..: 1 1 2 2 2 1 2 2 1 1 ...
##  $ System.of.trade                                  : Factor w/ 3 levels "","General","Special": 3 2 2 3 2 2 3 3 1 2 ...
##  $ Government.Accounting.concept                    : Factor w/ 3 levels "","Budgetary",..: 1 1 3 1 3 3 3 3 1 1 ...
##  $ IMF.data.dissemination.standard                  : Factor w/ 3 levels "","GDDS","SDDS": 1 1 2 2 2 2 3 3 1 2 ...
##  $ Source.of.most.recent.Income.and.expenditure.data: Factor w/ 77 levels "","1-2-3, 2005-06",..: 1 1 1 35 66 1 45 46 1 1 ...
##  $ Vital.registration.complete                      : Factor w/ 2 levels "","Yes": 1 2 1 1 2 1 2 2 2 2 ...
##  $ Latest.agricultural.census                       : Factor w/ 45 levels "","1960","1964-65",..: 1 1 1 3 32 32 41 1 1 1 ...
##  $ Latest.industrial.data                           : int  NA NA NA NA 2005 NA 2001 NA NA NA ...
##  $ Latest.trade.data                                : int  2008 2006 2008 1991 2008 2008 2008 2008 NA 2007 ...
##  $ Latest.water.withdrawal.data                     : int  NA NA 2000 2000 2000 2005 2000 2000 NA 1990 ...
##  $ X2.alpha.code                                    : Factor w/ 208 levels "","AD","AE","AF",..: 13 2 4 8 6 3 9 7 10 5 ...
##  $ WB.2.code                                        : Factor w/ 209 levels "","AD","AE","AF",..: 13 2 4 8 6 3 9 7 10 5 ...
##  $ Table.Name                                       : Factor w/ 234 levels "Afghanistan",..: 10 5 1 6 2 220 8 9 4 7 ...
##  $ Short.Name                                       : Factor w/ 234 levels "Afghanistan",..: 10 5 1 6 2 220 8 9 4 7 ...
```

```r
#select columns
educ <- dplyr::select(educ, CountryCode, Income.Group)
str(educ)
```

```
## 'data.frame':	234 obs. of  2 variables:
##  $ CountryCode : Factor w/ 234 levels "ABW","ADO","AFG",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ Income.Group: Factor w/ 6 levels "","High income: nonOECD",..: 2 2 4 5 6 2 6 5 6 6 ...
```

```r
tail(educ, 40)
```

```
##     CountryCode         Income.Group
## 195         STP  Lower middle income
## 196         SUR  Upper middle income
## 197         SVK    High income: OECD
## 198         SVN    High income: OECD
## 199         SWE    High income: OECD
## 200         SWZ  Lower middle income
## 201         SYC  Upper middle income
## 202         SYR  Lower middle income
## 203         TCA High income: nonOECD
## 204         TCD           Low income
## 205         TGO           Low income
## 206         THA  Lower middle income
## 207         TJK           Low income
## 208         TKM  Lower middle income
## 209         TMP  Lower middle income
## 210         TON  Lower middle income
## 211         TTO High income: nonOECD
## 212         TUN  Lower middle income
## 213         TUR  Upper middle income
## 214         TUV  Lower middle income
## 215         TZA           Low income
## 216         UGA           Low income
## 217         UKR  Lower middle income
## 218         UMC                     
## 219         URY  Upper middle income
## 220         USA    High income: OECD
## 221         UZB  Lower middle income
## 222         VCT  Upper middle income
## 223         VEN  Upper middle income
## 224         VIR High income: nonOECD
## 225         VNM  Lower middle income
## 226         VUT  Lower middle income
## 227         WBG  Lower middle income
## 228         WLD                     
## 229         WSM  Lower middle income
## 230         YEM  Lower middle income
## 231         ZAF  Upper middle income
## 232         ZAR           Low income
## 233         ZMB           Low income
## 234         ZWE           Low income
```

we see that there are some countries whose income.group is missing such as in 228th observation.So,name them as NAs first:


```r
educ2 <- read.csv(file="educ.csv", na.strings=c(""))
educ2 <- dplyr::select(educ2, CountryCode, Income.Group)
# let's prove that it actually named empty ones as NA
educ2[228,]
```

```
##     CountryCode Income.Group
## 228         WLD         <NA>
```

now let's see how many missing ones we have:


```r
sum(is.na(educ2))
```

```
## [1] 24
```
let's make our variables character, instead of factor:


```r
educ2$CountryCode <- as.character(educ2$CountryCode)
educ2$Income.Group <- as.character(educ2$Income.Group)
str(educ2)
```

```
## 'data.frame':	234 obs. of  2 variables:
##  $ CountryCode : chr  "ABW" "ADO" "AFG" "AGO" ...
##  $ Income.Group: chr  "High income: nonOECD" "High income: nonOECD" "Low income" "Lower middle income" ...
```

Finally, merging two datasets:


```r
MergedData <- merge(x = gdp2, y=educ2, by = "CountryCode", all=TRUE)
names(MergedData)
```

```
## [1] "CountryCode"  "Ranking"      "Economy"      "GDP"         
## [5] "Income.Group"
```

```r
dim(MergedData)
```

```
## [1] 235   5
```

```r
str(MergedData)
```

```
## 'data.frame':	235 obs. of  5 variables:
##  $ CountryCode : chr  "ABW" "ADO" "AFG" "AGO" ...
##  $ Ranking     : num  161 NA 105 60 125 32 26 133 NA 172 ...
##  $ Economy     : chr  "Aruba" NA "Afghanistan" "Angola" ...
##  $ GDP         : num  2584 NA 20497 114147 12648 ...
##  $ Income.Group: chr  "High income: nonOECD" "High income: nonOECD" "Low income" "Lower middle income" ...
```

###### QUESTION 1 ######
HOW MANY OF THE IDS MATCH?
In order to find the answer, let's check dataset with no missing values


```r
### data set with all nonmissing values ###
MergedData2 <- na.omit(MergedData)
str(MergedData2)
```

```
## 'data.frame':	189 obs. of  5 variables:
##  $ CountryCode : chr  "ABW" "AFG" "AGO" "ALB" ...
##  $ Ranking     : num  161 105 60 125 32 26 133 172 12 27 ...
##  $ Economy     : chr  "Aruba" "Afghanistan" "Angola" "Albania" ...
##  $ GDP         : num  2584 20497 114147 12648 348595 ...
##  $ Income.Group: chr  "High income: nonOECD" "Low income" "Lower middle income" "Upper middle income" ...
##  - attr(*, "na.action")=Class 'omit'  Named int [1:46] 2 9 35 46 50 55 56 57 58 61 ...
##   .. ..- attr(*, "names")= chr [1:46] "2" "9" "35" "46" ...
```

# Answer 1: 
# Since there is 189 observations, we can clearly say that 189 of IDs matched.

And, in MergedData2 data frame, we have only non-missing values, since we dropped all NAs.



```r
#another way of showing this is 
sum(!is.na(unique(MergedData$GDP)))
```

```
## [1] 189
```

```r
#also, we can check the number of missings in each variable
sum(!is.na(unique(MergedData$Income.Group)))
```

```
## [1] 5
```

```r
sum(!is.na(unique(MergedData$CountryCode)))
```

```
## [1] 235
```

```r
sum(!is.na(unique(MergedData$Ranking)))
```

```
## [1] 189
```

```r
sum(!is.na(unique(MergedData$Economy)))
```

```
## [1] 190
```

```r
#We will continue with using MergedData2 data frame
```

###### QUESTION 2 ######
WHAT IS THE 13TH COUNTRY IN THE RESULTING DATA FRAME?


```r
#before sorting it, lets check if everything looks ok:
str(MergedData2)
```

```
## 'data.frame':	189 obs. of  5 variables:
##  $ CountryCode : chr  "ABW" "AFG" "AGO" "ALB" ...
##  $ Ranking     : num  161 105 60 125 32 26 133 172 12 27 ...
##  $ Economy     : chr  "Aruba" "Afghanistan" "Angola" "Albania" ...
##  $ GDP         : num  2584 20497 114147 12648 348595 ...
##  $ Income.Group: chr  "High income: nonOECD" "Low income" "Lower middle income" "Upper middle income" ...
##  - attr(*, "na.action")=Class 'omit'  Named int [1:46] 2 9 35 46 50 55 56 57 58 61 ...
##   .. ..- attr(*, "names")= chr [1:46] "2" "9" "35" "46" ...
```

```r
# SORT DATA FRAME IN ASCENDING ORDER BY GDP RANK
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
MergedData3 <- arrange(MergedData2,GDP)
head(MergedData3)
```

```
##   CountryCode Ranking               Economy GDP        Income.Group
## 1         TUV     190                Tuvalu  40 Lower middle income
## 2         KIR     189              Kiribati 175 Lower middle income
## 3         MHL     188      Marshall Islands 182 Lower middle income
## 4         PLW     187                 Palau 228 Upper middle income
## 5         STP     186 São Tomé and Principe 263 Lower middle income
## 6         FSM     185 Micronesia, Fed. Sts. 326 Lower middle income
```

```r
tail(MergedData3, 10)
```

```
##     CountryCode Ranking            Economy      GDP        Income.Group
## 180         IND      10              India  1841710 Lower middle income
## 181         ITA       9              Italy  2014670   High income: OECD
## 182         RUS       8 Russian Federation  2014775 Upper middle income
## 183         BRA       7             Brazil  2252664 Upper middle income
## 184         GBR       6     United Kingdom  2471784   High income: OECD
## 185         FRA       5             France  2612878   High income: OECD
## 186         DEU       4            Germany  3428131   High income: OECD
## 187         JPN       3              Japan  5959718   High income: OECD
## 188         CHN       2              China  8227103 Lower middle income
## 189         USA       1      United States 16244600   High income: OECD
```

```r
#So The United States is the last
head(MergedData3, 14)
```

```
##    CountryCode Ranking                        Economy GDP
## 1          TUV     190                         Tuvalu  40
## 2          KIR     189                       Kiribati 175
## 3          MHL     188               Marshall Islands 182
## 4          PLW     187                          Palau 228
## 5          STP     186          São Tomé and Principe 263
## 6          FSM     185          Micronesia, Fed. Sts. 326
## 7          TON     184                          Tonga 472
## 8          DMA     183                       Dominica 480
## 9          COM     182                        Comoros 596
## 10         WSM     181                          Samoa 684
## 11         VCT     180 St. Vincent and the Grenadines 713
## 12         GRD     178                        Grenada 767
## 13         KNA     178            St. Kitts and Nevis 767
## 14         VUT     177                        Vanuatu 787
##           Income.Group
## 1  Lower middle income
## 2  Lower middle income
## 3  Lower middle income
## 4  Upper middle income
## 5  Lower middle income
## 6  Lower middle income
## 7  Lower middle income
## 8  Upper middle income
## 9           Low income
## 10 Lower middle income
## 11 Upper middle income
## 12 Upper middle income
## 13 Upper middle income
## 14 Lower middle income
```

# Answer 2: 
# 13th country is St. Kitts and Nevis .

###### QUESTION 3 ######
WHAT ARE THE AVERAGE GDP RANKINGS FOR THE "High income: OECD" AND "High income: nonOECD" GROUPS?



```r
#MergedData3$Income.Group <- as.factor(MergedData3$Income.Group)
str(MergedData3)
```

```
## 'data.frame':	189 obs. of  5 variables:
##  $ CountryCode : chr  "TUV" "KIR" "MHL" "PLW" ...
##  $ Ranking     : num  190 189 188 187 186 185 184 183 182 181 ...
##  $ Economy     : chr  "Tuvalu" "Kiribati" "Marshall Islands" "Palau" ...
##  $ GDP         : num  40 175 182 228 263 326 472 480 596 684 ...
##  $ Income.Group: chr  "Lower middle income" "Lower middle income" "Lower middle income" "Upper middle income" ...
##  - attr(*, "na.action")=Class 'omit'  Named int [1:46] 2 9 35 46 50 55 56 57 58 61 ...
##   .. ..- attr(*, "names")= chr [1:46] "2" "9" "35" "46" ...
```

```r
summary(MergedData3)
```

```
##  CountryCode           Ranking         Economy               GDP          
##  Length:189         Min.   :  1.00   Length:189         Min.   :      40  
##  Class :character   1st Qu.: 48.00   Class :character   1st Qu.:    6972  
##  Mode  :character   Median : 95.00   Mode  :character   Median :   28242  
##                     Mean   : 95.31                      Mean   :  379597  
##                     3rd Qu.:143.00                      3rd Qu.:  205789  
##                     Max.   :190.00                      Max.   :16244600  
##  Income.Group      
##  Length:189        
##  Class :character  
##  Mode  :character  
##                    
##                    
## 
```

```r
#finding mean
tapply(MergedData3$Ranking,MergedData3$Income.Group, mean)
```

```
## High income: nonOECD    High income: OECD           Low income 
##             91.91304             32.96667            133.72973 
##  Lower middle income  Upper middle income 
##            107.70370             92.13333
```

# Answer 3:
# So Average GDP Ranking For "High income: OECD" : 32.96667  
# and Average GDP Ranking For "High income: nonOECD" : 91.91304 


###### QUESTION 4 ######
CUT THE GDP RANKINGS INTO 5 SEPERATE QUANTILE GROUPS. MAKE A TABLE VERSUS INCOME.GROUP.
HOW MANY COUNTRIES ARE LOWER MIDDLE INCOME BUT AMONG THE 38 NATIONS WITH THE HIGHEST GDP?



```r
#Here, we'll use Hmisc package and cut() function to cut it into quatile groups.
library(Hmisc)
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: Formula
## Loading required package: ggplot2
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, src, summarize
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
MergedData3$RKgr <- cut2(MergedData3$Ranking, g=5)
#change their names
GDPgrLabels <- c("Quantile 1", "Quantile 2", "Quantile 3", "Quantile 4", "Quantile 5")
MergedData3$RKgr <- factor(MergedData3$RKgr, labels=GDPgrLabels)
head(MergedData3)
```

```
##   CountryCode Ranking               Economy GDP        Income.Group
## 1         TUV     190                Tuvalu  40 Lower middle income
## 2         KIR     189              Kiribati 175 Lower middle income
## 3         MHL     188      Marshall Islands 182 Lower middle income
## 4         PLW     187                 Palau 228 Upper middle income
## 5         STP     186 São Tomé and Principe 263 Lower middle income
## 6         FSM     185 Micronesia, Fed. Sts. 326 Lower middle income
##         RKgr
## 1 Quantile 5
## 2 Quantile 5
## 3 Quantile 5
## 4 Quantile 5
## 5 Quantile 5
## 6 Quantile 5
```

```r
str(MergedData3)
```

```
## 'data.frame':	189 obs. of  6 variables:
##  $ CountryCode : chr  "TUV" "KIR" "MHL" "PLW" ...
##  $ Ranking     : num  190 189 188 187 186 185 184 183 182 181 ...
##  $ Economy     : chr  "Tuvalu" "Kiribati" "Marshall Islands" "Palau" ...
##  $ GDP         : num  40 175 182 228 263 326 472 480 596 684 ...
##  $ Income.Group: chr  "Lower middle income" "Lower middle income" "Lower middle income" "Upper middle income" ...
##  $ RKgr        : Factor w/ 5 levels "Quantile 1","Quantile 2",..: 5 5 5 5 5 5 5 5 5 5 ...
##  - attr(*, "na.action")=Class 'omit'  Named int [1:46] 2 9 35 46 50 55 56 57 58 61 ...
##   .. ..- attr(*, "names")= chr [1:46] "2" "9" "35" "46" ...
```

```r
summary(MergedData3)
```

```
##  CountryCode           Ranking         Economy               GDP          
##  Length:189         Min.   :  1.00   Length:189         Min.   :      40  
##  Class :character   1st Qu.: 48.00   Class :character   1st Qu.:    6972  
##  Mode  :character   Median : 95.00   Mode  :character   Median :   28242  
##                     Mean   : 95.31                      Mean   :  379597  
##                     3rd Qu.:143.00                      3rd Qu.:  205789  
##                     Max.   :190.00                      Max.   :16244600  
##  Income.Group               RKgr   
##  Length:189         Quantile 1:38  
##  Class :character   Quantile 2:38  
##  Mode  :character   Quantile 3:38  
##                     Quantile 4:38  
##                     Quantile 5:37  
## 
```

```r
table(  MergedData3$RKgr,  MergedData3$Income.Group)
```

```
##             
##              High income: nonOECD High income: OECD Low income
##   Quantile 1                    4                18          0
##   Quantile 2                    5                10          1
##   Quantile 3                    8                 1          9
##   Quantile 4                    5                 1         16
##   Quantile 5                    1                 0         11
##             
##              Lower middle income Upper middle income
##   Quantile 1                   5                  11
##   Quantile 2                  13                   9
##   Quantile 3                  12                   8
##   Quantile 4                   8                   8
##   Quantile 5                  16                   9
```

# Answer 4: 
# There are 5 Lower middle income countries among the 38 nations with the highest GDP.

















