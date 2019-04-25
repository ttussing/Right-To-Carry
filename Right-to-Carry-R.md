---
title: ''
output:
  word_document: default
  pdf_document: default
  html_document:
    fig_height: 3
    fig_width: 4
    highlight: textmate
    theme: cerulean
---

```{r setup, echo=FALSE, message=F, warning=F  }

#==============================================================================


# Clear the working space
  rm(list = ls())


  MyDir = "/Users/htussing/Dropbox/Older/Spring 2016/Econ173/data"

### Leave the rest of this chunk alone, unless you need to add a package

# Sundstrom directory (leave this alone!)
  SundstromDir <- "/Users/wsundstrom/Google Drive/Econ173/data"

# Choose correct working directory and proceed
  if (file.exists(SundstromDir)){
    WorkDir = SundstromDir
  } else {
    WorkDir = MyDir
  }
  
# Set working directory 
  setwd(MyDir)

### Load the packages (all must have been installed)
  library(knitr)
  library(AER)
  library(sandwich)
  library(lmtest)
  library(car)
  library(stargazer)
  library(ggplot2)
  library(dplyr)
  library(gdata)
  library(compareGroups)
  library(doBy)
  library(plm)
  library(ivpack)
  library(sampleSelection)
  library(foreign)
  library(downloader)
  library(repmis)
  
### More settings

# Root directory for knitr
  opts_knit$set(root.dir = WorkDir)

# turn off scientific notation except for big numbers
  options(scipen = 9)
# set font size for qplot (default is 12)
  theme_set(theme_gray(base_size = 12))

### functions for correct SEs in regression tables

# function to calculate corrected SEs for OLS regression 
  cse = function(reg) {
    rob = sqrt(diag(vcovHC(reg, type = "HC1")))
    return(rob)
  }
  
# clustered SEs, clustered on "group"... could also cluster on "time" 
# compute Stata-like degrees of freedom adjustment for number of groups
# See http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/

  clse = function(reg) { 
    # index(reg, "id") returns the id or entity variable vector 
    G = length(unique(index(reg,"id")))
    N = length(index(reg,"id"))
    dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
    rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                               cluster = "group")))
    return(rob)
  }
  
# corrected SEs for IV regressions
  ivse = function(reg) {
    rob = robust.se(reg)[,2]
    return(rob)
  }
  
```

<base target="_top"/>
  
##Right To Carry
###Hale Tussing
####June 6, 2016    


***


```{r, results='asis', message=F, warning=F, comment=NA, echo=F}
#Data

library(readstata13)

rtc <- read.dta13("rtc.dta")

#Create natural log variables
rtc$violentln <- log(rtc$violent_rate)

rtc$murderln <- log(rtc$murder_rate)

rtc$rapeln <- log(rtc$rape_rate)

rtc$robberyln <- log(rtc$robbery_rate)

rtc$aggrivatedln <- log(rtc$aggravated_rate)

rtc$propertyln <- log(rtc$property_rate)

rtc$burglaryln <- log(rtc$burglary_rate)

rtc$larcenyln <- log(rtc$larceny_rate)

rtc$autotheftln <- log(rtc$autotheft_rate)

#Shall Issue variable

rtc$shallissue <- ifelse(rtc$RTC <= rtc$year, c(1), c(0))

#Isolate MS and AL

rtcMSAL=subset(rtc, state_id=="AL" | state_id=="MS")




```

In 1997, John Lott and David Mustard produced a report based on their collected data that carried the claim:  Introducing a right to carry law will decrease the amount of violent crime in that particular state. Lott and Mustard based their findings using shall issue laws, “The results imply that ‘shall issue’ laws coincide with fewer murders, rapes, aggravated assaults, and rapes”[^1]. While I do not know why they stated ‘rapes’ twice, they made a very clear statement based on the data they believed to be sufficient. Since it was published, numerous reports manifested using data to argue both sides. Lott and Mustard made claims that reports such as the paper by McDowall suffers from their methodology. “they pick only three cities in Florida and one city each in Mississippi and Oregon (despite the provisions involving statewide laws), and they use neither the same sample period nor the same method of picking geographical areas for each of those cities”[^1]. Despite this complaint of theirs, many other papers have made similar claims of them. For instance, one piece aptly named “A Lott of Lies” gave their opinion on the data that Lott and Mustard used: “In an audacious display of cherry-picking, Lott argues that there were ‘more guns’ between 1977 to 1992 by choosing to examine two seemingly arbitrary surveys on gun ownership, and then sloppily applying a formula he devised to correct for survey limitations”[^3]. One of Lott and Mustard’s most significant claims was that, were states to have adopted right to carry laws in 1992, there would be 1,500 fewer murders, 4,000 fewer rapes, 11,000 fewer robberies and 60,000 fewer aggravated assaults. They then claim that this results in a social benefit of $6.6 billion[^1].
There were many obstacles that Lott and Mustard had to work around in order to obtain legitimate findings. One of the problems was that despite states changing laws, different counties will show different effects since some already allowed carrying. A solution was determining if the “shall issue” right to carry laws produced different effects between high and low population counties. Lott and Mustard were also criticized for not having data that was current, “Lott and Mustard’s period of analysis ended just before the extraordinary crime drop of the 1990s. They concluded that extending Lott and Mustard’s data set beyond 1992 undermined the MGLC hypothesis”[^2]. Including the attempts I made at presenting the crimes over a time series, it is clear that there was a significant decline in crimes due not to guns, but to the sharp decline in crack cocaine period. Had this data been a part of Lott and Mustard’s analysis, the More Guns Less Crime hypothesis might have been more difficult to prove.
Plassmann and Whitley have the idea to approach similar to what I want to do, they state that “The question is not whether these coefficients are different from zero, but whether they have changes relative to other coefficients”[^5]. I think this means that data is only important if it has context. Comparing two similar entities is what might be the best way to understand how they change.



Data:

The data Lott obtains originates from different areas depending on the level. State level data he obtained from the Crime Prevention Research Center. County data on arrests and offenses was collected from the Uniform Crime Report, but Lott also contacted the state departments of corrections, state attorneys general, state secretaries of state, and state police offices in every state for other variables.

Lott and Mustard telephoned law enforcement officials across the nation to collect conviction rates and mean prison sentence lengths, however only managed to get information from Arizona, Oregon and Washington. They also gained data from the FBI crime reports on seven categories: murder, rape, aggravated assault, robbery, auto theft, burglary and larceny, separated into the two types of crimes: violent and property. To make it more specific, their data was specific to include different races (white black and other) and different genders of 18 age ranges.

In the newer data, we were provided with the crack index, both adjusted and unadjusted [^2]. I think this can be valuable since one large complaint about Lott and Mustard’s findings was they did not acknowledge the period of time where crimes changed due to the crack cocaine epidemic. This bias was something I hoped to later solve for.

***

######_For this project, I wanted to stick to the crime variable I think is most important, the Log of the Violent Crime Rate._

***

###Mississippi and Alabama Violent Crime Rate Time Series
#####Mississippi Started RTC law in 1990
```{r, message=F, warning=F, comment=NA, echo=F}


qplot(year, violentln, data=subset(rtc, state_id=="MS" | state_id=="AL" | state_id=="LA"), 
      shape=state_id, color=state_id, 
      geom=c("point", "smooth"), main = "", 
      ylab = "Log of Violent Crime Rate", xlab = "Year")

```

<b/>


I wanted a clear visual representation of how two states that are similar geographically compare when only one changes to having a Right to Carry law. Alabama, which had had a RTC law since 1970, is consistently higher than Mississippi, which enacted this law in 1990. Beginning in 1985, both states followed a similar trend, and despite Mississippi changing the law in 1990, there seems to be little, if any difference in the slopes of the two states after that year. I also decided to add Louisiana, another state of similar type as Mississippi in this graph. I did so to prove the trends that Mississippi shares with Alabama is not unique to just those two states, but a trend of the area. Louisiana, which adopted the RTC laws in 1996 also shows no visible change on that year.
<b/>
Based only on this, it would seem that the passing of right to carry laws has almost no effect in Mississippi.



###Difference of Differences
I think using difference of differences on these two similar states is an effective way to show if there is an actual shift in the state's culture and violent crime rate after enacting a Right to Carry law. By comparing Mississippi with Alabama I was hoping to remove bias from many different possible sources in the data to isolate how the right to carry law might change a state.

```{r, message=F, warning=F, comment=NA, echo=F}
#Create variables for before and after 1990
rtcMSAL$vcrrtc1 <- rtcMSAL$year < 1990
rtcMSAL$vcrrtc2 <- rtcMSAL$year >= 1990


#Create subset of data for individual states
MSvcrrtc <- subset(rtcMSAL, state_id=="MS")
ALvcrrtc <- subset(rtcMSAL, state_id=="AL")

#Create subset of data for before and after 1990 in Mississippi and Alabama (total of 4)
MSvcrrtc1 <- subset(MSvcrrtc, year<1990)
ALvcrrtc1 <- subset(ALvcrrtc, year<1990)
MSvcrrtc2 <- subset(MSvcrrtc, year>=1990)
ALvcrrtc2 <- subset(ALvcrrtc, year>=1990)

#Find mean in Mississippi for before and after 1990
MSvcrrtcbf <- mean(MSvcrrtc1$violentln)
MSvcrrtcaf <- mean(MSvcrrtc2$violentln)


#Find Difference #1, Mississippi's mean of violent crime before and after 1990
DMSvcrrtc <- MSvcrrtcaf-MSvcrrtcbf


#Find mean in Alabama for before and after 1990
ALvcrrtcbf <- mean(ALvcrrtc1$violentln)
ALvcrrtcaf <- mean(ALvcrrtc2$violentln)

#Find Difference #2, Alabama's mean of violent crime before and after 1990
DALvcrrtc <- ALvcrrtcaf-ALvcrrtcbf


#Find Difference #3, the difference between Mississippi's change in mean from Alabama's change in mean
DiDMSALvcrrtc <- DALvcrrtc-DMSvcrrtc



```

I created subsets for Mississippi and Alabama and variables for before and after 1990 which is when Mississippi changed their RTC law. I first had to find the mean of the violent crimes before and after 1990. Before the change in law in Mississippi, log of violent crime rate was _`r MSvcrrtcbf`_, and after _`r MSvcrrtcaf`_. In Alabama, before Mississippi changed laws the log of the violent crime rate was _`r ALvcrrtcbf`_, and after was _`r ALvcrrtcaf`_. Before moving on, it was interesting to note that Alabama's violent crime rate was higher than in Mississippi, especially since Alabama has had RTC laws since before the start of this study. Afterwards, I found the difference of the mean of those two time periods in their respective states. In Mississippi this difference was _`r DMSvcrrtc`_, and in Alabama it was _`r DALvcrrtc`_. Mississippi’s difference being _`r DMSvcrrtc`_ means that the ratio of the difference and the before is _`r DMSvcrrtc/ MSvcrrtcbf`_. What follows is the percent change in the log of the violent crime rate, _`r (DMSvcrrtc/ MSvcrrtcbf)*100`%_. 
Finally, after comparing the difference of the states of their before and after change (the difference in differences), I found a little difference _`r DiDMSALvcrrtc`_. However, this difference is negative, which means that Mississippi has a slightly larger change than Alabama.
Looking at Mississippi, the first difference means that the violence crime was getting larger by _`r (DMSvcrrtc/ MSvcrrtcbf)*100`%_ after right to carry laws. The difference in differences and first difference shows that the RTC laws either did not affect Mississippi at all (when compared to a similar state untreated), or showed a larger slope comparatively, meaning an increase in crime since the law. Either way, this proves Lott and Mustard wrong in saying that right to carry laws decrease violent crimes.
<b/>
I was unable to use Louisiana in this because I set 1990 as the year of reference, and Louisiana changed their laws in 1996.



***



Lott and Mustard’s Table 4 showed fixed effects for years and states, however the variables they controlled for were not sufficient. Other than the shall issue dummy and the arrest rate, I included density, unemployment rate, per capita income and the adjusted crack index. I think density unemployment and per capita income are major players in violent crime rate, and I wanted to see how controlling for the crack epidemic changed the findings. I think that using these would remove endogeneity problems that were not addressed (on purpose or not) by Lott and Mustard. I was expecting that these four variables could remove bias from Lott and Mustard’s findings which his table did not reflect. Many crimes result from gang related activity, and the higher the density, the more gangs might have to interact. Looking at Lott and Mustard’s variables I do not think that it was necessary to have the specific ages, races, and genders so I will leave them out. After research it was difficult to find some of the variables' meanings. However, of the ones I understood such as the police officers were also unnecessary to include.

```{r, message=F, warning=F, comment=NA, echo=F}

#Table 4
#a) Violent Crime
reg1 = plm(violentln~ shallissue + violent_arrestrate + density + unemployment_rate + pci + crackindex_adjusted, data= rtc, index = c("state","year"), effect = "twoways", model = "within")

reg2 = plm(violentln~ shallissue + violent_arrestrate, data= rtc, index = c("state","year"), effect = "twoways", model = "within")

stargazer(reg1, reg2, 
          se=list(cse(reg1), cse(reg2)),
          title="State Level Crime Data", type="text", 
          df=FALSE, digits=3)
```
My results showed that PCI and unemployment were significant in the outcome of violent crime rate. As expected, both of these were negatively correlated with violent crime. Unfortunately, the adjusted crack index did not have as large of an effect as much as I thought it might.

***

Black and Nagin concluded saying that “Without Florida in the sample, there is no detectable impact for these two crimes that, by the calculations of Lott and Mustard, account for 80 percent of the social benefit of RTC laws”[^4]. Based on my use of data from 1977 through 2010 it seems to me that, if anything, violent crime rate increased in Mississippi after the right to carry laws were introduced in 1990. 
Before creating any number values, simply looking at the graphs of Mississippi and Alabama Violent Crime Rates gives the impression that there was little or no regression discontinuity in either state around that year. Just by the view, it seems Mississippi follows a very similar path as Alabama. This makes sense do to the geographical proximity. Beyond that I calculated the difference in differences of Mississippi and Alabama. The fact that both showed an increase in crime since the RTC laws went into effect in Mississippi disproves the claims by Lott, however on top of that, Mississippi showed more of a change than Alabama did, meaning that if RTC laws did have an effect, it would be significant only in a positive direction. States, and counties as Lott and Mustard have suggested, are all different. So while I have found that Mississippi did not benefit from these laws, Oregon for instance, which Lott has mentioned as an example, could benefit greatly. Like most things, states react differently to different outside forces, I attempted to compare two that are very similar in many ways to see the effects. However, this attempt would not have been reasonable if I tried comparing states that are on different coasts. This is a study that can confirm how it is necessary to have national, state, and county-specific laws.





[^1]: Lott, Jr. John R., and David B. Mustard. "Crime, Deterrence, and Right‐to‐Carry Concealed Handguns." The Journal of Legal Studies 26.1 (1997): 1-68. Web.

[^2]: Aneja, Abhay, John Donohue, and Alexandria Zhang. "The Impact of Right to Carry Laws and the NRC Report: The Latest Lessons for the Empirical Evaluation of Law and Policy." (2012): n. pag. Web.

[^3]: <http://www.armedwithreason.com/shooting-down-the-gun-lobbys-favorite-academic-a-lott-of-lies/> [Link]

[^4]: Black, DAN A., and Nagin Daniel S. "Do Right‐To‐Carry Laws Deter Violent Crime?" The Journal of Legal Studies 27.1 (1998): 209-19. Web.

[^5]: Plassmann, Florenz, and Whitley John. "Confirming "More Guns, Less Crime""Stanford Law Review 55.4 (2003): 1313-369. Web.

