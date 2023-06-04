# Benford-data
Benford Covid data for the research work on "How dependable is World Continental COVID-19 data? Disclosure of Inconsistencies in Daily Reportage Confirmed Cases and Deaths"
### The following are the packages used for this analysis
library(benford.analysis)
library(BenfordTests)
library(plotrix)
library(dgof)
library(readxl)
library(xlsx)
library(openxlsx)
library(kSamples)
library(xtable)
library(graphics)
library(ggplot2)

### First-Digit Test
####################################################################
###################################################################

#########R codes for First digits test

pop<-read.csv("C:\\Users\\EDMONDO EL FREDONDO\\Dropbox\\My PC (EDMONDO-el-FREDONDO)\\Desktop\\EDMONDO EL FREDONDO\\PHD_MANUSCRIPT\\MANUSCRIPTS\\Benford and COVID-19 Manuscript\\Data_Codes\\covid_data.csv")
View(pop)

## turn dataset into a vector
#unlist function turns it into a vector other than as.vector
COVID.vec = na.omit(unlist(pop[1:193,6]))
length(COVID.vec)

## function that extracts the first digit of all numbers computed
bben <- function(k){
  as.numeric(head(strsplit(as.character(k),'')[[1]],n=1))
}

## extract the first digits of COVID's data
digits = c()
n = length(COVID.vec)
for(i in 1:n){
  digits[i] = bben(COVID.vec[i])
}


## gather the sum of observed values for each digit (1-9)
observed = c()
for (i in 1:9){
  observed[i] = length(which(digits == i))
}

benford = as.vector(pbenf(1))
expected = c()
for (i in 1:9){
  expected[i] = n*benford[i]
}
## Pearson's chi-square proportions
chi.sq = c()
for(i in 1:9){
  chi.sq[i] = ((observed[i] - expected[i])^2)/expected[i]
}


## percentage observed of each digit
## (n-1) because one of the values in the data set is 0
pct = c()
for(i in 1:9){
  pct[i] = observed[i]/(n-1)
}


######BENFORD GRAPHS IN R

benlaw <- function(d) log10(1 + 1 / d)
digits <- 1:9
baseBarplot <- barplot(benlaw(digits), names.arg = digits, xlab = "First Digits", ylab = "Proportions",
                       ylim = c(0, .35))

lines(x = baseBarplot[,1], y = pct, col = "789", lwd = 2, 
      type = "b", pch = 23, cex = 1.5, bg = "789")

lines(x = baseBarplot[,1], y = benford, col = "red", lwd = 2, 
      type = "b", pch = 23, cex = 1.5, bg = "red")

# Add a legend
legend("topright",legend=c("Whole world", "Benford"),
       col=c("789","red"), lty=1:2, cex=0.8)


#### Psi-Factor
dfactor<- sqrt(sum((pct-benford)^2))/1.03606
dfactor

####alpha Statistic
alphastat<-sqrt(sum((pct-benford)^2))
alphastat

####omega statistic
omegastat<-max(abs(pct-benford))
omegastat

## Kolmogorov smirnov test for Benford
ks.benftest(COVID.vec,digits = 1,pvalmethod = "simulate", pvalsims = 10000)


## Pearson's chi-square test for Benford
chisq.benftest(COVID.vec,digits = 1, pvalmethod = "simulate", pvalsims = 10000)


########ESTATBLISHING CORRELATIONS AMONG VARIOUS TESTS
psicorr<-c(0.0738,0.1184,0.1332,0.1191,0.1217,0.1716,0.1350,0.1586,0.4279,0.5403,
           0.3804,0.4560,0.1516,0.1004,0.1392,0.0968,0.1738,0.2337,0.1714,0.1887,
           0.2935,0.2180,0.1978,0.2417)
alphacorr<-c(0.0765,0.1227,0.1370,0.1234,0.1260,0.1778,0.1399,0.1643,0.4434,0.5598,
             0.3941,0.4725,0.1570,0.1040,0.1442,0.1003,0.1801,0.2421,0.1775,0.1955,
             0.3041,0.2258,0.2049,0.2504)
omegacorr<-c(0.0421,0.1095,0.0808,0.096,0.0792,0.1273,0.1005,0.1091,0.2990,0.4906,
             0.2990,0.3488,0.1106,0.0760,0.0708,0.0524,0.0959,0.1990,0.0954,0.1027,
             0.2177,0.1343,0.1156,0.1990)
Kolmogorovcorr<-c(0.2482,0.7817,0.5535,0.3819,0.4979,0.5071,0.4979,0.5632,0.8725,
                  0.7959,0.4874,0.9874,0.7710,0.5362,0.3760,0.5149,0.5423,0.9377,
                  0.4341,0.8436,0.8080,0.5307,0.3254,0.5787)

##################### Correlations between the Statistic used #########################################
# Generate sample data
x <- psicorr
y <- alphacorr

# Create a data frame
data <- data.frame(x = x, y = y)

# Perform linear regression
model <- lm(y ~ x, data = data)

# Get R-squared value
r_squared <- summary(model)$r.squared

# Create scatter plot with regression line
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "", x = "Psi Factor", y = "Alpha Statistic") +
  annotate("text", x = max(x), y = max(y), label = paste("R-squared =", round(r_squared, 4)), hjust = 1, vjust = 0)


