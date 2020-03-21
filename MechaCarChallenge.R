## (1) MPG REGRESSION 
#Read MechaCar data
mecha_car <- read.csv('MechaCar_mpg.csv')
#Perform mulitple linear regression with continuous data
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=mecha_car)
#Get r-squared and p-value
##  #Perfrom MULTIPLE LINEAR REGRESSION
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=mecha_car))   #generate summary statistics



## (2) SUSPENSION COIL SUMMARY
# Read file
suspension_coil <- read.csv(file='Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
# Create summary statistics table for suspension coil's PSI
summary_table <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarise(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variane=var(PSI), Standard_Deviation=sd(PSI))


## (3)SUSPENSION COIL T-Test
# Create subsets
#Filter suspension_coil data by Lot
sample_lot1 <- suspension_coil %>%
  filter(Manufacturing_Lot=='Lot1')
sample_lot2 <- suspension_coil %>%
  filter(Manufacturing_Lot=='Lot2')
sample_lot3 <- suspension_coil %>%
  filter(Manufacturing_Lot=='Lot3')
#Calculate Independent (one Sample) T-Test
t.test(sample_lot1$PSI, mu=1500)
t.test(sample_lot2$PSI, mu=1500)
t.test(sample_lot3$PSI, mu=1500)