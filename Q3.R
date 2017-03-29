library(ggplot2)
library(plotly)
library(broom)
library(gtools)
library(dplyr)

# Read the US data

all_content = readLines("2015.csv")
skip_second = all_content[-2]
data_2015 = read.csv(textConnection(skip_second), header = TRUE)

all_content = readLines("2013.csv")
skip_second = all_content[-2]
data_2013 = read.csv(textConnection(skip_second), header = TRUE)

all_content = readLines("2006.csv")
skip_second = all_content[-2]
data_2006 = read.csv(textConnection(skip_second), header = TRUE)


data_us <- smartbind(data_2015,data_2013,data_2006)
data_us <- data_us[-grep("Manufacturing", data_us$NAICS.display.label),]


remove(data_2006,data_2013,data_2015)

#saveRDS(data_us, file = "data_us.rds")

# Read State Data

all_content = readLines("2015_state_1.csv")
skip_second = all_content[-2]
data_state_2015_1 = read.csv(textConnection(skip_second), header = TRUE)
data_state_2015_1 <- data_state_2015_1[data_state_2015_1$YEAR.id == 2015,]

all_content = readLines("2015_state_2.csv")
skip_second = all_content[-2]
data_state_2015_2 = read.csv(textConnection(skip_second), header = TRUE)
data_state_2015_2 <- data_state_2015_2[data_state_2015_2$YEAR.id == 2015,]


all_content = readLines("2014_state_1.csv")
skip_second = all_content[-2]
data_state_2014_1 = read.csv(textConnection(skip_second), header = TRUE)

all_content = readLines("2014_state_2.csv")
skip_second = all_content[-2]
data_state_2014_2 = read.csv(textConnection(skip_second), header = TRUE)

all_content = readLines("2011_state_2.csv")
skip_second = all_content[-2]
data_state_2011_2 = read.csv(textConnection(skip_second), header = TRUE)


data_state_1 <- smartbind(data_state_2015_1,data_state_2014_1)
data_state_1 <- data_state_1[-grep("United States", data_state_1$GEO.display.label),]
data_state_1 <- data_state_1[-grep("District of Columbia", data_state_1$GEO.display.label),]
#save(data_state_1,file = "data_state_1.rds")

data_state_2 <- smartbind(data_state_2015_2,data_state_2014_2,data_state_2011_2)
data_state_2 <- data_state_2[-grep("United States", data_state_2$GEO.display.label),]
data_state_2 <- data_state_2[-grep("District of Columbia", data_state_2$GEO.display.label),]
data_state_2$ele_avg <- data_state_2$CSTELEC / data_state_2$ELECPCH
#save(data_state_2, file = "data_state_2.rds")

remove(data_state_2011_2,data_state_2014_2,data_state_2014_1,data_state_2015_1,data_state_2015_2 )



fringe_state <- aggregate(BENEFIT ~ GEO.display.label, data = data_state_1, mean, na.action = na.omit)
#fringe_state$BENEFIT <- fringe_state$BENEFIT/4


ele_state <- aggregate(ele_avg ~ GEO.display.label, data = data_state_1, mean, na.action = na.omit)
#ele_state$ele_avg <- ele_state$ele_avg/4

contract_state <- aggregate(CSTCNT ~ GEO.display.label, data = data_state_1, mean, na.action = na.omit)
#contract_state$CSTCNT <- contract_state$CSTCNT/4
 
material_state <- aggregate(CSTMTOT ~ GEO.display.label, data = data_state_1, mean, na.action = na.omit)
#material_state$CSTMTOT <- material_state$CSTMTOT/4

emp_state <- aggregate(EMP ~ GEO.display.label, data = data_state_2, mean, na.action = na.omit)


data_state <- merge(fringe_state, ele_state, by = "GEO.display.label")
data_state <- merge(data_state, contract_state, by = "GEO.display.label")
data_state <- merge(data_state, material_state, by = "GEO.display.label")
data_state <- merge(data_state, emp_state, by = "GEO.display.label")

data_state$BENEFIT <- data_state$BENEFIT / data_state$EMP
data_state$CSTCNT <- data_state$CSTCNT / data_state$EMP
data_state$CSTMTOT <- data_state$CSTMTOT / data_state$EMP

#saveRDS(data_state, file = "data_state.rds")

########  FOR SPECIFIC SECTOR ##########


sector1 = "Food"

sector = "Chocolate"

chemical <- data_us[grep(sector, data_us$NAICS.display.label),]
chemical <- chemical[,-grep("_S", colnames(chemical))]
chemical <- chemical[chemical$YEAR.id > 2011,]

fringe <- sum(chemical$BENEFIT,na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
ele <- sum(chemical$CSTELEC,na.rm = T) / sum(chemical$ELECPCH, na.rm = T)   # $ / kWh / emp
contract <- sum(chemical$CSTCNT, na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
material <- sum(chemical$CSTMTOT, na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
emp <- mean(chemical$EMP)

###### CALCULATE COSTS #########

data_state$tot_cost2 <- (data_state$BENEFIT * emp) + (data_state$ele_avg * emp * 16986848) + (data_state$CSTMTOT * emp) + (data_state$CSTCNT * emp)
tot_cost <- (fringe * emp) + (ele * emp * 16986848) + (contract * 42449) + (material * emp)


data_state$per <- 100 - ( 100 * data_state$tot_cost2 / tot_cost)
data_state <- data_state[order(-data_state$per),]

if (sum(data_state$per > 0) > 4) {
  data_state <- data_state[-grep("Montana", data_state$GEO.display.label), ]
  data_state <- data_state[-grep("Washington", data_state$GEO.display.label), ]
}

state <- data_state[1:2,]
state$per <- round(state$per,1)


library(googleVis)

Gauge <-  gvisGauge(state[1,c(1,8)], 
                    options=list(min=-10, max=50, greenFrom=15,
                                 greenTo=50, yellowFrom=0, yellowTo=15,
                                 redFrom=-10, redTo=0, width=400, height=300))
plot(Gauge)
















# all_content = readLines("2011.csv")
# skip_second = all_content[-2]
# data_2011 = read.csv(textConnection(skip_second), header = TRUE)
# data_2011 <- data_2011 %>% distinct(EMP, .keep_all = T)
# 
# all_content = readLines("2009.csv")
# skip_second = all_content[-2]
# data_2009 = read.csv(textConnection(skip_second), header = TRUE)
# data_2009 <- data_2009 %>% distinct(EMP, .keep_all = T)
# 
# all_content = readLines("2008.csv")
# skip_second = all_content[-2]
# data_2008 = read.csv(textConnection(skip_second), header = TRUE)
# data_2008 <- data_2008 %>% distinct(EMP, .keep_all = T)
# data_2007 <- data_2008[data_2008$YEAR.id == 2007,]
# 
# all_content = readLines("2006.csv")
# skip_second = all_content[-2]
# data_2006 = read.csv(textConnection(skip_second), header = TRUE)
# colnames(data_2006)[9] <- "EMP"
# colnames(data_2006)[10] <- "EMP_S"
# data_2006 <- data_2006 %>% distinct(EMP, .keep_all = T)





chemical <- data_us[grep("chemical", data_us$NAICS.display.label),]
chemical <- chemical[,-grep("_S", colnames(chemical))]
chemical <- chemical[chemical$YEAR.id > 2011,]

fringe <- sum(chemical$BENEFIT,na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
ele <- sum(chemical$CSTELEC,na.rm = T) / sum(chemical$ELECPCH, na.rm = T)   # $ / kWh / emp
contract <- sum(chemical$CSTCNT, na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
material <- sum(chemical$CSTMTOT, na.rm = T) / sum(chemical$EMP,na.rm = T)  # $1000 / emp
emp <- mean(chemical$EMP)

tot_cost <- (fringe * emp) + (fuel * emp) + (ele * emp * mean(chemical$ELECPCH, na.rm = T)) + (contract * emp) + (material * emp)




#### STATE DATA ##########


all_content = readLines("2015_state_1.csv")
skip_second = all_content[-2]
data_state_2015_1 = read.csv(textConnection(skip_second), header = TRUE)
data_state_2015_1 <- data_state_2015_1[data_state_2015_1$YEAR.id == 2015,]

all_content = readLines("2015_state_2.csv")
skip_second = all_content[-2]
data_state_2015_2 = read.csv(textConnection(skip_second), header = TRUE)
data_state_2015_2 <- data_state_2015_2[data_state_2015_2$YEAR.id == 2015,]

all_content = readLines("2014_state_1.csv")
skip_second = all_content[-2]
data_state_2014_1 = read.csv(textConnection(skip_second), header = TRUE)

all_content = readLines("2014_state_2.csv")
skip_second = all_content[-2]
data_state_2014_2 = read.csv(textConnection(skip_second), header = TRUE)

data_state_1 <- rbind(data_state_2015_1,data_state_2014_1)
data_state_1 <- data_state_1[-grep("United States", data_state_1$GEO.display.label),]
data_state_1 <- data_state_1[-grep("District of Columbia", data_state_1$GEO.display.label),]

data_state_2 <- rbind(data_state_2015_2,data_state_2014_2)
data_state_2 <- data_state_2[-grep("United States", data_state_2$GEO.display.label),]
data_state_2 <- data_state_2[-grep("District of Columbia", data_state_2$GEO.display.label),]


fringe_state <- aggregate(BENEFIT ~ GEO.display.label, data = data_state_1, mean, na.action = na.omit)
#fringe_state$BENEFIT <- fringe_state$BENEFIT/4

data_state_1$ele_avg <- data_state_1$CSTELEC / data_state_1$ELECPCH
ele_state <- aggregate(ele_avg ~ GEO.display.label, data = data_state_1, mean, na.action = na.omit)
#ele_state$ele_avg <- ele_state$ele_avg/4

contract_state <- aggregate(CSTCNT ~ GEO.display.label, data = data_state_1, mean, na.action = na.omit)
#contract_state$CSTCNT <- contract_state$CSTCNT/4

material_state <- aggregate(CSTMTOT ~ GEO.display.label, data = data_state_1, mean, na.action = na.omit)
#material_state$CSTMTOT <- material_state$CSTMTOT/4

emp_state <- aggregate(EMP ~ GEO.display.label, data = data_state_2, mean, na.action = na.omit)


data_state <- merge(fringe_state, ele_state, by = "GEO.display.label")
data_state <- merge(data_state, contract_state, by = "GEO.display.label")
data_state <- merge(data_state, material_state, by = "GEO.display.label")
data_state <- merge(data_state, emp_state, by = "GEO.display.label")

data_state$BENEFIT <- data_state$BENEFIT / data_state$EMP
data_state$CSTCNT <- data_state$CSTCNT / data_state$EMP
data_state$CSTMTOT <- data_state$CSTMTOT / data_state$EMP

data_state$tot_cost <- data_state$BENEFIT + data_state$CSTCNT + data_state$CSTMTOT

data_state <- data_state[data_state$EMP > 40000,]

data_state$tot_cost2 <- (data_state$BENEFIT * 42449) + (data_state$ele_avg * 42449 * 16986848) + (data_state$CSTMTOT * 42449) + (data_state$CSTCNT * 42449)
tot_cost <- (fringe * 42449) + (ele * 42449 * 16986848) + (contract * 42449) + (material * 42449)




ind <- c("Machinery", "Food", "Chemical", "Paper", "Electronics")
cost <- c(3992,2705,2536,1662,1079)
Oklahoma <- data.frame(ind,cost)
saveRDS(Oklahoma, "Oklahoma.rds")

p <- plot_ly(Montana, labels = ~ind, values = ~cost, type = 'pie') %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

m <- list(l = 0,r = 0,b = 0,t = 0, pad = 0, autoexpand = TRUE)

p8 <- plot_ly(Montana, labels =  ~ind, values = ~cost, type = 'pie',hole = 0.8,
              textinfo = 'value',insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',text = ~paste(ind,': $', cost),showlegend = T) %>%
  layout(margin = m,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         legend = list(x = 0.25, y = 0.5)) %>%
  config(displayModeBar = F)




























library(googleVis)

Gauge <-  gvisGauge(chemical_emp, 
                    options=list(min=0, max=800, greenFrom=500,
                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                 redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)
summary(data_us)

sector = "chemical"







######### OLD COMMANDS ###################

# 1. Fastest growing manu. field in the US

# Clean the dataset
data_2015 <- data_2015[data_2015$YEAR.id == 2015,]
#data_2015$VALADD <- data_2015$VALADD/1000000

profit_2015 <- aggregate(VALADD ~ NAICS.display.label + YEAR.id, data = data_2015, na.action = na.omit, sum)
colnames(profit_2015) <- c("Label","Year_2015", "Earnings_2015")
profit_2015$Earnings_2015 <- round(profit_2015$Earnings_2015,0)

#data_2014$VALADD <- data_2014$VALADD/1000000
profit_2014 <- aggregate(VALADD ~ NAICS.display.label + YEAR.id, data = data_2014, na.action = na.omit, sum)

profit_2013 <- profit_2014[profit_2014$YEAR.id == 2013,]
colnames(profit_2013) <- c("Label","Year_2013", "Earnings_2013")
#profit_2013$Earnings_2013 <- round(profit_2013$Earnings_2013,0)

profit_2012 <- profit_2014[profit_2014$YEAR.id == 2012,]
colnames(profit_2012) <- c("Label","Year_2012", "Earnings_2012")
#profit_2012$Earnings_2012 <- round(profit_2012$Earnings_2012,0)

profit_2014 <- profit_2014[profit_2014$YEAR.id == 2014,]
colnames(profit_2014) <- c("Label","Year_2014", "Earnings_2014")
#profit_2014$Earnings_2014 <- round(profit_2014$Earnings_2014,0)

# Merge Dataset

recent_data <- merge(profit_2015,profit_2014, by = "Label", all.x = TRUE)
recent_data <- merge(recent_data, profit_2013, by = "Label", all.x = TRUE)
recent_data <- merge(recent_data, profit_2012, by = "Label", all.x = TRUE)

recent_data <- select(recent_data, select = -c(Year_2015,Year_2014,Year_2013,Year_2012))
recent_data <- recent_data[complete.cases(recent_data),]

recent_data$earning <- (recent_data$Earnings_2012 + recent_data$Earnings_2013 +recent_data$Earnings_2014 + recent_data$Earnings_2015)/4

recent_data$grow_2012 = (recent_data$Earnings_2013 - recent_data$Earnings_2012) * 100 / recent_data$Earnings_2012
recent_data$grow_2013 = (recent_data$Earnings_2014 - recent_data$Earnings_2013) * 100 / recent_data$Earnings_2013
recent_data$grow_2014 = (recent_data$Earnings_2015 - recent_data$Earnings_2014) * 100 / recent_data$Earnings_2014

recent_data$grow_avg = (recent_data$grow_2012 + recent_data$grow_2013 + recent_data$grow_2014) / 3

max_earning <- recent_data[order(-recent_data$earning),]
max_earning <- max_earning[-1,]
max_earning <- max_earning[1:7,]
# Avg manufracturing growth ~ 1.84  in last 4 years%
test <- as.data.frame(t(max_earning))
colnames(test) <- c("Chemical", "Transportation", "Pharmaceutical", "Food", "Aerospace", "Coal", "Refineries")
test <- test[-1,]
test <- test[1:4,]
test$Year <- c(2015,2014,2013,2012)



max_growth <- recent_data[order(-recent_data$grow_avg),]
max_growth <- max_growth[-1,]
max_growth <- max_growth[1:7,]
max_growth$grow_avg <- round(max_growth$grow_avg,1)

max_growth$Label <- factor(max_growth$Label)

levels(max_growth$Label) <- c("Dairy", "Motor-Home (RV)", "Wood Preservation", "Gypsum", "Truss", "Trucks", "Wood Building")
                              

#saveRDS(test, file = "test.rds")
p1 <- plot_ly(
  x = c("2015","2014","2013","2012"),
  y = as.vector(test$Chemical),
  type = 'bar',
  name = "Chemical"
) %>%
  add_trace(y = as.vector(test$Transportation), name = "Transportation") %>%
  add_trace(y = as.vector(test$Pharmaceutical), name = "Pharmaceutical") %>%
  add_trace(y = as.vector(test$Food), name = "Food") %>%
  add_trace(y = as.vector(test$Aerospace), name = "Aerospace") %>%
  add_trace(y = as.vector(test$Coal), name = "Coal") %>%
  add_trace(y = as.vector(test$Refineries), name = "Refineries") %>%
              layout( title = "Highest Earning Manufacturing Industries",
                      font = list(size = 25),
                      margin = list(l = 100, r = 50, b = 100, t = 100, pad = 4),
                      yaxis = list(title = "Earnings (Billion US$)"), xaxis = list(title = paste("Year","",sep = "<br>")), 
                      barmode = 'group',legend = list(orientation = 'v'))


#saveRDS(max_growth, file = "max_growth.rds")
p2 <- plot_ly(
  x= max_growth$Label,
  y = max_growth$grow_avg,
  type='bar'
) %>%
  layout(title = "Fastest Growing Manufacturing Industries",font = list(size = 20),
        margin = list(l = 100, r = 50, b = 100, t = 100, pad = 4),
         yaxis = list(title = "Growth (%)"), xaxis = list(title = "Industry"), barmode = 'group',
         annotations = list(
           x = 0.48 , y = 1.0, text = "(The Average Growth of Manufacturing Industries is 1.84%)", showarrow = F, xref='paper', yref='paper'))





data_2015$data_expense <- data_2015$PCHEXSO + data_2015$PCHDAPR + data_2015$CEXMCHC

data_2015$total_expense <- data_2015$CSTMTOT + data_2015$CSTMTOT + data_2015$CSTMPRT + data_2015$CSTFU + 
                            data_2015$CSTELEC + data_2015$CSTCNT

data_2015 <- subset(data_2015, select = c(NAICS.display.label,data_expense,total_expense,EMP))
data_2015 <- data_2015[-1,]
data_2015 <- data_2015[complete.cases(data_2015),]
colnames(data_2015) <- c("Label", "Data", "Total", "Emp")

comb_table <- merge(recent_data,data_2015, by = "Label", all.x = TRUE)
comb_table$earning <- comb_table$earning/comb_table$Emp
comb_table$Data <- comb_table$Data/comb_table$Emp

comb_table <- comb_table[complete.cases(comb_table),]

#saveRDS(comb_table, file = "comb_table.rds")

p3 <- plot_ly(comb_table, x = ~Data) %>%
  add_markers(y = ~earning, color = ~grow_avg, marker = list(size = 10, colorbar = list(title = paste("Average", "Growth", sep = "<br>")
                                                                             ))) %>%
  layout(title = "Earning Vs. Spending on Data Science", margin = list(l = 100, r = 50, b = 100, t = 100, pad = 4), font = list(size = 25),
         xaxis = list(title = "Data Science Spending per Employee ($1000 US)", range = c(0,4.5)), 
         yaxis = list(title = "Earning per Employee ($1000 US)", range = c(0,1000)),
         annotations = list(
           x = 0.52 , y = 1.05, text = "(Across all Manufacturing Industries in the US)", showarrow = F, xref='paper', yref='paper'))
  
### State Data ##


all_content = readLines("state.csv")
skip_second = all_content[-2]
state = read.csv(textConnection(skip_second), header = TRUE)

state$avg_ele <- state$CSTELEC / state$ELECPCH
ele <- aggregate(avg_ele ~ GEO.display.label + YEAR.id, data = state, mean, na.action = na.omit)

ele_expensive <- ele[order(-ele$avg_ele),] # unit = US $ / kWh
ele_expensive <- ele_expensive[1:10,]

# States with most expensive electrocity = {Hawaii, Rhode Island, Alaska, California, Massachusetts, New Hampshire }

ele_lessexpensive <- ele[order(ele$avg_ele),]

# States with least expensive electrocity = {Montana, Washington, Oregon, Kentucky, West Virginia, Texas }

# Read state2

state2 <- read.csv("state2.csv")

state_total <- merge(state,state2, by.x = "GEO.display.label", by.y = "State", all.x = TRUE)

# Average material cost 

state_total$Manufacturing.Firms..2013. <- unfactor(state_total$Manufacturing.Firms..2013.)
state_total$Manufacturing.Firms..2013. <- as.numeric(gsub(",", "", state_total$Manufacturing.Firms..2013.))

state_total$avg_mat_cost <- state_total$CSTMTOT/state_total$Manufacturing.Firms..2013.

state_total <- state_total[order(state_total$avg_mat_cost),]

# States with least material cost = {Louisiana, Delaware, Kentucky, Texas, Nebraskaa, Iowa }


c=c("Texas")
map(database = "state",regions = c,col = "antiquewhite1",fill=T)