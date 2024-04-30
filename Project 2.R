library(readxl)
dataIncomes = read_xlsx("/Users/loannguyen/Documents/R files/Profitability of Fiber-Optic Internet Expansion Analysis/Incomes per State.xlsx")
dataSubscriptions = read_xlsx("/Users/loannguyen/Documents/R files/Profitability of Fiber-Optic Internet Expansion Analysis/Internet Subscriptions per State.xlsx")
dataComputer = read_xlsx("/Users/loannguyen/Documents/R files/Profitability of Fiber-Optic Internet Expansion Analysis/Computer Types by State.xlsx")

dataSubscriptions$`With an Internet subscription:` = as.numeric(gsub(",", "", dataSubscriptions$`With an Internet subscription:`))
dataSubscriptions$`Dial-up with no other type of Internet subscription` = as.numeric(gsub("," ,"", dataSubscriptions$`Dial-up with no other type of Internet subscription`))
dataSubscriptions$`Broadband of any type` = as.numeric(gsub(",", "", dataSubscriptions$`Broadband of any type`))
dataSubscriptions$`Cellular data plan` = as.numeric(gsub(",", "", dataSubscriptions$`Cellular data plan`))
dataSubscriptions$`Broadband such as cable, fiber optic or DSL` = as.numeric(gsub(",", "", dataSubscriptions$`Broadband such as cable, fiber optic or DSL`))
dataSubscriptions$`Satellite Internet service` = as.numeric(gsub(",", "", dataSubscriptions$`Satellite Internet service`))
dataSubscriptions$`Without an Internet subscription` = as.numeric(gsub(",", "", dataSubscriptions$`Without an Internet subscription`))

dataComputer$`Has one or more types of computing devices:` = as.numeric(gsub(",", "", dataComputer$`Has one or more types of computing devices:`))
dataComputer$`Desktop or laptop` = as.numeric(gsub(",","",dataComputer$`Desktop or laptop`))
dataComputer$Smartphone = as.numeric(gsub(",", "", dataComputer$Smartphone))
dataComputer$`Tablet or other portable wireless computer` = as.numeric(gsub(",", "", dataComputer$`Tablet or other portable wireless computer`))
dataComputer$`Other computer` = as.numeric(gsub(",", "", dataComputer$`Other computer`))
dataComputer$`No computer` = as.numeric(gsub(",", "", dataComputer$`No computer`))

dataIncomes = dataIncomes[-51,]

data = data.frame(one_or_more_computers = dataComputer$`Has one or more types of computing devices:`,
                  no_computer = dataComputer$`No computer`,
                  subscriptions = dataSubscriptions$`With an Internet subscription:`,
                  no_subscriptions = dataSubscriptions$`Without an Internet subscription`,
                  low_income = dataIncomes$`Less than $20,000:`,
                  middle_income = dataIncomes$`$20,000 to $74,999:`,
                  high_income = dataIncomes$`$75,000 or more:`)
head(data)
data$total_computers = data$one_or_more_computers + data$no_computer
data$computer_rate = data$one_or_more_computers/data$total_computers
data$subs_rate = data$subscriptions / data$total_computers
data$high_inc_rate = data$high_income / data$total_computers
head(data, 3)
data$state = dataComputer$State

library(ggplot2)
ggplot(data, aes(x = computer_rate, y = subs_rate, color = high_inc_rate)) + geom_point() +
  labs(title = "Correlation Between Computer Types, Internet Subscriptions, and High Income Rates",
       x = "Computer Rate", y = "Internet Subscription Rate") + theme(plot.title = element_text(hjust = .75, size = 10)) +
  guides(color = guide_legend(title = "High Income Rate"))
library(ggrepel)
ggplot(data, aes(x = computer_rate, y = subs_rate, color = high_inc_rate)) + geom_point() +
  geom_text_repel(aes(label = state), size = 3, show.legend = F) +
  labs(title = "Computer Types, Internet Subscriptions, and High Income Rates Across States", x = "Computer Rate",
       y = "Internet Subscription Rate") + theme(plot.title = element_text(hjust = .71, size = 10)) +
  guides(color = guide_legend(title = "High Inc Rate"))

dataNarrow = subset(data, round(high_inc_rate, digits = 2) > .50)
rownames(dataNarrow) = NULL
dataNarrow
dataNarrow$state = ""
num_label = c(5, 8, 4)
dataNarrow$state[num_label] = c("Maryland", "New Jersey", "Hawaii")
ggplot(dataNarrow, aes(x = computer_rate, y = subs_rate)) + geom_point() +
  geom_text_repel(aes(label = state), size = 3) +
  geom_point(color = ifelse(dataNarrow$state == "", "grey", "skyblue1")) +
  labs(title = "Three States with Potential Subscriptions, Incomes, and Computer Ownership",
       x = "Computer Rate", y = "Internet Subscription Rate") +
  theme(plot.title = element_text(hjust = .4))

dataNarrow2 = subset(data, round(high_inc_rate, digits = 2) > .45 & round(high_inc_rate, digits = 2) < .50)
rownames(dataNarrow2) = NULL
dataNarrow2
dataNarrow2$state = ""
dataNarrow2$state[c(6, 3, 4)] = c("Utah", "Minnesota", "New York")
ggplot(dataNarrow2, aes(x = computer_rate, y = subs_rate)) + geom_point() + geom_text_repel(aes(label = state),
                                                                                            size = 3) +
  geom_point(color = ifelse(dataNarrow2$state == "", "grey", "steelblue4")) +
  labs(title = "Internet Subscriptions and Computer Ownership in States",
       x = "Computer Rate", y = "Internet Subscription Rate") +
  theme(plot.title = element_text(hjust = .47))


