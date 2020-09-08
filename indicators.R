data = read.csv(file.choose())
head(data)

# Code for Active member as an indicator
data_exited = data[data$Exited == 1,]
head(data_exited)

print(nrow(data_exited$IsActiveMember == 1))

data_exited_active = data_exited[data_exited$IsActiveMember == 1,]

data_exited_nonActive = data_exited[data_exited$IsActiveMember == 0,]

pie_data = c(nrow(data_exited_active),nrow(data_exited_nonActive))
pie_percent = round(100*pie_data/sum(pie_data),1)

library(plotrix)


pie(pie_data,
    pie_percent,
    main = "Active or non active customer who exited",
    col = rainbow(length(pie_data))
    
    )
legend("topleft",c("Active Member","Non Active Member"),fill = rainbow(length(pie_data)),cex = 0.7)



# Code for Credit Score

customer_exited_badScore = data_exited[data$CreditScore<580,]
customer_exited_fairScore = data_exited[data$CreditScore>=580 & data$CreditScore <669,]
customer_exited_goodScore = data_exited[data$CreditScore>=670 & data$CreditScore <739,]
customer_exited_veryGoodScore = data_exited[data$CreditScore>=740 & data$CreditScore <=799,]
customer_exited_excellentScore = data_exited[data$CreditScore>=800,]

plot_data = c(nrow(customer_exited_badScore),nrow(customer_exited_fairScore),
              nrow(customer_exited_goodScore),nrow(customer_exited_veryGoodScore),
              nrow(customer_exited_excellentScore))
piePercent = round(100*plot_data/sum(plot_data),2)

pie(plot_data,
    piePercent,
    main = "Customer exited based on credit score",
    col = rainbow(length(plot_data))
    
)
segment_name = c("Bad Score","Fair Score","Good Score","Very Good Score","Excellent")
legend("topleft",segment_name,fill = rainbow(length(plot_data)),cex = 0.6)



# Code for age group
Age16To20 = data_exited[data_exited$Age>=16 & data_exited$Age<20,]
Age20To30 = data_exited[data_exited$Age>=20 & data_exited$Age<30,]
Age30To40 = data_exited[data_exited$Age>=30 & data_exited$Age<40,]
Age40To50 = data_exited[data_exited$Age>=40 & data_exited$Age<50,]
Age50To60 = data_exited[data_exited$Age>=50 & data_exited$Age<60,]
Age60To70 = data_exited[data_exited$Age>=60 & data_exited$Age<70,]
Age70To80 = data_exited[data_exited$Age>=70 & data_exited$Age<80,]
Age80To90 = data_exited[data_exited$Age>=80 & data_exited$Age<90,]


plot_Age_Data = c(nrow(Age16To20),nrow(Age20To30),nrow(Age30To40),nrow(Age40To50),
                  nrow(Age50To60),nrow(Age60To70),nrow(Age70To80),
                  nrow(Age80To90))



df <- data.frame(
    AgeGroup = c("Age 16 to 20", "Age 20 to 30", "Age 30 to 40","Age 40 to 50",
              "Age 50 to 60","Age 60 to 70","Age 70 to 80","Age 80 to 90"),
    count = c(nrow(Age16To20),nrow(Age20To30),nrow(Age30To40),nrow(Age40To50),
              nrow(Age50To60),nrow(Age60To70),nrow(Age70To80),
              nrow(Age80To90))
)
bp<- ggplot(df, aes(x="", y=count, fill=AgeGroup))+
    geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie





