getwd()

install.packages("tidyverse") #パッケージのインストール
library(tidyverse) #パッケージの読み込み
install.packages("quantreg")
install.packages("stargazer")


store_1 <- read_csv("drugstore_1.csv")
store_2 <- read_csv("drugstore_2.csv")
store_3  <- read_csv("drugstore_3.csv")
store_4  <- read_csv("drugstore_4.csv")
store <- read_csv("store.csv")

# テーブルを繋げる
store_12 <- bind_rows(store_1,store_2)
store_34 <- bind_rows(store_3,store_4)
store_1234 <- left_join(store_12,store_34,by=c("Store","Date"))
store_test <- left_join(store_1234,store,by=c("Store"))
summary(store_test)



## 1変数
　# sales_open_histogram


plot_sales<- ggplot(store_1234, aes(Sales)) + geom_histogram() + 
  labs(title = "店舗の売り上げ分布",x = "売り上げ", y = "頻度") +  
  theme(text = element_text(family = "Hiragino Sans")) 
print(plot_sales)

store_open <- filter(store_1234, Open == 1)
plot_sales_open<- ggplot(store_open, aes(Sales)) + 
  geom_histogram() +
  labs(title = "店舗の売り上げ分布（営業日のみ）",x = "売り上げ", y = "頻度") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(plot_sales_open)

　#sales_open_kernel(density)
plot_sales_open<- ggplot(store_open, aes(Sales)) + 
  geom_density(kernel = "gaussian",bw = 1000.0) +
  labs(title = "店舗の売り上げ分布（営業日のみ）",x = "売り上げ", y = "頻度") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(plot_sales_open)
　#plot_sales_open<- ggplot(store_open, aes(Sales)) + geom_density(kernel = "gaussian",adjust = 1.0)
　#print(plot_sales_open)

plot_sales_open<- ggplot(store_open, aes(Sales)) + geom_dotplot()
print(plot_sales_open)

　#記述統計テーブル
summary(store_open$Sales)

　#customers hist
library(tidyverse)
plot_customers<- ggplot(store_open, aes(Customers)) + 
  geom_histogram() +
  labs(title = "顧客数分布（営業日のみ）",x = "顧客数", y = "頻度") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(plot_customers)
plot_customers_open<- ggplot(store_open, aes(Customers)) + 
  geom_density(kernel = "gaussian",bw = 1000.0) +
  labs(title = "顧客数分布（営業日のみ）",x = "顧客数", y = "頻度") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(plot_customers_open)
　#記述
summary(store_open$Customers)

#分位回帰モデル
#library(quantreg)
#plot_sales_open_bunnichi <- rq(Sales ~ Customers,data = store_1234, tau = c(0.5))
#summary(plot_sales_open_bunnichi)

#library(stargazer)
#stargazer(plot_sales_open_bunnichi, type = "text")

##2変量
 #geom_pointで　sale and customers 回帰分析
sales_customers <- ggplot(store_open, aes( x = Customers, y = Sales)) + 
  geom_point(shape = 21) +
  geom_smooth(method = "lm", formula = y ~ x , 
                            color = "red", se = FALSE) +
  labs(title = "顧客数と売上の相関分析",x = "顧客数", y = "売上") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(sales_customers)

model_sc <- lm(Sales ~ Customers, data = store_open)
summary(model_sc)


#geom_boxplotで　sale and promo
sales_promo_box <- ggplot(store_open, aes(factor(Promo),Sales)) + 
  geom_boxplot() +
  labs(title = "売上とプロモーションの相関関係",x = "プロモーションの状態", y = "売上") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(sales_promo_box)

#geom_boxplotで　customers and holiday
holiday_customers_box <- ggplot(store_open, aes(factor(StateHoliday),Customers)) + 
  geom_boxplot() +
  labs(title = "祝日と顧客数の関係",x = "祝日", y = "顧客数") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(holiday_customers_box)

#geom_boxplotで　customers and school holiday
schoolholiday_customers_box <- ggplot(store_open, aes(factor(SchoolHoliday),Customers)) + geom_boxplot()
print(schoolholiday_customers_box)
#geom_boxplotで　store_type and sales
storetype_sales <- ggplot(store_test, aes(factor(StoreType),Sales)) + 
  geom_boxplot() +
  labs(title = "店舗の種類ごとに売上",x = "店舗種類", y = "売り上げ") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(storetype_sales)

#回帰分析
##sales = promo + state holiday, customer = state holiday, 
##sales = promo + state holiday + customer
#csh 回帰分析　残差分布図　
model_csh <- lm(Customers ~ StateHoliday, 
                data = store_open)
summary(model_csh)
resid_csh <- resid(model_csh)
csh_point_resid <- ggplot(store_open, 
                          aes(StateHoliday, resid_csh)) + 
  geom_point(shape = 21) +
  labs(title = "祝日と顧客数モデルの残差分布",x = "祝日", y = "予測した顧客数の残差") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(csh_point_resid)
#ダミー変数の場合、回帰線が書けない、箱ひげ図に変わる
  # geom_smooth(method = "lm", formula = y ~ x , 
 #                            color = "red", se = FALSE)
#print(csh_point_line)
#sps sp 回帰分析　残差分布図　
model_sp <- lm(Sales ~ Promo, 
                data = store_open)
summary(model_sp)
resid_sp <- resid(model_sp)
sp_point_resid <- ggplot(store_open, 
                          aes(Promo, resid_sp)) + 
  geom_point()
print(sp_point_resid)
#sps
model_sps <- lm(Sales ~ Promo + StateHoliday, 
               data = store_open)
summary(model_sps)
resid_sps <- resid(model_sps)
sps_point_resid <- ggplot(store_open, 
                         aes(Promo, resid_sps)) + 
  geom_point()
print(sps_point_resid)
#spsc
model_spsc <- lm(Sales ~ Promo + StateHoliday + Customers, 
                data = store_open)
#summary(model_spsc)
resid_spsc <- resid(model_spsc)
spsc_point_resid <- ggplot(store_open, 
                          aes(Promo, resid_spsc)) + 
  geom_point()
print(spsc_point_resid)

library(stargazer)
stargazer(model_sp, model_sps, model_spsc, type = "text",
                    title = "sales_promo")

#sales_cpdist
# 将年和月转换为日期
store_test$CompetitionMonYEAR <- 
  as.Date(paste(store_test$CompetitionOpenSinceYear, 
                store_test$CompetitionOpenSinceMonth, "01", sep = "-"))

# 查看结果
print(store_test$CompetitionMonYEAR)

sales_cpdist <- filter(store_test, Date >= CompetitionMonYEAR)
sales_cpdist_point <- ggplot(sales_cpdist, aes( x = CompetitionDistance, y = Sales)) + 
  geom_point(shape = 21) +
  geom_smooth(method = "lm", formula = y ~ x , 
              color = "red", se = FALSE) +
  labs(title = "競合店との距離と売上の相関分析",x = "競合店との距離", y = "売上") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(sales_cpdist_point)

model_sales_cpdist <- lm(Sales ~ CompetitionDistance, 
                         data = sales_cpdist)
summary(model_sales_cpdist)


#時系列の図 date によって　毎日saleの平均
mean_store <- store_open %>%
  group_by(Date) %>%
  summarise(Average_sell = mean(Sales))
timeseriese_daymean<-ggplot(mean_store, aes(x=Date,y=Average_sell))+
  geom_line() +
  labs(title = "日平均の売り上げの時系列",x = "時間", y = "日ごとの平均売上") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(timeseriese_daymean)

#時系列の図 store1の変化
store1_sales <- filter(store_open,Store == 1)
library(ggplot2)
timeseriese<-ggplot(store1_sales, aes(x=Date,y=Sales))+
  geom_line()+
  facet_wrap(~ Store, scales="free_y") +
  labs(title = "個別売り上げの時系列",x = "時間", y = "store1 日ごとの売上") +  
  theme(text = element_text(family = "Hiragino Sans")) 

print(timeseriese)





