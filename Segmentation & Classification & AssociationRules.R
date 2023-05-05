Sys.setenv(JAVA_HOME="")
install.packages("RWeka")



################################### 3-  Segmentation #############################################
## بخش بندی بازار --------------------------------------------------------------------------------


##Set work directory-----------------------------
setwd("C:/Users/saman/Desktop")
##Required libraries-----------------------------
library("cluster")                      #clustering
library("e1071")                        #classification
library("ggplot2")                      #visualization
library("factoextra")                   #clustering visualization

##Read data from file----------------------------
# Data Frame: segmentation

data <- read.csv(file.choose(), header = TRUE)
names(data)
dim(data)
head(data)
tail(data)
### Clustering Analysis--------------------------------------------------------
## نکته : داده ها دارای برچسب هستند و فقط در الگوریتم دسته بندی استفاده میشود

# Unservise Learning
df1 <- data[,-c(1,8)]       # حذف برچسب و ستون اول
head(df1)

##Data Inspection--------------------------------
## بررسی داده ها
summary(df1)
str(df1)
#Continuous variables                       نمایش متغیرهای پیوسته در یک نمودار
par(mfrow = c(1,3))  # 1 rows and 3 column


# C(1,3,4)= نمایش متغیرهای پیوسته یک و سه و چهار
for (i in c(1,3,4)) { 
  hist(df1[,i], xlab = "", main = paste("Histogram of", names(df1)[i]))
}


# نمایش حالت عادی
par(mfrow = c(1,1))

#correlation table
cor_table <- round(cor(df1[,c(1,3,4)]),2) 
cor_table

#Categorical variable       متغیرهای قیاسی


table(df1$Home_Owner)
table(df1$CRM)



### خوشه بندی با استفاده از الگوریتم های سلسله مراتبی : -------------------------
## Model 1: hierarchical clustering ---------------------------------------------




## محاسبه فاصله اقليدسي : 
##Euclidean way works with numeric data : فاصله اقلدسی داده های عددی

# dist = تابع محاسبه فاصله بین مشاهدات عددی


# محاسبه فاصله بین مشاهدات متغیرهای عددی
d <- dist(df1[,c("age", "Salary", "No_childs")])
d
as.matrix(d)[1:6, 1:6]  # در این مثال شش مشاهده اول را در یک ماتریس نمایش میدهیم

str(df1)


# نکته : در اینجا متغیرها عددی، باینری و قیاسی هستند
# پس از روش گوئر جهت  محاسبه مسافت متغیرهای غیر همجنس استفاده میکنیم
# در اینجا بجای فاصله اقلیدسی از روش گوئر استفاده میکنیم

## روش Gower : Gower way  works with "mixed data"types
# فاصله هر کدام از متغیرهای غیر هم جنس را جداگانه حساب کرده
# سپس همگی را هم مقیاس میکند

# daisy : تابع محاسبه مسافت

d<-daisy(df1[c(1,3,4)],metric = "gower")
as.matrix(d)[1:6, 1:6]




## apply hclust() on data :   محاسبه الگوریتم سلسله مراتبی -------------

# hclust()= تابع الگوریتم سلسله مراتبی
# method = "complete" : خوشه های مشابه را پیدا می کند
# d = ماتریس فاصله



seg_hc <- hclust(d, method = "complete") 


## Results : بررسی نتایج : -------------------
# ابتدا اجرای الگوریتم را بر روی نمودار دندوگرام نمایش نیدهیم

plot(seg_hc)


## فرض کنید میخواهیم خوشه های کمتر از پنج دهم را نمایش دهیم : ----------
##h <= 0.5: 5 clusters : در این صورت پنج خوشه خواهیم داشت

## با استفاده از دستور : Cut
plot(cut(as.dendrogram(seg_hc), h = 0.5)$lower[[1]])


## مقایسه اطلاعات مشاهدات در دیتا فریم با نمودار شماره یک
df1[c(128, 137),] # مشاهدات مشابه هم : مشتریان مشابه هم
df1[c(101, 102, 107),] # مشاهدات مشابه هم : مشتریان مشابه هم
df1[c(171, 141),]# مشتریانی که کمترین مشابهت را دارند




## Goodness-of-fit سنجس فیت شدن خوشه -------------
# بررسی میشود که چه مقدار نمودار درختی دندوگرام بر روی مدل فیت شده است
##cophenetic correlation coefficient (CPCC) : ضریب همبستگی کوفنتیک
# کوفنتیک بر اساس درخت هائی که ایجاد شده فاصله ها را محاسبه می کند
# ماتریس مشاهدات را بر اساس اینکه داده ها در چه سطحی با هم ادغام میشوند نمایش میدهد

cor(cophenetic(seg_hc), d) 
# d= ماتریس فاصله
#CPCC > 0.7 indicates a relatively strong fit هر چقدر همبستگی بین فاصله ها بلا باشد یعنی مدل خوب است


#### How many clusters? چه تعداد خوشه داشته باشیم ؟ --------------
plot(seg_hc)

# مثال : 
#h = 0.7    K = 2 groups  اگر سطح هفت دهم بگذاریم دو خوشه
#h = 0.4    K = 7 groups  اگر سطح چهار دهم بگذاریم هفت خوشه

## بررسی مثال چهار خوشه : -----------------------
##K = 4 groups     میخواهیم با فرض چهر خوشه مدل را بررسی کنیم
## rect.hclust : رسم خط متمایز کننده خوشه ها 

plot(seg_hc)

rect.hclust(seg_hc, k = 4, border="red")  # نمایش بصوری خوشه ها

# or :
rect.hclust(seg_hc, k = 4, border=2:6)

#------------------------------------------------
# dendrogram color management

install.packages('dendextend', dependencies = TRUE)
library(dendextend)

# function : color_branches

dend_seg<- as.dendrogram(seg_hc)
col_dend <- color_branches(dend_seg, k = 4)
plot(col_dend)
#-----------------------------------------------------


##Segments : بخش بندی ----------------------------
## با توجه به خوشه بندی می توانی به مشاهدات برچسب بزنیم

# cutree : هر خوشه را به داخل یک گروه برش می زند
seg_hc_segment <- cutree(seg_hc, k = 4) 
seg_hc_segment
table(seg_hc_segment)     # مشاهده فراوانی  برچسب مشاهدات خوشه ها


df1$segment <- seg_hc_segment # مقادیر برچسب را در ستونی بنام سگمنت قرار بده
head(df1)
tail(df1)
## Segment summary بررسی خوشه بندی ------------------------------

table(seg_hc_segment)

# بر  اساس سگمنت های مختلف بر روی سن میانگین میگیریم
# tapply : تابع را بر روی هر جزء از سلول های یک آرایه پیاده سازی می کند

tapply(df1$age, df1$segment, mean) # age میانگین سنی  هر سگمنت را نمایش میده

table(df1$gender, df1$segment)          # gender مقایسه جنسیت و سگمنت
tapply(df1$Salary, df1$segment, mean)   # Salary مقایسه درآمد در سگمنت ها
tapply(df1$No_child, df1$segment, mean) # No_child مقایسه تعداد فرزندان سگمنت ها
table(df1$Home_Owner, df1$segment)      # Home_Owner مقایسه مالکین خانه در سگمنت ها
table(df1$CRM, df1$segment)             # CRM مقایسه ثبت در مدیریت ارتباط با مشتریان


##Visualization ----------------------------
# CRM , gende

plot(jitter(as.numeric(df1$gender)),
     jitter(as.numeric(df1$CRM)),
     col = df1$segment, yaxt = "n", xaxt = "n", ylab = "", xlab = "")

axis(1, at = c(1, 2), labels = c("CRM: No", "CRM: Yes"))
axis(2, at = c(1, 2), labels = levels(df1$gender))


# xaxt & yaxt=n : مقادیر محور افقی و عمودی را نمایش نمیدهد

## می توانیم موارد فوق را به ازای مقادیر مختلف خوشه ها بررسی نمائیم ##




##########   خوشه بندی با استفاده از الگوریتم : k-means  ## ----------------------------------
## Model 2: kmeans clustering --------------------------------------------------------



##Required libraries-----------------------------
library("cluster")                      #clustering
library("e1071")                        #classification
library("ggplot2")                      #visualization
library("factoextra")                   #clustering visualization
install.packages("factoextra")
##Read data from file----------------------------
setwd("C:/Users/saman/Desktop")
data <- read.csv(file.choose(), header = TRUE)
names(data)
dim(data)
head(data)
tail(data)


df2 <- data[,- c(1,8)]       
str(df2)
# محاسبه فاصله بین مشاهدات 
# ابتدا داده های قیاسی رابصورت صفر و یک ذخیره می کنیم:

df2$gender    <- ifelse(df2$gender == "M", 0, 1)
df2$Home_Owner   <- ifelse(df2$Home_Owner == "NO", 0, 1)
df2$CRM <- ifelse(df2$CRM == "No", 0, 1)
head(df2)
tail(df2)

## apply kmeans on data اجرای الگوریتم  :--------
set.seed(234)
seg_km <- kmeans(df2, centers = 4)    # centers= تعداد خوشه ها


## اختصاص سگمنت به هر خوشه  : --------------
seg_km$cluster              
table(seg_km$cluster)         # نمایش فراوانی



#####   Elbow method   --------------------------------
#### روش آماری البو -----------------
# بدست آوردن ترکیب شاخصه بهینه خوشه ها


## within-cluster sum of square : مجموع مربعات فواصل درون خوشه ای-----------------------------
# تابعی که نشان می دهد داده های داخل خوشه فاصله شان از مرکز خوشه چقدر است
# هر چه تعداد خوشه ها بیشتر شود این شاخص کم می شود

## تابع محاسبه تجمیع مجموع مربعات درون خوشه ای
# Function to compute total within-cluster sum of square ):مجموع مربعات فواصل درون خوشه ای

# در اینجا تابع ابتدا تعداد خوشه را میگیرد و سپس مقدار تجمیع شده مجموع مربعات درون خوشه ای را میدهد

set.seed(234)

wss <- function(k) {
  kmeans(df2,k,nstart=10)$tot.withinss 
  
} 
# k= تعداد خوشه ها
# nstar = تعداد نمونه تصافی خوشه ها
# withinss = is the within cluster sum of squares مجموع مربعات فواصل درون خوشه ای
# $tot.withinss = sum ( $withinss )

#k values : مقدار k
k_values <- 1:10  # از یک تا ده خوشه را انتخاب میکند

## نتایج را در یک دیتا فریم ذخیره میکنیم --------------
#Extract wss for 1-10 clusters
wss_values <- data.frame(k = k_values) # دیتا فریم دارای یک ستون و مقادیر خوشه ها

for (i in k_values) {
  wss_values$wss[i] <- wss(wss_values$k[i]) 
}
# مقادیر متناظر با مجموع مربعات درون خوشه ای و را متناظر با خوشه ها قرار می دهید
wss_values   # متناظر به هر مقدار را با هر خوشه حساب می کند


## حال دیتا فریم را در یک نمودار نمایش میدهیم : ----------
##plot
plot(wss_values$k, wss_values$wss,
     type = "b", pch = 20, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")



# حال نتایج خوشه ها را به دیتا فریم اصلی اضافه می کنیم :
#Segments
df2$segment <- seg_km$cluster   # متناظر با هر مشاهده سگمنت آن مشخص می شود
head(df2)

# بررسی : 
tapply(df2$age, df2$segment, mean) # توزیع سگمنتها با سن
table(df2$gender, df2$segment) # توزیع سگمنتها با جنسیت
tapply(df2$Salary, df2$segment, mean) # توزیع سگمنتها با درآمد
tapply(df2$No_child, df2$segment, mean) # توزیع سگمنت ها با تعداد فرزندان
table(df2$Home_Owner, df2$segment) # توزیع سگمنت ها با وضعیت مالکیت خانه
table(df2$CRM, df2$segment) #CRM # توزیع سگمنت ها با ثبت در سامانه مدیریت ارتباط با مشتریان



## Visulization : رسم نمودار : -----------

## 1 : Box plot ;

boxplot(df2$Salary ~ df2$segment, ylab = "Salary", xlab = "Cluster")
# میبینیم که تفاوتهای معنی داری در توزیع  تفکیک خوشه ها وجود دارد


## 2 : fviz_cluster "PCA"; ------------------
# fviz_cluster = visual clustering result تابع  نمایش نتایج خوشه بندی
# library("factoextra") : کتابخانه مورد نیاز

library("factoextra")

fviz_cluster(seg_km, geom = "point", data = df2) + 
  ggtitle("Number of Clusters K = 4")

# خوشه های یک و دو همپوشانی دارند
# خوشه های سه و چهار همپوشانی دارند
# خوشه سه با خوشه های دو و یک تفکیک شده اند
# خوشه چهار با خوشه های یک و دو تفکیک شده اند


## 3 ) نمایش نمودار متغیرها بصورت مقایسه ای دو به دو --------------
# Salary vs. age : مقایسه سن و درآمد

ggplot(data = df2, aes(Salary, age, color = factor(segment))) +
  geom_point()
# در اينجا رنگ بر اساس سگمنت انتخاب شده است


# --------------------------------------------------------------------------------------------------------

######## k-means راه دوم ---------------------------------------------------------------------------------

# Rweka Packege
# To install the packege Refer to Slide session 01 : 
library(RWeka)

setwd("C:/Users/saman/Desktop") 
rm(data1)
data1<-read.csv(file.choose())

data1 <- na.omit(data1)
data1<-Normalize(data1)

data1<-data.frame(data1)


#data1 <- data1[,c(4,5)]


km1 <- kmeans( data1, centers = 3)  # اجرای الگوریتم

km1$cluster # تابع خوشه بندی

km1$centers # console نمایش مراکز هر خوشه 

km1$betweenss # console فاصله برون خوشه ای

# نمایش میانگین  فاصله های سه خوشه

km1$withinss  # console فاصله درون خوشه ای
# جهت تعیین مقدار بهینه میتوان تعداد مراکز مختلف را محاسبه و عدد کمتر را در نظر گرفت

# تعداد نمونه در هر خوشه
km1$size

plot(data1$cholestoral , data1$blood.pressure, col= km1$cluster)



# ارزیابی نتایج خوشه بندی : -----------------------

# Hyper K : 
install.packages("clusterCrit")
library(clusterCrit)
help(clusterCrit)

# 1- شرط اجرای این کتابخانه : ابتدا داده ها نومریک شوند
for(i in 1:ncol(data1)){
  data1[,i] <- as.numeric(data1[,i])
}

str(data1)

# 2- داده ها تبدیل به ماتریس شوند
data1 <- as.matrix(data1)


## استفاده از شاخص silhouette : 
# هر چه مقدار این شاخص بیشتر باشد کیفیت خوشه بندی بهتر است :
sil1 <- intCriteria(traj = data1, km1$cluster , "Silhouette")
#internal Criteria : تابع شاخص ارزیابی
#
# "Silhouette" : شاخص

sil1 # show console

# برای بدست آورد مقدار بهینه این شاخص مقادیر مختلف تعدا خوشه را بدست می آوریم و بهترین را انتخاب می کنیم
# best with other K values



list1 <- list()

for(i in 2:15){
  set.seed(1234)
  km1 <- kmeans(data1 , centers = i)
  list1[i] <- intCriteria(traj = data1 , km1$cluster, "Silhouette")
}

# رسم منحنی
plot(seq(2,15), unlist(list1),type = "l")
# l=line

km1 <- kmeans(data1, centers = 4)  # اجرای الگوریتم

# نمایش مقادیر خوشخه های
km1$cluster

# رسم منحنی خوشه ها
plot(data1[,1],data1[,2], col= km1$cluster)
head(data1)


####### Model 3: " Classification " اجرای الگوریتم طبقه بندی #########-------------------------------------
# الگوریتم دسته بندی یادگیری با ناظر می باشد و نیاز به برچسب یا همان تابع هدف است



setwd("C:/Users/saman/Desktop")
data <- read.csv(file.choose(), header = TRUE)
head(data)
tail(data)

df3 <- data[,-1]     # فقط ستون اول را حذف می کنیم

table(df3$segment)        # segment = بصورت پیش فرض و دانش قبلی به چهار کلاس تقسیم کردم


## داده ها را به آموزش و تست تقسیم می کنیم:-------------

set.seed(12345)
train_cases <- sample(1:nrow(df3), nrow(df3) * 0.7)  # هفتاد درصد از دویست و نود و هشت تا
train_cases


train <- df3[train_cases, ]        # داده های آموزش
test  <- df3[- train_cases, ]      # داده های تست

# مشاهده فراوانی داده های آموزش و تست
dim(train)   # 208
dim(test)    # 90
# مشاهده وضعیت توزیع
table(train$segment)
table(df3$segment)

######## Naive Bayse اجرای الگوریتم نایو بیز #########---------------------------------
library("e1071")    # کتابخانه مورد نیاز

# naiveBayes : تابع الگوریتم نایو بیز
seg_nb <- naiveBayes(segment ~ ., data = train)

## نتایج مشاهدات :
#Results
seg_nb
# احتمال وقوع هر سگمنت بدون داشتن اطلاعات اولیه
table(train$segment)/nrow(train) # احتمال وقوع : تعداد سگمنت در هر نمونه / تعداد  کل 
# بعنوان مثال اگر یک مشتری بصورت تصادفی انتخا ب بشود و بگوئیم
# با چه احتمالی در یک سگمنت خاص می باشد

#Moving up Suburb mix  Travelers  Urban hip 
#0.2451923  0.3317308  0.2500000  0.1730769 



# تشریح خروجی  ستون سن : ----------
#Conditional probabilities:
#  age
#            انحراف معیار  میانگین سن 
#              [,1]    [,2]
#Moving up  35.88235 3.403804
#Suburb mix 40.23188 5.369152
#Travelers  58.65385 8.667914
#Urban hip  24.02778 2.063092

# gender ------------------
#             درصد زن    درصد زن
#Y                    F         M
#Moving up  0.6470588 0.3529412
#Suburb mix 0.4637681 0.5362319
#Travelers  0.4615385 0.5384615
#Urban hip  0.4444444 0.5555556


## از این اطلاعات میتوانم برای به روز کردن احتمالها و تعیین کلاس های هر مشاهده استفاده می کنیم



## Prediction : پیش بینی ------------------------------------------
# بر روی دیتا بیس تست یک ستون جدید ایجاد می کنیم
# پیش بینی را با استفاده از نتایج الگوریم نایو بیز بر روی دیتا ست تست
# pred_class : نام متغیر
test$pred_class <- predict(seg_nb, test)
head(test)
tail(test)

# از نتایج تست برای مقایسه پیش بینی با حالت اولی استفاده می کنیم

## Model evaluation ارزیابی مدل -------------------------------
## ارزیابی مدل را بر روی تست انجام میدهیم : 

table(test$segment)      # مدل واقعی
table(test$pred_class)   # مدل پیش بینی شده

# محاسبه میکنیم چند درصد از برچسب های پیش بینی با برچسب های واقعی مطابقت دارد 
# با استفاده از دستور زیر :
mean(test$segment == test$pred_class)


## Confusion matrix ماتریس آشفتگی -------------------------------

table(test$segment, test$pred_class)   # جدول مقایسه پیش بینی با واقعیت 

# با جداول پیش بینی مقایسه می کنیم
table(test$segment)      # مدل واقعی
table(test$pred_class)   # پیش بینی

# واقعیت        Moving up Suburb mix Travelers Urban hip پیش بینی
#Moving up          8          6         4         1
#Suburb mix         8         20         3         0
#Travelers          0          0        26         0
#Urban hip          0          0         0        14
#-----------------------------------------------------------------

## محاسبه حالتهای چهارگانه : --------------------------------------
## ماتریس آشفتگی

#Suburb mix : مثال برای یکی از سگمنت ها 
## بقیه متغیرها به همین تزتیب می بایست نحاسبه شوند : 
#------------------------------------------------------
# 1 ) Accuracy = TP + TN / Total              # شاخص صحت
# TP = چند تا را بدرستی درست تشخیص دادیم
# TN = آنهایی که نیستند را بدرستی تشخیص دادیم
# TP = 20, TN = 14 + 26 + 19 - 6 = 53
(20+ 53) / 90
#----------------------------------------------------
# 2)  Precision = TP / TP + FP                # شاخص دقت
# چند درصد پیش بینی به درستی انجام شده
# FP = آنهایی که نیستند ولی به اشتباه تشخیص داده هستند
# TP = 20, FP = 6
20/ (20 + 6)
# -------------------------------------------------------------------
# 3) Sensitivity = TP / TP + FN             # شاخص حساسیت
# FN = آنهائی که تشخیص دادیم نیستند ولی در واقع هستند
# TP = 20, FN = 8 + 3
20/(20 + 11)
#--------------------------------------------------------------

# 4) Specificity= TN / TN + FP                # شاخص خاصیت 
# FP = آنهایی که نیستند ولی به اشتباه تشخیص داده هستند
# TN = 53, FP = 6
53 / (53 + 6)

## تمرین : این شاخص ها را برای بقیه سگمنت ها بررسی نمائید ###


## چک می کنیم که مدل چقدر خوب عمل کرده است :--------------------
## مقایسه مقادیر واقعی با مدل پیش بینی برای یک سگمنت
## suburb mix : مقادیر این سگمنت را برای نمونه بررسی می کنیم
# c(1,3,4)= (age , salary, No_child)

apply(test[test$segment    == "Suburb mix", c(1,3,4)], 2, mean)  # مقدار واقعی
apply(test[test$pred_class == "Suburb mix", c(1,3,4)], 2, mean) # مقدار پیش بینی شده



###################################################################################### 

############### Classification - Decision Tree --------------------------------------------
## الگوریتم درخت تصمیم

## Section (I) : ---------------
data1<-read.csv(file.choose())
names(data1)[1]<-"age"


library(rpart)


# الگوریتم درخت تصمیم :
fit1 <- rpart( c ~ . , method = "class" , data = data1) # الگوریتم بر روی داده ها فیت میشود


# decision Tree Plot
# Package rpart.plot

# To Make Decision Plot: 
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(fit1 , type = 2 , extra =2 , cex = 0.8)



# type: مدل رسم درخت را تعیین می کند
# extra: sample on each node!
# cex: سایز درخت

# تاثیر سه متغیر بر روی تارگت
fit1 <- rpart(c~ age+depression+chest.pain , method = "class", 
              data = data1)

# rpart plot
rpart.plot(fit1 , type= 2 , extra = 2 , cex = 0.7 )


# stop rules
fit1 <- rpart(c~. , method = "class", data= data1, 
              control = rpart.control(minsplit = 7 , maxdepth = 3))


rpart.plot(fit1 , type= 4 , extra = 2 , cex = 0.7 )


# minsplit : حداقل مشاهده تا گره انشعاب بزند
# maxdepth : ماکزیمم عمق درخت




### Section (II) : پیش بینی با الگوریتم درخت تصمیم -------------------
# Heart Data fata frame :
data1<-read.csv(file.choose())
library(rpart)

set.seed(1234)
sm1 <- sample(1:nrow(data1),nrow(data1)/3, replace = FALSE)
test1 <- data1[sm1,]
train1 <- data1[-sm1,]

fit1 <- rpart(c~. , method = "class", data = train1)

rpart.plot(fit1, type= 4 , extra = 2 , cex = 0.8)

pred1 <- predict(fit1 , test1, type="class")
pred1

table(pred1,test1$c)
mean(pred1==test1$c)  #accuracy

############# KNN : اجرای الگوریتم نزدیکترین همسایگی در مساله طبقه بندی ----------------------------------- 

# Heart Data Data Frame
data1 <- read.csv(file.choose())
names(data1)[1] <- "age"


# نصب پکیج ----------------
install.packages("caret")
library(caret)

# point
install.packages("caret", dependencies = TRUE) 

# متغیرهای باینری را به فاکتور تبدیل می کنیم

summary(data1)
data1$c <- as.factor(data1$c)
data1$chest.pain <- as.factor(data1$chest.pain)
data1$sex <- as.factor(data1$sex)

data1 <- na.omit(data1) 

set.seed(1234)
sm1 <- sample(1:nrow(data1),nrow(data1)/3,replace= FALSE)
test1 <- data1[sm1,]
train1 <- data1[-sm1,]

library("e1071")


# اجرای الگوریتم
knnmd <- knn3( c~. , data=train1 , k=15) # اجرای تابع نزدیک ترین همسایگ
# K = تعداد همسایه ها
pred1 <- predict(knnmd, test1 , type="class")

table(pred1 , test1$c)
mean(pred1 == test1$c)

######## تعیین مقدار بهینه K  #####


## روش اول : ----------------------------------------------
#choose the best K ( "Tuning" Hyper Parameters) -----------

list1 <- list() # ایجاد یک لیست خالی

ind <- 1   # مقدار اندیس

for(i in seq(1,100,5)){
  
  knnmd <- knn3(c~. , train1 , k=i)
  pred1 <- predict(knnmd, test1, type="class")
  
  list1[ind] <- mean(pred1 ==  test1$c)
  ind <- ind+1
}



# we have 20 models !
# i: step of 5
# ind: index

# you cant plot list
#if you plot list, you must unlist it
plot(seq(1,100,5),unlist(list1),type = "l")

# l= Line

knnmd <- knn3(c~. , train1 , k = 40) # k =65 با توجه به اطلاعات منحنی
pred1 <- predict(knnmd, test1, type="class")

#best model for KNN
mean(pred1 ==  test1$c)

### روش دوم : ----------------------------
# "Tune.knn" Function :

library(e1071)

x <- train1[,-14]
y <- train1[,14]

obj1 <- tune.knn(x,y,k=seq(40,60,2)) 

## tune.knn= تابع تیون هایپر پارامتر
# 40,60= بازه ای که بصورت تجربی انتخاب می شود

summary(obj1)  # 
plot(obj1)



###End of Code###--------------------------------


############################ قواعد انجمنی و تحلیل سبد مشتری #################################

### Association Rules
### Market Basket Analysis ### ----------------------------------------------------


##Set work directory-----------------------------
setwd("C:/Users/gsstech/Desktop")
##Required libraries-----------------------------
#install.packages("arules")
#association rules
# داده های تراکنش مربوط به یک خرده فروشی زنجیره ای : ------------
install.packages("arules",dependencies = TRUE)
library("arules")


##Load data from package-------------------------

data("Groceries")

inspect(head(Groceries)) 
summary(Groceries)

# 9835 rows  تراکنش 
# 169 columns  تعداد آیتم ها


## اجرای اگوریتم بر روی داده های تراکنشی Apriori ------------------------------------
## apriori : تابع

arules_model <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.3))
# supp= support , conf=confidence : فرض
arules_model
# set of 125 rules 

inspect(arules_model[1:20])  # بیست ردیف اول را فراخانی میکنم
# تحلیل : 
# support : نرخ خرید از کل تراکنش ها
# confidence : در هر تراکنش خرید یک آیتم خاص  

## سایر تحلیل ها 

## رسم نمودار :
#ItemFrequencyPlot : رسم نمودار تکرار آیتمها
# topN = پانزده تراکش اول را نمایش بده

itemFrequencyPlot(Groceries, topN = 15,
                  main = "Items Distribution", 
                  type = "absolute", ylab = "Frequency")



# relative : density

## بررسی چند نمونه با برش جهت تعیین سبد مشتری :  ------------------------
# بررسی چند قواعد دیگر

#Lift > 3
inspect(subset(arules_model, lift > 3))
# coverage= support / confidence

#Support > 0.05   سطح ساپورت بالاتر
inspect(subset(arules_model, support > 0.05))

#lhs includes whole milk :
# lhs: left hand side تراکنشهایی که سمت چپ آن شامل شیر است را برش بزن
# subset = برش
inspect(subset(arules_model, subset = lhs %in% "whole milk"))


#lhs include both whole milk and butter
# تراکنش هایی که سمت چپ آن هم شامل شیر و هم کره باشد را برش یزن
inspect(subset(arules_model, subset = lhs %ain% c("whole milk","butter")))


#rhs includes root vegetables and lift > 3
# چه چیزهایی را خریده اند که منجر شده سیب زمینی و پیاز هم بخرند
# lift>3
# rhs = right hand side
inspect(subset(arules_model, subset = rhs %in% "root vegetables", lift > 3))

# ---------------------------------------------------------------------------------------------------


####  Mor extracting with measure & advance Visualizing


# Loading the arules package ------------------
library(arules)

# Loading the Groceries dataset
data("Groceries")
summary(Groceries)
inspect(head(Groceries))

# Density of Groceries ------------------------
# Plotting a sample of 200 transactions
image(sample(Groceries, 200))


## Most and least popular items ----------------

# Most popular items
itemFrequencyPlot(Groceries,type="relative",
                  topN=10,horiz=TRUE,col='steelblue3')


# Least popular items
par(mar=c(2,10,2,2) mfrow=c(1,1))

barplot(sort(table(unlist(LIST(Groceries))))[1:10],
        horiz = TRUE,las = 1,col='orange')


### Cross tables by index -------------------------

# Contingency table
tbl <-crossTable(Groceries)
tbl[1:4,1:4]

# Sorted  table
tbl = crossTable(Groceries, sort = TRUE)
tbl[1:4,1:4]

### Cross tables by item names -------------------------

## Contingency tables

# Counts
tbl['whole milk','flour']

# Chi-square test
crossTable(Groceries, measure='chi')['whole milk', 'flour']

## Contingency tables with other metrics

crossTable(Groceries, measure='lift',sort=T)[1:4,1:4]

### Mining association rules ------------------------------

# Extracting frequent itemsets of min size 2
# Extract the set of most frequent itemsets
itemsets_freq2 <-
  apriori(Groceries,parameter = list(supp = 0.01,minlen = 2,target = 'frequent'))

# Sorting and inspecting frequent itemsets
inspect(head(sort(itemsets_freq2, by="support")))


# Rules with the apriori
rules = apriori(Groceries, parameter = list(supp=.001,conf=.5,minlen=2,target='rules'))

inspect(head(sort(rules, by="confidence")))

##### Choose parameters arules ---------------------------------------------------------


# Set of confidence levels
confidenceLevels <- seq(from=0.1, to=0.9, by =0.1)
confidenceLevels
# Create empty vector
rules_sup0005 <- NULL

# Apriori algorithm with a support level of 0.5%
for (i in 1:length(confidenceLevels)) {
  rules_sup0005[i] =
    length(apriori(Groceries,
                   parameter=list(supp=0.005,
                                  conf=confidenceLevels[i],
                                  target="rules")))
}


## Plotting  -----------------------------------
library(ggplot2)
# Number of rules found with a support level of 0.5%
qplot(confidenceLevels, rules_sup0005,
      geom=c("point", "line"),xlab="Confidence level",
      ylab="Number of rules found") +
  theme_bw()

# Subsetting rules
inspect(subset(rules, subset =
                 items %in% c("soft cheese","whole milk") &
                 confidence >.95))

#### Interactive inspection --------------------------------------------
rules <- apriori(Groceries,parameter = list(supp=.001,conf=.5,minlen=2,target='rules'))


# DT : Datatable inspection

install.packages("arulesViz")
library(arulesViz)

inspectDT(rules)




# Plot rules as scatterplot -----------------------------
plot(rules, method = "scatterplot",engine = "html")


# Plot rules as graph
plot(rules, method = "graph",engine = "html")

########################################################################################
####### Visualizing transactions and rules ##### --------------------------------------

### Interactive inspection -------------------------
install.packages("arulesViz")
library(arulesViz)
library(arules)

rules = apriori(Groceries,parameter = list(supp=.001,conf=.5,minlen=2,target='rules'))

# Datatable inspection : "HTML" table
inspectDT(rules)

## Interactive scatterplots -------------------------------

# Plot rules as scatterplot :

plot(rules, method = "scatterplot",engine = "html")  # scatterplot

# Other types of plots using method :
plot(rules, method = "two-key plot",engine = "html")  # two-key plot

plot(rules, method = "matrix",engine = "html")        # matrix


## Interactive graphs ------------------------

# Plot rules as graph "
# The engine and the method
plot(rules, method = "graph",engine = "html")


## Interactive subgraphs --------------------------------------

#Sorting extracted rules
# Top 10 rules with highest confidence
top10_rules_Groceries =head(sort(rules,by = "confidence"), 10)

inspect(top10_rules_Groceries)

# Plot the top 10 rules
plot(top10_rules_Groceries,method = "graph", engine = "html")


# RuleExploring Groceries :

ruleExplorer(rules)

