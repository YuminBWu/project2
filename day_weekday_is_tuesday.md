Project 2
================
Yumin Wu
October 10, 2020

``` r
library(shiny)
library(dplyr)
library(caret)
library(ggplot2)
library(purrr)
library(tidyr)
library(corrplot)
library(rpart)
library(gbm)
library(rmarkdown)
```

Introduction
------------

The data set is about the number of sharing articles on a website. There are 19 predictors in this data. Such as: Number of words in the title,Number of words in the content, Number of links, Number of links to other articles published by Mashable, Number of links to other articles published by Mashable, Number of images,Number of videos, different kinds of channels (Lifestyle,Entertainment,Business, Social Media, Tech, World...) and if the article is published on Monday, Tuesday, Wednesday, etc. The purpose of my analysis is to find which factors would lead to more shares. I would use regression tree model and boosted tree model.

``` r
#Read in and process data
OnlineNewsPopularity<-read.csv("dataset/OnlineNewsPopularity.csv")

OnlineNewsPopularity<-OnlineNewsPopularity%>%select(n_tokens_title,n_tokens_content,num_hrefs,num_self_hrefs,num_imgs,num_videos,data_channel_is_lifestyle,data_channel_is_entertainment,data_channel_is_bus,data_channel_is_socmed,data_channel_is_tech,data_channel_is_world,weekday_is_monday,weekday_is_tuesday,weekday_is_wednesday,weekday_is_thursday,weekday_is_friday,weekday_is_saturday,weekday_is_sunday,shares)

OnlineNewsPopularity<-tbl_df(OnlineNewsPopularity)

OnlineNewsPopularity$data_channel_is_lifestyle<-as.factor(OnlineNewsPopularity$data_channel_is_lifestyle)
OnlineNewsPopularity$data_channel_is_entertainment<-as.factor(OnlineNewsPopularity$data_channel_is_entertainment)
OnlineNewsPopularity$data_channel_is_bus<-as.factor(OnlineNewsPopularity$data_channel_is_bus)
OnlineNewsPopularity$data_channel_is_socmed<-as.factor(OnlineNewsPopularity$data_channel_is_socmed)
OnlineNewsPopularity$data_channel_is_tech<-as.factor(OnlineNewsPopularity$data_channel_is_tech)
OnlineNewsPopularity$data_channel_is_world<-as.factor(OnlineNewsPopularity$data_channel_is_world)
OnlineNewsPopularity$weekday_is_monday<-as.factor(OnlineNewsPopularity$weekday_is_monday)
OnlineNewsPopularity$weekday_is_tuesday<-as.factor(OnlineNewsPopularity$weekday_is_tuesday)
OnlineNewsPopularity$weekday_is_wednesday<-as.factor(OnlineNewsPopularity$weekday_is_wednesday)
OnlineNewsPopularity$weekday_is_thursday<-as.factor(OnlineNewsPopularity$weekday_is_thursday)
OnlineNewsPopularity$weekday_is_friday<-as.factor(OnlineNewsPopularity$weekday_is_friday)
OnlineNewsPopularity$weekday_is_saturday<-as.factor(OnlineNewsPopularity$weekday_is_saturday)
OnlineNewsPopularity$weekday_is_sunday<-as.factor(OnlineNewsPopularity$weekday_is_sunday)
```

``` r
# Form training and test data
set.seed(1)
nr = nrow(OnlineNewsPopularity)
train <- sample(1:nr, size = nr*0.7)
test <- setdiff(1:nr, train)
OnlineNewsPopDataTrain <- OnlineNewsPopularity[train, ]
OnlineNewsPopDataTest <- OnlineNewsPopularity[test, ]
```

Visual plots
------------

The first plot is the correlation among the variables. The scatter plots show the relationships between the number of shares and the number of words in contents, the number of links, the number of pictures and the number of videos.

``` r
#Summarization
news_correlation <- cor(select(OnlineNewsPopDataTrain,n_tokens_title,n_tokens_content,num_hrefs, num_self_hrefs,num_imgs,num_videos,shares))
corrplot(news_correlation, type = "upper", tl.pos = "lt")
corrplot(news_correlation, type = "lower", method = "number", add = TRUE, tl.pos = "n")
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(OnlineNewsPopDataTrain, aes(x = n_tokens_content, y = shares)) +
geom_point() 
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
ggplot(OnlineNewsPopDataTrain, aes(x = num_hrefs, y = shares)) +
geom_point() 
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
ggplot(OnlineNewsPopDataTrain, aes(x = num_videos, y = shares)) +
geom_point() 
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-4-4.png)

``` r
ggplot(OnlineNewsPopDataTrain, aes(x = num_imgs, y = shares)) +
geom_point()
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-4-5.png)

``` r
ggplot(OnlineNewsPopDataTrain, aes(x = data_channel_is_socmed, y = shares)) +
geom_point() 
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-4-6.png)

``` r
ggplot(OnlineNewsPopDataTrain, aes(x = data_channel_is_socmed, y = shares)) +
geom_point() 
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-4-7.png)

``` r
ggplot(OnlineNewsPopDataTrain, aes(x = data_channel_is_entertainment, y = shares)) +
geom_point() 
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-4-8.png)

``` r
ggplot(OnlineNewsPopDataTrain, aes(x = data_channel_is_tech, y = shares)) +
geom_point() 
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-4-9.png)

``` r
ggplot(OnlineNewsPopDataTrain, aes(x = data_channel_is_world, y = shares)) +
geom_point() 
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-4-10.png)

``` r
OnlineNewsPopDataTrain %>%summarise(
          count = n(),
          mean_tokens_title = mean(n_tokens_title, na.rm = TRUE),
          mean_tokens_content = mean(n_tokens_content, na.rm = TRUE),
          mean_imgs = mean(num_imgs, na.rm = TRUE),
          mean_videos = mean(num_videos, na.rm = TRUE),
          mean_links = mean(num_hrefs, na.rm = TRUE),
          mean_other_links = mean(num_self_hrefs, na.rm = TRUE)
          )
```

    ## # A tibble: 1 x 7
    ##   count mean_tokens_title mean_tokens_content mean_imgs mean_videos mean_links mean_other_links
    ##   <int>             <dbl>               <dbl>     <dbl>       <dbl>      <dbl>            <dbl>
    ## 1 27750              10.4                546.      4.57        1.25       10.9             3.28

Model
-----

For the unensembled tree model, I checked the correlation among the variables and the scatter plots to choose the predictors. Set the different values for minsplit and cp to choose the final model.Then choose the model with the smaller RMSE.
For the second model, I used the boosted tree method to build the model, set the grid for tuning parameters and used cross validation to choose the final model.

``` r
#a (not ensemble) tree-based model
set.seed(2)
cur_d = params$days
formula = paste0("shares~num_hrefs+num_imgs+num_videos+",cur_d,"+data_channel_is_world+data_channel_is_tech+data_channel_is_socmed+data_channel_is_bus+data_channel_is_entertainment")
formula = as.formula(formula)

treeFit0<-rpart(formula, data=OnlineNewsPopDataTrain, method="anova",control=rpart.control(minsplit=20, cp=0.001) )

treeFit<-rpart(formula, data=OnlineNewsPopDataTrain, method="anova",control=rpart.control(minsplit=30, cp=0.001) )

tree0Pred<-predict(treeFit0,newdata=select(OnlineNewsPopDataTest,-shares))
tree0RMSE<-sqrt(mean((tree0Pred-OnlineNewsPopDataTest$shares)^2))

treePred<-predict(treeFit,newdata=select(OnlineNewsPopDataTest,-shares))
treeRMSE<-sqrt(mean((treePred-OnlineNewsPopDataTest$shares)^2))

tree0RMSE
```

    ## [1] 14505.37

``` r
treeRMSE
```

    ## [1] 14441.41

``` r
plot(treeFit)
text(treeFit, cex=.6,use.n = TRUE, xpd = TRUE)
```

![](day_weekday_is_tuesday_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
#a boosted tree model chosen using cross-validation
cur_d = params$days
set.seed(2)

fitControl <- trainControl(
                           method = "cv",
                           number = 10)
gbmGrid <-  expand.grid(interaction.depth = c(1, 3), 
                        n.trees = 500, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
formula = paste0("shares~num_hrefs+num_imgs+num_videos+",cur_d,"+data_channel_is_world+data_channel_is_tech+data_channel_is_socmed+data_channel_is_bus+data_channel_is_entertainment")
formula = as.formula(formula)

gbmFit <- train(formula, data = OnlineNewsPopDataTrain, 
                 method = "gbm", 
                 preProcess = c("center", "scale"),
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid)
gbmFit
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 27750 samples
    ##     9 predictor
    ## 
    ## Pre-processing: centered (9), scaled (9) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 24975, 24974, 24975, 24976, 24975, 24975, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  RMSE      Rsquared     MAE     
    ##   1                  9783.582  0.015258906  3095.379
    ##   3                  9893.303  0.008728224  3135.713
    ## 
    ## Tuning parameter 'n.trees' was held constant at a value of 500
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## Tuning parameter 'n.minobsinnode' was held constant at
    ##  a value of 20
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 500, interaction.depth = 1, shrinkage = 0.1 and n.minobsinnode = 20.

Comparison
----------

I used the test data set to predict in order to compare the RMSE between two models. The final model should be the boosted tree model. As the RMSE of it is less than that of the unensembled tree model.

``` r
#Compare two models on the test set
treePred<-predict(treeFit,newdata=select(OnlineNewsPopDataTest,-shares))
treeRMSE<-sqrt(mean((treePred-OnlineNewsPopDataTest$shares)^2))

boostPred<-predict(gbmFit,newdata=select(OnlineNewsPopDataTest,-shares),n.trees=500)
boostRMSE<-sqrt(mean((boostPred-OnlineNewsPopDataTest$shares)^2))
c(tree=treeRMSE,boost=boostRMSE)
```

    ##     tree    boost 
    ## 14441.41 14348.07

Linear Model: Training
----------------------

``` r
lmFit <- train(shares ~ ., data = OnlineNewsPopDataTrain,
               method = "lm")

summary(lmFit)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -10547  -2183  -1436   -356 686523 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     4701.7226   417.3424  11.266  < 2e-16 ***
    ## n_tokens_title                    76.8931    29.3022   2.624 0.008692 ** 
    ## n_tokens_content                  -0.2866     0.1577  -1.817 0.069176 .  
    ## num_hrefs                         33.8080     6.4063   5.277 1.32e-07 ***
    ## num_self_hrefs                   -50.6917    18.0292  -2.812 0.004932 ** 
    ## num_imgs                          30.9077     8.4667   3.651 0.000262 ***
    ## num_videos                        46.0276    15.5145   2.967 0.003012 ** 
    ## data_channel_is_lifestyle1     -1889.3599   316.1219  -5.977 2.31e-09 ***
    ## data_channel_is_entertainment1 -2609.7467   219.3139 -11.900  < 2e-16 ***
    ## data_channel_is_bus1           -2124.0521   233.6870  -9.089  < 2e-16 ***
    ## data_channel_is_socmed1        -1640.9587   303.4945  -5.407 6.47e-08 ***
    ## data_channel_is_tech1          -2240.3281   225.5883  -9.931  < 2e-16 ***
    ## data_channel_is_world1         -3046.8155   218.8074 -13.925  < 2e-16 ***
    ## weekday_is_monday1               186.0918   276.3400   0.673 0.500688    
    ## weekday_is_tuesday1             -494.1258   273.2155  -1.809 0.070531 .  
    ## weekday_is_wednesday1           -601.7539   272.5810  -2.208 0.027279 *  
    ## weekday_is_thursday1            -360.5618   273.4519  -1.319 0.187328    
    ## weekday_is_friday1              -483.1778   282.0410  -1.713 0.086697 .  
    ## weekday_is_saturday1             157.7468   339.0881   0.465 0.641785    
    ## weekday_is_sunday1                     NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10130 on 27731 degrees of freedom
    ## Multiple R-squared:  0.01458,    Adjusted R-squared:  0.01394 
    ## F-statistic: 22.79 on 18 and 27731 DF,  p-value: < 2.2e-16

Linear Model: Test
------------------

``` r
lmPred <- predict(lmFit, newdata = OnlineNewsPopDataTest)
lmPred.res <- postResample(lmPred, OnlineNewsPopDataTest$shares)
round(lmPred.res, 3)
```

    ##      RMSE  Rsquared       MAE 
    ## 14357.861     0.009  3173.035
