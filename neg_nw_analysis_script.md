Analysis of Households with Negative Net Worth Using Data from the 2019
Survey of Consumer Finances
================
Shehryar Nabi, Senior Research Associate, Aspen Institute Financial
Security Program

# Solutions Scan

## Share of households with negative net worth for each year from 1989-2019

``` r
# 1989 (below process repeated for each year)

# New binary variable that distinguishes net debt HHs from all HHs
scf_89 <- mutate(scf_89, 
                 netdebt_status = ifelse(scf_89$NETWORTH < 0, 1, 0))

# Taking weighted share of net debt households and all else
wpct(scf_89$netdebt_status, scf_89$WGT)
```

    ##          0          1 
    ## 0.92565811 0.07434189

``` r
# 1992

scf_92 <- mutate(scf_92, 
                 netdebt_status = ifelse(scf_92$NETWORTH < 0, 1, 0))

wpct(scf_92$netdebt_status, scf_92$WGT)
```

    ##          0          1 
    ## 0.92902195 0.07097805

``` r
# 1995

scf_95 <- mutate(scf_95, 
                 netdebt_status = ifelse(scf_95$NETWORTH < 0, 1, 0))

wpct(scf_95$netdebt_status, scf_95$WGT)
```

    ##          0          1 
    ## 0.92944551 0.07055449

``` r
# 1998

scf_98 <- mutate(scf_98, 
                 netdebt_status = ifelse(scf_98$NETWORTH < 0, 1, 0))

wpct(scf_98$netdebt_status, scf_98$WGT)
```

    ##          0          1 
    ## 0.92037308 0.07962692

``` r
# 2001

scf_01 <- mutate(scf_01, 
                 netdebt_status = ifelse(scf_01$NETWORTH < 0, 1, 0))

wpct(scf_01$netdebt_status, scf_01$WGT)
```

    ##          0          1 
    ## 0.93197087 0.06802913

``` r
# 2004

scf_04 <- mutate(scf_04, 
                 netdebt_status = ifelse(scf_04$NETWORTH < 0, 1, 0))

wpct(scf_04$netdebt_status, scf_04$WGT)
```

    ##          0          1 
    ## 0.92860293 0.07139707

``` r
# 2007

scf_07 <- mutate(scf_07, 
                 netdebt_status = ifelse(scf_07$NETWORTH < 0, 1, 0))

wpct(scf_07$netdebt_status, scf_07$WGT)
```

    ##         0         1 
    ## 0.9223927 0.0776073

``` r
# 2010 

scf_10 <- mutate(scf_10, 
                 netdebt_status = ifelse(scf_10$NETWORTH < 0, 1, 0))

wpct(scf_10$netdebt_status, scf_10$WGT)
```

    ##         0         1 
    ## 0.8896416 0.1103584

``` r
# 2013 

scf_13 <- mutate(scf_13, 
                 netdebt_status = ifelse(scf_13$NETWORTH < 0, 1, 0))

wpct(scf_13$netdebt_status, scf_13$WGT)
```

    ##         0         1 
    ## 0.8840618 0.1159382

``` r
# 2016

scf_16 <- mutate(scf_16, 
                 netdebt_status = ifelse(scf_16$NETWORTH < 0, 1, 0))

wpct(scf_16$netdebt_status, scf_16$WGT)
```

    ##         0         1 
    ## 0.8900018 0.1099982

``` r
# 2019

scf_19 <- mutate(scf_19, 
                 netdebt_status = ifelse(scf_19$NETWORTH < 0, 1, 0))

wpct(scf_19$netdebt_status, scf_19$WGT)
```

    ##         0         1 
    ## 0.8956651 0.1043349

## Income and net worth

I calculate the differences in median income, assets, debt, and net
worth between negative net worth households and all households. I also
calculate the share of households with negative net worth by income
quintile.

``` r
# Negative net worth households in 2019

neg_nw_19 <- filter(scf_19, scf_19$NETWORTH < 0)

# Comparing median income of households with and without net debt

weighted.median(neg_nw_19$INCOME, neg_nw_19$WGT)
```

    ## [1] 39706.6

``` r
weighted.median(scf_19$INCOME, scf_19$WGT)
```

    ## [1] 59050.84

``` r
# Getting median debt, asset and net worth values for all HHs with net debt

neg_nw_19_wealth <- neg_nw_19 %>%
  group_by(INCOME_quintiles) %>%
  summarise(median_debt = weighted.median(DEBT, WGT)/1000,
            median_assets = weighted.median(ASSET, WGT)/1000,
            median_nw = weighted.median(NETWORTH, WGT)/1000, 
            n = n())


# Getting share of net debtors by income quintile

neg_nw_19_wealth$prop <- wpct(neg_nw_19$INCOME_quintiles, neg_nw_19$WGT)


# Exporting data

write.csv(neg_nw_19_wealth, paste(getwd(), '\\csv-exports', 
                '\\neg_nw_wealth.csv', 
                sep=''))
```

## Racial breakdown of households with negative net worth

``` r
# Data frame comparing racial breakdown of all HHs vs net debtors

race_scf_netdebtors <- data.frame(scf = wpct(scf_19$RACE, 
                                             scf_19$WGT),
                                  net_debtors = wpct(neg_nw_19$RACE,
                                                     neg_nw_19$WGT))


# Exporting data 

write.csv(race_scf_netdebtors,  paste(getwd(), '\\csv-exports', 
                '\\race_scf_netdebtors.csv', 
                sep=''))
```

## Age and family structure of households with negative net worth compared to the overall population

See code descriptions for family structure here:
<https://sda.berkeley.edu/sdaweb/docs/scfcomb2019/DOC/hcbk0001.htm#FAMSTRUCT>.

``` r
# Age 

weighted.median(scf_19$AGE, scf_19$WGT)
```

    ## [1] 52

``` r
weighted.median(neg_nw_19$AGE, neg_nw_19$WGT)
```

    ## [1] 34.39521

``` r
# Family structure

wpct(scf_19$FAMSTRUCT, scf_19$WGT)
```

    ##         1         2         3         4         5 
    ## 0.1087220 0.1506436 0.1801701 0.2776858 0.2827786

``` r
wpct(neg_nw_19$FAMSTRUCT, neg_nw_19$WGT)
```

    ##         1         2         3         4         5 
    ## 0.1961853 0.2976189 0.1085989 0.2078087 0.1897882

``` r
# Gender

wpct(scf_19$HHSEX, scf_19$WGT)
```

    ##         1         2 
    ## 0.7304569 0.2695431

``` r
wpct(neg_nw_19$HHSEX, neg_nw_19$WGT)
```

    ##         1         2 
    ## 0.5610977 0.4389023

``` r
# Gender breakdown of single parents

wpct(filter(scf_19, scf_19$FAMSTRUCT == 1)$HHSEX,
     filter(scf_19, scf_19$FAMSTRUCT == 1)$WGT)
```

    ##         1         2 
    ## 0.1943099 0.8056901

``` r
wpct(filter(neg_nw_19, neg_nw_19$FAMSTRUCT == 1)$HHSEX,
     filter(neg_nw_19, neg_nw_19$FAMSTRUCT == 1)$WGT)
```

    ##          1          2 
    ## 0.09518594 0.90481406

``` r
# Family structure breakdown of women-respondent HHs

wpct(filter(scf_19, scf_19$HHSEX == 2)$FAMSTRUCT,
     filter(scf_19, scf_19$HHSEX == 2)$WGT)
```

    ##          1          2          3          4          5 
    ## 0.32498036 0.22781932 0.41688501 0.01504148 0.01527383

``` r
wpct(filter(neg_nw_19, neg_nw_19$HHSEX == 2)$FAMSTRUCT,
     filter(neg_nw_19, neg_nw_19$HHSEX == 2)$WGT)
```

    ##          1          2          3          4          5 
    ## 0.40444361 0.38155795 0.18069860 0.01897267 0.01432717

## Debt holding rates

I calculate the proportion of negative net worth households holding
different types of debt.

``` r
debt_hold <- data.frame(RESDBT = wpct(neg_nw_19$RESDBT > 0,
                                      neg_nw_19$WGT)[1],
                        MRTHEL = wpct(neg_nw_19$MRTHEL > 0, 
                                      neg_nw_19$WGT)[1],
                        CCBAL = wpct(neg_nw_19$CCBAL > 0, 
                                     neg_nw_19$WGT)[1],
                        EDN_INST = wpct(neg_nw_19$EDN_INST > 0, 
                                        neg_nw_19$WGT)[1],
                        VEH_INST = wpct(neg_nw_19$VEH_INST > 0, 
                                        neg_nw_19$WGT)[1],
                        OTHDBT = wpct(neg_nw_19$OTHDBT > 0, 
                                      neg_nw_19$WGT)[1])


# Exporting data

write.csv(debt_hold, paste(getwd(), '\\csv-exports', 
                '\\debt_hold.csv', 
                sep=''))
```

## Median debt

I calculate the median value per debt type among households holding it
for both households with negative net worth and overall.

``` r
# Calculating median debt value for select debt types among net debtors 
# holding them

debts_holding <- data.frame(RESDBT = weighted.median(filter(neg_nw_19,neg_nw_19$RESDBT > 0)$RESDBT, 
                filter(neg_nw_19,neg_nw_19$RESDBT > 0)$WGT),
                            MRTHEL = 
weighted.median(filter(neg_nw_19,neg_nw_19$MRTHEL > 0)$MRTHEL, 
                filter(neg_nw_19,neg_nw_19$MRTHEL > 0)$WGT),
                            CCBAL = 
weighted.median(filter(neg_nw_19,neg_nw_19$CCBAL > 0)$CCBAL, 
                filter(neg_nw_19,neg_nw_19$CCBAL > 0)$WGT),
                            EDN_INST = 
weighted.median(filter(neg_nw_19,neg_nw_19$EDN_INST > 0)$EDN_INST, 
                filter(neg_nw_19,neg_nw_19$EDN_INST > 0)$WGT),
                            VEH_INST = 
weighted.median(filter(neg_nw_19,neg_nw_19$VEH_INST > 0)$VEH_INST, 
                filter(neg_nw_19,neg_nw_19$VEH_INST > 0)$WGT),
                            OTHDBT = 
weighted.median(filter(neg_nw_19,neg_nw_19$OTHDBT > 0)$OTHDBT, 
                filter(neg_nw_19,neg_nw_19$OTHDBT > 0)$WGT))                              

# Export to csv 

write.csv(debts_holding,  paste(getwd(), '\\csv-exports', 
                '\\debts_holding.csv', 
                sep=''))
```

# Supplementary Figures

## Net debtors by age group and race

I find the age distribution of households with negative net worth
compared to overall households. I also calculate the proportion of
households with net debt by race and age.

``` r
# Getting the distribution of ages for negative net worth HHs and overall

wpct(scf_19$age_group, scf_19$WGT)
```

    ##  Under 35     35-54   Over 55 
    ## 0.2087219 0.3377451 0.4535330

``` r
wpct(neg_nw_19$age_group, neg_nw_19$WGT)
```

    ##  Under 35     35-54   Over 55 
    ## 0.4998895 0.3223889 0.1777215

``` r
# Finding share of HHs with net debt and debt types by age group and race

prop_nd_race <- scf_19 %>%
  group_by(age_group, RACE) %>%
  summarise(prop = wpct(NETWORTH < 0, WGT)[1],
            prop_edu_all = wpct(EDN_INST > 0, WGT)[1],
            prop_edu = wpct(NETWORTH < 0 & EDN_INST > 0, WGT)[1],
            prop_veh = wpct(NETWORTH < 0 & VEH_INST > 0, WGT)[1],
            prop_ccbal = wpct(NETWORTH < 0 & CCBAL > 0, WGT)[1],
            prop_mort = wpct(NETWORTH < 0 & MRTHEL > 0, WGT)[1],
            prop_rdebt = wpct(NETWORTH < 0 & RESDBT > 0, WGT)[1],
            count = n())
```

    ## `summarise()` has grouped output by 'age_group'. You can override using the `.groups` argument.

``` r
# Exporting to CSV 

write.csv(prop_nd_race, paste(getwd(), '\\csv-exports', 
                '\\prop_nd_race.csv', 
                sep=''))
```

## Median asset values for negative net worth households holding the asset in 2019

``` r
assets_holding_19 <- data.frame(LIQ = weighted.median(filter(neg_nw_19, neg_nw_19$LIQ >0)$LIQ,filter(neg_nw_19, neg_nw_19$LIQ > 0)$WGT),
                            SECURITIES = 
weighted.median(filter(neg_nw_19,neg_nw_19$SECURITIES > 0)$SECURITIES, 
                filter(neg_nw_19,neg_nw_19$SECURITIES > 0)$WGT),
                            RETQLIQ = 
weighted.median(filter(neg_nw_19,neg_nw_19$RETQLIQ > 0)$RETQLIQ, 
                filter(neg_nw_19,neg_nw_19$RETQLIQ > 0)$WGT),
                            OTHFIN_COMPLETE = 
weighted.median(filter(neg_nw_19,neg_nw_19$OTHFIN_COMPLETE > 0)$OTHFIN_COMPLETE, 
                filter(neg_nw_19,neg_nw_19$OTHFIN_COMPLETE > 0)$WGT),
                            VEHIC = 
weighted.median(filter(neg_nw_19,neg_nw_19$VEHIC> 0)$VEHIC, 
                filter(neg_nw_19,neg_nw_19$VEHIC > 0)$WGT),
                            HOUSES = 
weighted.median(filter(neg_nw_19,neg_nw_19$HOUSES > 0)$HOUSES, 
                filter(neg_nw_19,neg_nw_19$HOUSES > 0)$WGT),
                            NONRES = 
weighted.median(filter(neg_nw_19,neg_nw_19$NONRES > 0)$NONRES, 
                filter(neg_nw_19,neg_nw_19$NONRES > 0)$WGT),
                            BUS = 
weighted.median(filter(neg_nw_19,neg_nw_19$BUS > 0)$BUS, 
                filter(neg_nw_19,neg_nw_19$BUS > 0)$WGT),
                            OTHNFIN = 
weighted.median(filter(neg_nw_19,neg_nw_19$OTHNFIN > 0)$OTHNFIN, 
                filter(neg_nw_19,neg_nw_19$OTHNFIN > 0)$WGT))  
```

## Median asset value for negative net worth households overall by year

``` r
## Separating negative net worth households for SCF waves from 2007 to 2016. 

neg_nw_07 <- filter(scf_07, scf_07$NETWORTH < 0)
neg_nw_10 <- filter(scf_10, scf_10$NETWORTH < 0)
neg_nw_13 <- filter(scf_13, scf_13$NETWORTH < 0)
neg_nw_16 <- filter(scf_16, scf_16$NETWORTH < 0)


## Calculating overall median asset value by asset type and year

assets_med_19 <- data.frame(transaction = weighted.median(neg_nw_19$LIQ,
                                                          neg_nw_19$WGT)/1000,
                  securities = weighted.median(neg_nw_19$SECURITIES, 
                                               neg_nw_19$WGT)/1000,
                   ret = weighted.median(neg_nw_19$RETQLIQ, 
                                         neg_nw_19$WGT)/1000,
                   othfin = weighted.median(neg_nw_19$OTHFIN_COMPLETE,
                                            neg_nw_19$WGT)/1000,
                   vehic = weighted.median(neg_nw_19$VEHIC, 
                                           neg_nw_19$WGT)/1000,
                   res = weighted.median(neg_nw_19$HOUSES, 
                                         neg_nw_19$WGT)/1000,
                   nonres = weighted.median(neg_nw_19$NONRES, 
                                            neg_nw_19$WGT)/1000,
                   business = weighted.median(neg_nw_19$BUS, 
                                              neg_nw_19$WGT)/1000,
                   othnfin = weighted.median(neg_nw_19$OTHNFIN, 
                                             neg_nw_19$WGT)/1000)


assets_med_16 <- data.frame(transaction = weighted.median(neg_nw_16$LIQ,
                                                          neg_nw_16$WGT)/1000,
                  securities = weighted.median(neg_nw_16$SECURITIES, 
                                               neg_nw_16$WGT)/1000,
                   ret = weighted.median(neg_nw_16$RETQLIQ, 
                                         neg_nw_16$WGT)/1000,
                   othfin = weighted.median(neg_nw_16$OTHFIN_COMPLETE,
                                            neg_nw_16$WGT)/1000,
                   vehic = weighted.median(neg_nw_16$VEHIC, 
                                           neg_nw_16$WGT)/1000,
                   res = weighted.median(neg_nw_16$HOUSES, 
                                         neg_nw_16$WGT)/1000,
                   nonres = weighted.median(neg_nw_16$NONRES, 
                                            neg_nw_16$WGT)/1000,
                   business = weighted.median(neg_nw_16$BUS, 
                                              neg_nw_16$WGT)/1000,
                   othnfin = weighted.median(neg_nw_16$OTHNFIN, 
                                             neg_nw_16$WGT)/1000)


assets_med_13 <- data.frame(transaction = weighted.median(neg_nw_13$LIQ,
                                                          neg_nw_13$WGT)/1000,
                  securities = weighted.median(neg_nw_13$SECURITIES, 
                                               neg_nw_13$WGT)/1000,
                   ret = weighted.median(neg_nw_13$RETQLIQ, 
                                         neg_nw_13$WGT)/1000,
                   othfin = weighted.median(neg_nw_13$OTHFIN_COMPLETE,
                                            neg_nw_13$WGT)/1000,
                   vehic = weighted.median(neg_nw_13$VEHIC, 
                                           neg_nw_13$WGT)/1000,
                   res = weighted.median(neg_nw_13$HOUSES, 
                                         neg_nw_13$WGT)/1000,
                   nonres = weighted.median(neg_nw_13$NONRES, 
                                            neg_nw_13$WGT)/1000,
                   business = weighted.median(neg_nw_13$BUS, 
                                              neg_nw_13$WGT)/1000,
                   othnfin = weighted.median(neg_nw_13$OTHNFIN, 
                                             neg_nw_13$WGT)/1000)


assets_med_10 <- data.frame(transaction = weighted.median(neg_nw_10$LIQ,
                                                          neg_nw_10$WGT)/1000,
                  securities = weighted.median(neg_nw_10$SECURITIES, 
                                               neg_nw_10$WGT)/1000,
                   ret = weighted.median(neg_nw_10$RETQLIQ, 
                                         neg_nw_10$WGT)/1000,
                   othfin = weighted.median(neg_nw_10$OTHFIN_COMPLETE,
                                            neg_nw_10$WGT)/1000,
                   vehic = weighted.median(neg_nw_10$VEHIC, 
                                           neg_nw_10$WGT)/1000,
                   res = weighted.median(neg_nw_10$HOUSES, 
                                         neg_nw_10$WGT)/1000,
                   nonres = weighted.median(neg_nw_10$NONRES, 
                                            neg_nw_10$WGT)/1000,
                   business = weighted.median(neg_nw_10$BUS, 
                                              neg_nw_10$WGT)/1000,
                   othnfin = weighted.median(neg_nw_10$OTHNFIN, 
                                             neg_nw_10$WGT)/1000)



assets_med_07 <- data.frame(transaction = weighted.median(neg_nw_07$LIQ,
                                                          neg_nw_07$WGT)/1000,
                  securities = weighted.median(neg_nw_07$SECURITIES, 
                                               neg_nw_07$WGT)/1000,
                   ret = weighted.median(neg_nw_07$RETQLIQ, 
                                         neg_nw_07$WGT)/1000,
                   othfin = weighted.median(neg_nw_07$OTHFIN_COMPLETE,
                                            neg_nw_07$WGT)/1000,
                   vehic = weighted.median(neg_nw_07$VEHIC, 
                                           neg_nw_07$WGT)/1000,
                   res = weighted.median(neg_nw_07$HOUSES, 
                                         neg_nw_07$WGT)/1000,
                   nonres = weighted.median(neg_nw_07$NONRES, 
                                            neg_nw_07$WGT)/1000,
                   business = weighted.median(neg_nw_07$BUS, 
                                              neg_nw_07$WGT)/1000,
                   othnfin = weighted.median(neg_nw_07$OTHNFIN, 
                                             neg_nw_07$WGT)/1000)
```

## Asset holding rate by year

``` r
assets_pct_19 <- data.frame(trans = sum(neg_nw_19$LIQ > 0)/nrow(neg_nw_19),
           sec = sum(neg_nw_19$SECURITIES > 0)/nrow(neg_nw_19),
           ret = sum(neg_nw_19$RETQLIQ > 0)/nrow(neg_nw_19),
           othfin = sum(neg_nw_19$OTHFIN_COMPLETE > 0)/nrow(neg_nw_19),
           veh = sum(neg_nw_19$VEHIC > 0)/nrow(neg_nw_19),
           res = sum(neg_nw_19$HOUSES > 0)/nrow(neg_nw_19),
           nres = sum(neg_nw_19$NONRES > 0)/nrow(neg_nw_19),
           bus = sum(neg_nw_19$BUS > 0)/nrow(neg_nw_19),
           othnfin = sum(neg_nw_19$OTHNFIN > 0)/nrow(neg_nw_19))
           

assets_pct_16 <- data.frame(trans = sum(neg_nw_16$LIQ > 0)/nrow(neg_nw_16),
           sec = sum(neg_nw_16$SECURITIES > 0)/nrow(neg_nw_16),
           ret = sum(neg_nw_16$RETQLIQ > 0)/nrow(neg_nw_16),
           othfin = sum(neg_nw_16$OTHFIN_COMPLETE > 0)/nrow(neg_nw_16),
           veh = sum(neg_nw_16$VEHIC > 0)/nrow(neg_nw_16),
           res = sum(neg_nw_16$HOUSES > 0)/nrow(neg_nw_16),
           nres = sum(neg_nw_16$NONRES > 0)/nrow(neg_nw_16),
           bus = sum(neg_nw_16$BUS > 0)/nrow(neg_nw_16),
           othnfin = sum(neg_nw_16$OTHNFIN > 0)/nrow(neg_nw_16))


assets_pct_13 <- data.frame(trans = sum(neg_nw_13$LIQ > 0)/nrow(neg_nw_13),
           sec = sum(neg_nw_13$SECURITIES > 0)/nrow(neg_nw_13),
           ret = sum(neg_nw_13$RETQLIQ > 0)/nrow(neg_nw_13),
           othfin = sum(neg_nw_13$OTHFIN_COMPLETE > 0)/nrow(neg_nw_13),
           veh = sum(neg_nw_13$VEHIC > 0)/nrow(neg_nw_13),
           res = sum(neg_nw_13$HOUSES > 0)/nrow(neg_nw_13),
           nres = sum(neg_nw_13$NONRES > 0)/nrow(neg_nw_13),
           bus = sum(neg_nw_13$BUS > 0)/nrow(neg_nw_13),
           othnfin = sum(neg_nw_13$OTHNFIN > 0)/nrow(neg_nw_13))


assets_pct_10 <- data.frame(trans = sum(neg_nw_10$LIQ > 0)/nrow(neg_nw_10),
           sec = sum(neg_nw_10$SECURITIES > 0)/nrow(neg_nw_10),
           ret = sum(neg_nw_10$RETQLIQ > 0)/nrow(neg_nw_10),
           othfin = sum(neg_nw_10$OTHFIN_COMPLETE > 0)/nrow(neg_nw_10),
           veh = sum(neg_nw_10$VEHIC > 0)/nrow(neg_nw_10),
           res = sum(neg_nw_10$HOUSES > 0)/nrow(neg_nw_10),
           nres = sum(neg_nw_10$NONRES > 0)/nrow(neg_nw_10),
           bus = sum(neg_nw_10$BUS > 0)/nrow(neg_nw_10),
           othnfin = sum(neg_nw_10$OTHNFIN > 0)/nrow(neg_nw_10))


assets_pct_07 <- data.frame(trans = sum(neg_nw_07$LIQ > 0)/nrow(neg_nw_07),
           sec = sum(neg_nw_07$SECURITIES > 0)/nrow(neg_nw_07),
           ret = sum(neg_nw_07$RETQLIQ > 0)/nrow(neg_nw_07),
           othfin = sum(neg_nw_07$OTHFIN_COMPLETE > 0)/nrow(neg_nw_07),
           veh = sum(neg_nw_07$VEHIC > 0)/nrow(neg_nw_07),
           res = sum(neg_nw_07$HOUSES > 0)/nrow(neg_nw_07),
           nres = sum(neg_nw_07$NONRES > 0)/nrow(neg_nw_07),
           bus = sum(neg_nw_07$BUS > 0)/nrow(neg_nw_07),
           othnfin = sum(neg_nw_07$OTHNFIN > 0)/nrow(neg_nw_07))
```

``` r
debts_med_19 <- data.frame(RESDBT = weighted.median(neg_nw_19$RESDBT,
                                                    neg_nw_19$WGT)/1000,
                  MRTHEL = weighted.median(neg_nw_19$MRTHEL, 
                                               neg_nw_19$WGT)/1000,
                   CCBAL = weighted.median(neg_nw_19$CCBAL, 
                                         neg_nw_19$WGT)/1000,
                   EDN_INST = weighted.median(neg_nw_19$EDN_INST,
                                            neg_nw_19$WGT)/1000,
                   VEH_INST = weighted.median(neg_nw_19$VEH_INST, 
                                           neg_nw_19$WGT)/1000,
                   OTHDBT = weighted.median(neg_nw_19$OTHDBT,
                                            neg_nw_19$WGT)/1000)


debts_med_16 <- data.frame(RESDBT = weighted.median(neg_nw_16$RESDBT,
                                                    neg_nw_16$WGT)/1000,
                  MRTHEL = weighted.median(neg_nw_16$MRTHEL, 
                                               neg_nw_16$WGT)/1000,
                   CCBAL = weighted.median(neg_nw_16$CCBAL, 
                                         neg_nw_16$WGT)/1000,
                   EDN_INST = weighted.median(neg_nw_16$EDN_INST,
                                            neg_nw_16$WGT)/1000,
                   VEH_INST = weighted.median(neg_nw_16$VEH_INST, 
                                           neg_nw_16$WGT)/1000,
                   OTHDBT = weighted.median(neg_nw_16$OTHDBT,
                                            neg_nw_16$WGT)/1000)


debts_med_13 <- data.frame(RESDBT = weighted.median(neg_nw_13$RESDBT,
                                                    neg_nw_13$WGT)/1000,
                  MRTHEL = weighted.median(neg_nw_13$MRTHEL, 
                                               neg_nw_13$WGT)/1000,
                   CCBAL = weighted.median(neg_nw_13$CCBAL, 
                                         neg_nw_13$WGT)/1000,
                   EDN_INST = weighted.median(neg_nw_13$EDN_INST,
                                            neg_nw_13$WGT)/1000,
                   VEH_INST = weighted.median(neg_nw_13$VEH_INST, 
                                           neg_nw_13$WGT)/1000,
                   OTHDBT = weighted.median(neg_nw_13$OTHDBT,
                                            neg_nw_13$WGT)/1000)


debts_med_10 <- data.frame(RESDBT = weighted.median(neg_nw_10$RESDBT,
                                                    neg_nw_10$WGT)/1000,
                  MRTHEL = weighted.median(neg_nw_10$MRTHEL, 
                                               neg_nw_10$WGT)/1000,
                   CCBAL = weighted.median(neg_nw_10$CCBAL, 
                                         neg_nw_10$WGT)/1000,
                   EDN_INST = weighted.median(neg_nw_10$EDN_INST,
                                            neg_nw_10$WGT)/1000,
                   VEH_INST = weighted.median(neg_nw_10$VEH_INST, 
                                           neg_nw_10$WGT)/1000,
                   OTHDBT = weighted.median(neg_nw_10$OTHDBT,
                                            neg_nw_10$WGT)/1000)



debts_med_07 <- data.frame(RESDBT = weighted.median(neg_nw_07$RESDBT,
                                                    neg_nw_07$WGT)/1000,
                  MRTHEL = weighted.median(neg_nw_07$MRTHEL, 
                                               neg_nw_07$WGT)/1000,
                   CCBAL = weighted.median(neg_nw_07$CCBAL, 
                                         neg_nw_07$WGT)/1000,
                   EDN_INST = weighted.median(neg_nw_07$EDN_INST,
                                            neg_nw_07$WGT)/1000,
                   VEH_INST = weighted.median(neg_nw_07$VEH_INST, 
                                           neg_nw_07$WGT)/1000,
                   OTHDBT = weighted.median(neg_nw_07$OTHDBT,
                                            neg_nw_07$WGT)/1000)
```

``` r
debt_pct_19 <- data.frame(RESDBT = sum(neg_nw_19$RESDBT > 0)/nrow(neg_nw_19),
           MRTHEL = sum(neg_nw_19$MRTHEL > 0)/nrow(neg_nw_19),
           CCBAL = sum(neg_nw_19$CCBAL > 0)/nrow(neg_nw_19),
           EDN_INST = sum(neg_nw_19$EDN_INST > 0)/nrow(neg_nw_19),
           VEH_INST = sum(neg_nw_19$VEH_INST > 0)/nrow(neg_nw_19),
           OTHDBT = sum(neg_nw_19$OTHDBT > 0)/nrow(neg_nw_19))


debt_pct_16 <- data.frame(RESDBT = sum(neg_nw_16$RESDBT > 0)/nrow(neg_nw_16),
           MRTHEL = sum(neg_nw_16$MRTHEL > 0)/nrow(neg_nw_16),
           CCBAL = sum(neg_nw_16$CCBAL > 0)/nrow(neg_nw_16),
           EDN_INST = sum(neg_nw_16$EDN_INST > 0)/nrow(neg_nw_16),
           VEH_INST = sum(neg_nw_16$VEH_INST > 0)/nrow(neg_nw_16),
           OTHDBT = sum(neg_nw_16$OTHDBT > 0)/nrow(neg_nw_16))

debt_pct_13 <- data.frame(RESDBT = sum(neg_nw_13$RESDBT > 0)/nrow(neg_nw_13),
           MRTHEL = sum(neg_nw_13$MRTHEL > 0)/nrow(neg_nw_13),
           CCBAL = sum(neg_nw_13$CCBAL > 0)/nrow(neg_nw_13),
           EDN_INST = sum(neg_nw_13$EDN_INST > 0)/nrow(neg_nw_13),
           VEH_INST = sum(neg_nw_13$VEH_INST > 0)/nrow(neg_nw_13),
           OTHDBT = sum(neg_nw_13$OTHDBT > 0)/nrow(neg_nw_13))


debt_pct_10 <- data.frame(RESDBT = sum(neg_nw_13$RESDBT > 0)/nrow(neg_nw_10),
           MRTHEL = sum(neg_nw_10$MRTHEL > 0)/nrow(neg_nw_10),
           CCBAL = sum(neg_nw_10$CCBAL > 0)/nrow(neg_nw_10),
           EDN_INST = sum(neg_nw_10$EDN_INST > 0)/nrow(neg_nw_10),
           VEH_INST = sum(neg_nw_10$VEH_INST > 0)/nrow(neg_nw_10),
           OTHDBT = sum(neg_nw_10$OTHDBT > 0)/nrow(neg_nw_10))


debt_pct_07 <- data.frame(RESDBT = sum(neg_nw_13$RESDBT > 0)/nrow(neg_nw_07),
           MRTHEL = sum(neg_nw_07$MRTHEL > 0)/nrow(neg_nw_07),
           CCBAL = sum(neg_nw_07$CCBAL > 0)/nrow(neg_nw_07),
           EDN_INST = sum(neg_nw_07$EDN_INST > 0)/nrow(neg_nw_07),
           VEH_INST = sum(neg_nw_07$VEH_INST > 0)/nrow(neg_nw_07),
           OTHDBT = sum(neg_nw_07$OTHDBT > 0)/nrow(neg_nw_07))

debts_time <- data.frame(year = c("2007", "2010", "2013", "2016", "2019"),
                         rbind(debt_pct_07, debt_pct_10, debt_pct_13, 
                               debt_pct_16, debt_pct_19))


write.csv(debts_time,  paste(getwd(), '\\csv-exports', 
                '\\debts_time.csv', 
                sep=''))
```

## Education level

I calculate the share of households by education level for those with
negative net worth and overall. See here for the level of education the
numbered categories correspond to:
<https://sda.berkeley.edu/sdaweb/docs/scfcomb2019/DOC/hcbk0001.htm#EDCL>.

``` r
# Education levels for all HHs and net worth HHs

wpct(scf_19$EDCL, scf_19$WGT)
```

    ##         1         2         3         4 
    ## 0.1072253 0.2448393 0.2846868 0.3632485

``` r
wpct(neg_nw_19$EDCL, neg_nw_19$WGT)
```

    ##          1          2          3          4 
    ## 0.07839683 0.16360847 0.37463233 0.38336238

## Income to Debt Ratio

``` r
# Debt to income ratio compared to overall population

weighted.median(neg_nw_19$DEBT2INC, neg_nw_19$WGT)
```

    ## [1] 1.16185

``` r
weighted.median(scf_19$DEBT2INC, scf_19$WGT)
```

    ## [1] 0.4679763

## Deliquent payments

``` r
# Late payment in the last year 

wpct(scf_19$LATE, scf_19$WGT)
```

    ##         0         1 
    ## 0.8769632 0.1230368

``` r
wpct(neg_nw_19$LATE, neg_nw_19$WGT)
```

    ##         0         1 
    ## 0.6666086 0.3333914

``` r
# Payment past 60 days due 

wpct(scf_19$LATE60, scf_19$WGT)
```

    ##          0          1 
    ## 0.95366669 0.04633331

``` r
wpct(neg_nw_19$LATE60, neg_nw_19$WGT)
```

    ##         0         1 
    ## 0.8394062 0.1605938
