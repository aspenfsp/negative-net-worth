Negative Net Worth SCF 2019
================

## Negative Net Worth Analysis

## Share of net debtors in overall population.

``` r
# New variable that distinguishes net debt HHs from all HHs

scf_19 <- mutate(scf_19, 
                 netdebt_status = ifelse(scf_19$NETWORTH < 0, 1, 0))


# Taking weighted share of net debt households and all else

wpct(scf_19$netdebt_status, scf_19$WGT)
```

    ##         0         1 
    ## 0.8956651 0.1043349

``` r
# New variable that distinguishes net debt HHs from all HHs

scf_07 <- mutate(scf_07, 
                 netdebt_status = ifelse(scf_07$NETWORTH < 0, 1, 0))


# Taking weighted share of net debt households and all else

wpct(scf_07$netdebt_status, scf_07$WGT)
```

    ##         0         1 
    ## 0.9223927 0.0776073

``` r
# New variable that distinguishes net debt HHs from all HHs

scf_89 <- mutate(scf_89, 
                 netdebt_status = ifelse(scf_89$NETWORTH < 0, 1, 0))


# Taking weighted share of net debt households and all else

wpct(scf_89$netdebt_status, scf_89$WGT)
```

    ##          0          1 
    ## 0.92565811 0.07434189

## Income quintile breakdowns

``` r
# Creating a new subset of HHs with negative net worth

neg_nw <- filter(scf_19, scf_19$NETWORTH < 0)


# Comparing median income of households with and without net debt

weighted.median(neg_nw$INCOME, neg_nw$WGT)
```

    ## [1] 39706.6

``` r
weighted.median(scf_19$INCOME, scf_19$WGT)
```

    ## [1] 59050.84

``` r
# Get median debt, asset and net worth values for all HHs with net debt

neg_nw_wealth <- neg_nw %>%
  group_by(INCOME_quintiles) %>%
  summarise(median_debt = weighted.median(DEBT, WGT)/1000,
            median_assets = weighted.median(ASSET, WGT)/1000,
            median_nw = weighted.median(NETWORTH, WGT)/1000, 
            n = n())


# Get share of net debtors by income quintile

neg_nw_wealth$prop <- wpct(neg_nw$INCOME_quintiles, neg_nw$WGT)


# Export data

#write.csv(neg_nw_count, paste(getwd(), 
#                '\\neg_nw_count.csv', 
#                sep=''))
```

## Net debtors by age group and race

``` r
# Create three age categories

scf_19$age_group <- cut(scf_19$AGE,
                            c(17, 34, 54, 96),
                            labels = c("Under 35", "35-54",
                                   "Over 55"))


# Find share of net debtors by age group and race

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
neg_nw$age_group <- cut(neg_nw$AGE,
                            c(17, 34, 54, 96),
                            labels = c("Under 35", "35-54",
                                   "Over 55"))

prop2 <- neg_nw %>%
  group_by(age_group, RACE) %>%
    summarise(prop_edu = wpct(EDN_INST > 0, WGT)[1],
            count = n())
```

    ## `summarise()` has grouped output by 'age_group'. You can override using the `.groups` argument.

``` r
# age distribution

wpct(scf_19$age_group, scf_19$WGT)
```

    ##  Under 35     35-54   Over 55 
    ## 0.2087219 0.3377451 0.4535330

``` r
wpct(neg_nw$age_group, neg_nw$WGT)
```

    ##  Under 35     35-54   Over 55 
    ## 0.4998895 0.3223889 0.1777215

``` r
# Export to CSV 

write.csv(prop_nd_race, paste(getwd(), 
                '\\prop_nd_race.csv', 
                sep=''))
```

## Debt and Asset Shares Overall

``` r
# Total debt share 

all_debt <- select(neg_nw, MRTHEL, RESDBT, CCBAL, EDN_INST, VEH_INST, OTHDBT, WGT)

w_all_debt <- apply(all_debt, 2, function(x,w) {x*w}, w = all_debt$WGT)

totals <- apply(w_all_debt, 2, sum)
totals[4]/sum(totals)
```

    ##  EDN_INST 
    ## 0.4418227

## Asset and Debt Mixes

``` r
# Asset mix

# Isolating assets

assets_negnw_list <- select(neg_nw, LIQ, RETQLIQ, OTHFIN, SECURITIES, 
                     VEHIC, HOUSES, BUS, OTHNFIN, NONRES, WGT)


# Creating new weighted percentage function to use within apply function 

new_wpct <- function(x, w) {
  wpct(x > 0, w)
}


# Creating dataframe of median asset values and % holding them 

assets_negnw <- data.frame(med = apply(assets_negnw_list, 
                                       2, 
                                       weighted.median, 
                                       w = assets_negnw_list$WGT))

assets_negnw$prop <- t(data.frame(apply(assets_negnw_list, 
                                        2, 
                                        new_wpct, 
                                        w = assets_negnw$WGT))[1,])                         
assets_negnw <- assets_negnw[-c(10), ]                       
         
                           
# Export to csv

write.csv(assets_negnw,  paste(getwd(), 
                '\\assets_negnw.csv', 
                sep=''))



# Debt mix 

# Isolating debts

debt_negnw_list <- select(neg_nw, MRTHEL, RESDBT, CCBAL, EDN_INST, VEH_INST, OTHDBT, WGT)


# Creating dataframe of median debt values and % holding them

debt_negnw <- data.frame(med = apply(debt_negnw_list,
                                     2,
                                     weighted.median,
                                     w = debt_negnw_list$WGT))

debt_negnw$prop <- t(data.frame(apply(debt_negnw_list, 
                                        2, 
                                        new_wpct, 
                                        w = debt_negnw_list$WGT))[1,]) 

debt_negnw <- debt_negnw[-c(7), ] 


# checking sample sizes

sum(debt_negnw_list$MRTHEL > 0)
```

    ## [1] 344

``` r
sum(debt_negnw_list$RESDBT > 0)
```

    ## [1] 32

``` r
sum(debt_negnw_list$CCBAL > 0)
```

    ## [1] 1613

``` r
sum(debt_negnw_list$EDN_INST > 0)
```

    ## [1] 1802

``` r
sum(debt_negnw_list$VEH_INST > 0)
```

    ## [1] 1105

``` r
sum(debt_negnw_list$OTHDBT > 0)
```

    ## [1] 872

``` r
# Exporting to csv

write.csv(debt_negnw,  paste(getwd(), 
                '\\debt_negnw.csv', 
                sep=''))
```

## Debt mix among those holding

``` r
# Calculating median debt value for select debt types among net debtors 
# holding them

debts_holding <- data.frame(RESDBT = weighted.median(filter(neg_nw,neg_nw$RESDBT > 0)$RESDBT, 
                filter(neg_nw,neg_nw$RESDBT > 0)$WGT),
                            MRTHEL = 
weighted.median(filter(neg_nw,neg_nw$MRTHEL > 0)$MRTHEL, 
                filter(neg_nw,neg_nw$MRTHEL > 0)$WGT),
                            CCBAL = 
weighted.median(filter(neg_nw,neg_nw$CCBAL > 0)$CCBAL, 
                filter(neg_nw,neg_nw$CCBAL > 0)$WGT),
                            EDN_INST = 
weighted.median(filter(neg_nw,neg_nw$EDN_INST > 0)$EDN_INST, 
                filter(neg_nw,neg_nw$EDN_INST > 0)$WGT),
                            VEH_INST = 
weighted.median(filter(neg_nw,neg_nw$VEH_INST > 0)$VEH_INST, 
                filter(neg_nw,neg_nw$VEH_INST > 0)$WGT),
                            OTHDBT = 
weighted.median(filter(neg_nw,neg_nw$OTHDBT > 0)$OTHDBT, 
                filter(neg_nw,neg_nw$OTHDBT > 0)$WGT))                              
write.csv(debts_holding,  paste(getwd(), 
                '\\debts_holding.csv', 
                sep=''))

nrow(filter(neg_nw, neg_nw$RESDBT > 0))
```

    ## [1] 32

``` r
nrow(filter(neg_nw, neg_nw$MRTHEL > 0))
```

    ## [1] 344

``` r
nrow(filter(neg_nw, neg_nw$CCBAL > 0))
```

    ## [1] 1613

``` r
nrow(filter(neg_nw, neg_nw$EDN_INST > 0))
```

    ## [1] 1802

``` r
nrow(filter(neg_nw, neg_nw$VEH_INST > 0))
```

    ## [1] 1105

``` r
nrow(filter(neg_nw, neg_nw$OTHDBT > 0))
```

    ## [1] 872

## Racial distribution of net debtors

``` r
# Data frame comparing racial breakdown of all HHs vs net debtors

race_scf_netdebtors <- data.frame(scf = wpct(scf_19$RACE, 
                                             scf_19$WGT),
                                  net_debtors = wpct(neg_nw$RACE,
                                                     neg_nw$WGT))


# Get sample sizes for each race in net debt 

count_nd <- neg_nw %>%
  group_by(RACE) %>%
  summarise(n = n())

# Exporting data 

write.csv(race_scf_netdebtors,  paste(getwd(), 
                '\\race_scf_netdebtors.csv', 
                sep=''))
```

## Measures of Debt Burden

## Income to Debt Ratio

``` r
# Debt to income ratio compared to overall population

weighted.median(neg_nw$DEBT2INC, neg_nw$WGT)
```

    ## [1] 1.16185

``` r
weighted.median(scf_19$DEBT2INC, scf_19$WGT)
```

    ## [1] 0.4679763

## Deliquent payments

``` r
# Late payment in the last year 

nrow(filter(scf_19, scf_19$LATE == 1))
```

    ## [1] 3173

``` r
nrow(filter(neg_nw, neg_nw$LATE == 1))
```

    ## [1] 911

``` r
wpct(scf_19$LATE, scf_19$WGT)
```

    ##         0         1 
    ## 0.8769632 0.1230368

``` r
wpct(neg_nw$LATE, neg_nw$WGT)
```

    ##         0         1 
    ## 0.6666086 0.3333914

``` r
# Payment past 60 days due 

nrow(filter(scf_19, scf_19$LATE60 == 1))
```

    ## [1] 1184

``` r
nrow(filter(neg_nw, neg_nw$LATE60 == 1))
```

    ## [1] 462

``` r
wpct(scf_19$LATE60, scf_19$WGT)
```

    ##          0          1 
    ## 0.95366669 0.04633331

``` r
wpct(neg_nw$LATE60, neg_nw$WGT)
```

    ##         0         1 
    ## 0.8394062 0.1605938

## Age group comparison

``` r
weighted.median(scf_19$AGE, scf_19$WGT)
```

    ## [1] 52

``` r
weighted.median(neg_nw$AGE, neg_nw$WGT)
```

    ## [1] 34.39521

``` r
wpct(neg_nw$age_group, neg_nw$WGT)
```

    ##  Under 35     35-54   Over 55 
    ## 0.4998895 0.3223889 0.1777215

## Family structure and gender

``` r
# Family structure



nrow(filter(scf_19, scf_19$FAMSTRUCT == 2))
```

    ## [1] 3770

``` r
nrow(filter(neg_nw, neg_nw$FAMSTRUCT == 2))
```

    ## [1] 776

``` r
wpct(scf_19$FAMSTRUCT, scf_19$WGT)
```

    ##         1         2         3         4         5 
    ## 0.1087220 0.1506436 0.1801701 0.2776858 0.2827786

``` r
wpct(neg_nw$FAMSTRUCT, neg_nw$WGT)
```

    ##         1         2         3         4         5 
    ## 0.1961853 0.2976189 0.1085989 0.2078087 0.1897882

``` r
# Gender

nrow(filter(scf_19, scf_19$HHSEX == 2))
```

    ## [1] 6465

``` r
nrow(filter(neg_nw, neg_nw$HHSEX == 2))
```

    ## [1] 1106

``` r
wpct(scf_19$HHSEX, scf_19$WGT)
```

    ##         1         2 
    ## 0.7304569 0.2695431

``` r
wpct(neg_nw$HHSEX, neg_nw$WGT)
```

    ##         1         2 
    ## 0.5610977 0.4389023

``` r
# Gender breakdown of single parents

nrow(filter(filter(scf_19, scf_19$FAMSTRUCT == 1),
       filter(scf_19, scf_19$FAMSTRUCT == 1)$HHSEX == 2))
```

    ## [1] 2150

``` r
nrow(filter(filter(neg_nw, neg_nw$FAMSTRUCT == 1),
       filter(neg_nw, neg_nw$FAMSTRUCT == 1)$HHSEX == 2))
```

    ## [1] 466

``` r
wpct(filter(scf_19, scf_19$FAMSTRUCT == 1)$HHSEX,
     filter(scf_19, scf_19$FAMSTRUCT == 1)$WGT)
```

    ##         1         2 
    ## 0.1943099 0.8056901

``` r
wpct(filter(neg_nw, neg_nw$FAMSTRUCT == 1)$HHSEX,
     filter(neg_nw, neg_nw$FAMSTRUCT == 1)$WGT)
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
wpct(filter(neg_nw, neg_nw$HHSEX == 2)$FAMSTRUCT,
     filter(neg_nw, neg_nw$HHSEX == 2)$WGT)
```

    ##          1          2          3          4          5 
    ## 0.40444361 0.38155795 0.18069860 0.01897267 0.01432717

## Housing status

``` r
wpct(scf_19$HOUSECL, scf_19$WGT)
```

    ##         1         2 
    ## 0.6489863 0.3510137

``` r
wpct(neg_nw$HOUSECL, neg_nw$WGT)
```

    ##         1         2 
    ## 0.1887738 0.8112262

## Education level

``` r
# Education levels for all HHs
wpct(scf_19$EDCL, scf_19$WGT)
```

    ##         1         2         3         4 
    ## 0.1072253 0.2448393 0.2846868 0.3632485

``` r
# Education levels for HHs with negative net worth
wpct(neg_nw$EDCL, neg_nw$WGT)
```

    ##          1          2          3          4 
    ## 0.07839683 0.16360847 0.37463233 0.38336238

``` r
# Combining HS and no HS
sum(wpct(scf_19$EDCL, scf_19$WGT)[1], wpct(scf_19$EDCL, scf_19$WGT)[2])
```

    ## [1] 0.3520647

``` r
sum(wpct(neg_nw$EDCL, neg_nw$WGT)[1], wpct(neg_nw$EDCL, neg_nw$WGT)[2])
```

    ## [1] 0.2420053

``` r
# Breakdown of some college by race

wpct(filter(neg_nw, neg_nw$EDCL == 3)$RACE,
     filter(neg_nw, neg_nw$EDCL == 3)$WGT)
```

    ## Black, non-Hispanic            Hispanic               Other White, non-Hispanic 
    ##          0.31456840          0.08416106          0.03890382          0.56236673

``` r
wpct(scf_19$OCCAT1, scf_19$WGT)
```

    ##          1          2          3          4 
    ## 0.57913929 0.10830557 0.27054535 0.04200979

``` r
wpct(neg_nw$OCCAT1, neg_nw$WGT)
```

    ##          1          2          3          4 
    ## 0.73478773 0.05477638 0.13097402 0.07946188

``` r
wpct(scf_19$OCCAT2, scf_19$WGT)
```

    ##         1         2         3         4 
    ## 0.2953042 0.2053901 0.1867506 0.3125551

``` r
wpct(neg_nw$OCCAT2, neg_nw$WGT)
```

    ##         1         2         3         4 
    ## 0.3354621 0.2684323 0.1856697 0.2104359

``` r
# Labor force participation 

wpct(scf_19$LF, scf_19$WGT)
```

    ##         0         1 
    ## 0.2877515 0.7122485

``` r
wpct(neg_nw$LF, neg_nw$WGT)
```

    ##         0         1 
    ## 0.1650558 0.8349442

``` r
nrow(filter(scf_19, scf_19$TURNDOWN == 1))
```

    ## [1] 2706

``` r
nrow(filter(neg_nw, neg_nw$TURNDOWN == 1))
```

    ## [1] 711

``` r
wpct(scf_19$TURNDOWN, scf_19$WGT)
```

    ##         0         1 
    ## 0.8929811 0.1070189

``` r
wpct(neg_nw$TURNDOWN, neg_nw$WGT)
```

    ##         0         1 
    ## 0.7326868 0.2673132

``` r
nrow(filter(scf_19, scf_19$BNKRUPLAST5 == 1))
```

    ## [1] 495

``` r
nrow(filter(neg_nw, neg_nw$BNKRUPLAST5 == 1))
```

    ## [1] 85

``` r
wpct(scf_19$BNKRUPLAST5, scf_19$WGT)
```

    ##          0          1 
    ## 0.98040561 0.01959439

``` r
wpct(neg_nw$BNKRUPLAST5, neg_nw$WGT)
```

    ##          0          1 
    ## 0.96782608 0.03217392

``` r
wpct(scf_19$FORECLLAST5, scf_19$WGT)
```

    ##          0          1 
    ## 0.98705142 0.01294858

``` r
wpct(neg_nw$FORECLLAST5, neg_nw$WGT)
```

    ##          0          1 
    ## 0.98603017 0.01396983

``` r
wpct(scf_19$NOCCBAL, scf_19$WGT)
```

    ##        0        1 
    ## 0.439593 0.560407

``` r
wpct(neg_nw$NOCCBAL, neg_nw$WGT)
```

    ##         0         1 
    ## 0.6160212 0.3839788

``` r
wpct(scf_19$EXPENSHILO, scf_19$WGT)
```

    ##          1          2          3 
    ## 0.23317724 0.04364993 0.72317283

``` r
wpct(neg_nw$EXPENSHILO, neg_nw$WGT)
```

    ##          1          2          3 
    ## 0.30999967 0.04902132 0.64097901

``` r
wpct(scf_19$SAVED, scf_19$WGT)
```

    ##         0         1 
    ## 0.4143882 0.5856118

``` r
wpct(neg_nw$SAVED, neg_nw$WGT)
```

    ##         0         1 
    ## 0.5887916 0.4112084

``` r
wpct(scf_19$WSAVED, scf_19$WGT)
```

    ##         1         2         3 
    ## 0.1305858 0.2838023 0.5856118

``` r
wpct(neg_nw$WSAVED, neg_nw$WGT)
```

    ##         1         2         3 
    ## 0.2558673 0.3329242 0.4112084

``` r
wpct(scf_19$HBORRALT, scf_19$WGT)
```

    ##          0          1 
    ## 0.98895161 0.01104839

``` r
wpct(neg_nw$HBORRALT, neg_nw$WGT)
```

    ##          0          1 
    ## 0.98658005 0.01341995

``` r
wpct(scf_19$NOFINRISK, scf_19$WGT)
```

    ##         0         1 
    ## 0.6076206 0.3923794

``` r
wpct(neg_nw$NOFINRISK, neg_nw$WGT)
```

    ##         0         1 
    ## 0.6220468 0.3779532

``` r
wpct(scf_19$HCUTFOOD, scf_19$WGT)
```

    ##          0          1 
    ## 0.96247782 0.03752218

``` r
wpct(neg_nw$HCUTFOOD, neg_nw$WGT)
```

    ##          0          1 
    ## 0.98026546 0.01973454

``` r
# Create age groups 

range(scf_19$AGE)
```

    ## [1] 18 95

``` r
scf_19$age_group <- cut(scf_19$AGE,
                            c(17, 35, 45, 55, 65, 75, 96),
                            labels = c("Less than 35", "35-44",
                                   "45-54", "55-64", 
                                   "65-74", "75 or older"))


scf_19$w_nw <- scf_19$NETWORTH*scf_19$WGT


neg_nw$w_nw <- neg_nw$NETWORTH*neg_nw$WGT

neg_nw$age_group <- cut(neg_nw$AGE,
                            c(17, 35, 45, 55, 65, 75, 96),
                            labels = c("Less than 35", "35-44",
                                   "45-54", "55-64", 
                                   "65-74", "75 or older"))

netdebt_age <- neg_nw %>%
  group_by(age_group) %>%
  summarise(prop = n()/2540,
            n = n())
```

``` r
wpct(filter(scf_19, scf_19$EDN_INST > 0)$NETWORTH < 0,
     filter(scf_19, scf_19$EDN_INST > 0)$WGT)
```

    ##      TRUE     FALSE 
    ## 0.3512502 0.6487498

``` r
with_edu <- filter(scf_19, scf_19$EDN_INST > 0)



edu_stats <- with_edu %>%
  group_by(age_group) %>%
  summarise(prop = sum(w_nw < 0)/n(),
            med = weighted.median(EDN_INST, WGT))
```

``` r
weighted.mean(neg_nw$KIDS, neg_nw$WGT)
```

    ## [1] 0.7251194

``` r
wpct(neg_nw$FAMSTRUCT, neg_nw$WGT)
```

    ##         1         2         3         4         5 
    ## 0.1961853 0.2976189 0.1085989 0.2078087 0.1897882
