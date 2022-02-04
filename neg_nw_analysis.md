Analysis of Households with Negative Net Worth Using Data from the 2019
Survey of Consumer Finances
================
Shehryar Nabi, Senior Research Associate, Aspen Institute Financial
Security Program

## Breakdowns of age, race, income, and net worth

Before calculating variables of interest, I first create variables that
group data into age, race, income, and net worth categories.

``` r
# Create age groups 

scf_19$age_group <- cut(scf_19$AGE,
                            c(17, 34, 54, 96),
                            labels = c("Under 35", "35-54",
                                   "Over 55"))


# Rename race categories

scf_19$RACE[scf_19$RACE == 1] <- "White, non-Hispanic"
scf_19$RACE[scf_19$RACE == 2] <- "Black, non-Hispanic"
scf_19$RACE[scf_19$RACE == 3] <- "Hispanic"
scf_19$RACE[scf_19$RACE == 5] <- "Other"


# Create income quintiles 

inc_quintile_breaks_19 <- c(weighted_networth_quantiles(scf_19$INCOME, 
                                                  scf_19$WGT, 1/5))
inc_quintile_breaks_19[1] <- inc_quintile_breaks_19[1] - 1
scf_19$INCOME_quintiles <- cut(scf_19$INCOME,
                            c(inc_quintile_breaks_19),
                            labels = c("0-19.9", "20-39.9",
                                   "40-59.9", "60-79.9", 
                                   "80-100"))


# SCF net worth quintiles 

scf_19$nw_scf_breaks <- cut(scf_19$NETWORTH,
    c(-955502, 12410, 121760, 404100, 1218737, 1967199000),
    labels = c("Less than 25", "25-49.9",
                "50-74.9", "75-89.9", 
                "90-100"), na.rm = TRUE)
```

## Creating “other” debt variable

For simplicity, I combine “other” SCF debt types into one “other” debt
variable.

``` r
# Debt

otherdebt <- scf_19 %>%
  select(OTHLOC, OTH_INST, ODEBT)
scf_19$OTHDBT <- rowSums(otherdebt)
```

## Share of households with negative net worth in 1989, 2007, and 2019

I calculate the proportion of households in the SCF with negative net
worth.

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
# 2007

scf_07 <- mutate(scf_07, 
                 netdebt_status = ifelse(scf_07$NETWORTH < 0, 1, 0))

wpct(scf_07$netdebt_status, scf_07$WGT)
```

    ##         0         1 
    ## 0.9223927 0.0776073

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
# Creating a new data frame of households with negative net worth

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
# Getting median debt, asset and net worth values for all HHs with net debt

neg_nw_wealth <- neg_nw %>%
  group_by(INCOME_quintiles) %>%
  summarise(median_debt = weighted.median(DEBT, WGT)/1000,
            median_assets = weighted.median(ASSET, WGT)/1000,
            median_nw = weighted.median(NETWORTH, WGT)/1000, 
            n = n())


# Getting share of net debtors by income quintile

neg_nw_wealth$prop <- wpct(neg_nw$INCOME_quintiles, neg_nw$WGT)


# Exporting data

write.csv(neg_nw_wealth, paste(getwd(), '\\csv-exports', 
                '\\neg_nw_wealth.csv', 
                sep=''))
```

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
wpct(neg_nw$age_group, neg_nw$WGT)
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

## Debt portfolios

I calculate the median value per debt type among households holding it
for both households with negative net worth and overall.

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
write.csv(debts_holding,  paste(getwd(), '\\csv-exports', 
                '\\debts_holding.csv', 
                sep=''))
```

## Racial distribution of net debtors

I calculate the racial distribution of households with negative net
worth and households overall.

``` r
# Data frame comparing racial breakdown of all HHs vs net debtors

race_scf_netdebtors <- data.frame(scf = wpct(scf_19$RACE, 
                                             scf_19$WGT),
                                  net_debtors = wpct(neg_nw$RACE,
                                                     neg_nw$WGT))


# Exporting data 

write.csv(race_scf_netdebtors,  paste(getwd(), '\\csv-exports', 
                '\\race_scf_netdebtors.csv', 
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
wpct(neg_nw$EDCL, neg_nw$WGT)
```

    ##          1          2          3          4 
    ## 0.07839683 0.16360847 0.37463233 0.38336238

## Family structure and gender

``` r
# Family structure

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

## Measures of Debt Burden

The following sections display code used to calculate two indicators of
debt burden: income-to-debt ratios and delinquent payments,

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

wpct(scf_19$LATE60, scf_19$WGT)
```

    ##          0          1 
    ## 0.95366669 0.04633331

``` r
wpct(neg_nw$LATE60, neg_nw$WGT)
```

    ##         0         1 
    ## 0.8394062 0.1605938
