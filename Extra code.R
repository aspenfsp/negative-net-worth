# Total debt share 

all_debt <- select(neg_nw, MRTHEL, RESDBT, CCBAL, EDN_INST, VEH_INST, OTHDBT, WGT)

w_all_debt <- apply(all_debt, 2, function(x,w) {x*w}, w = all_debt$WGT)

totals <- apply(w_all_debt, 2, sum)
totals[4]/sum(totals)




# Creating new weighted percentage function to use within apply function 

new_wpct <- function(x, w) {
  wpct(x > 0, w)
}


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


# Exporting to csv

write.csv(debt_negnw,  paste(getwd(), '\\csv-exports',
                             '\\debt_negnw.csv',
                             sep=''))


# checking sample sizes

sum(debt_negnw_list$MRTHEL > 0)
sum(debt_negnw_list$RESDBT > 0)
sum(debt_negnw_list$CCBAL > 0)
sum(debt_negnw_list$EDN_INST > 0)
sum(debt_negnw_list$VEH_INST > 0)
sum(debt_negnw_list$OTHDBT > 0)



nrow(filter(neg_nw, neg_nw$RESDBT > 0))
nrow(filter(neg_nw, neg_nw$MRTHEL > 0))
nrow(filter(neg_nw, neg_nw$CCBAL > 0))
nrow(filter(neg_nw, neg_nw$EDN_INST > 0))
nrow(filter(neg_nw, neg_nw$VEH_INST > 0))
nrow(filter(neg_nw, neg_nw$OTHDBT > 0))


# Get sample sizes for each race in net debt 

count_nd <- neg_nw %>%
  group_by(RACE) %>%
  summarise(n = n())


```{r housing status}


wpct(scf_19$HOUSECL, scf_19$WGT)
wpct(neg_nw$HOUSECL, neg_nw$WGT)

```


```{r work and occupation status}

wpct(scf_19$OCCAT1, scf_19$WGT)

wpct(neg_nw$OCCAT1, neg_nw$WGT)

wpct(scf_19$OCCAT2, scf_19$WGT)

wpct(neg_nw$OCCAT2, neg_nw$WGT)


# Labor force participation 

wpct(scf_19$LF, scf_19$WGT)

wpct(neg_nw$LF, neg_nw$WGT)

```





```{r burden of debt}

nrow(filter(scf_19, scf_19$TURNDOWN == 1))
nrow(filter(neg_nw, neg_nw$TURNDOWN == 1))

wpct(scf_19$TURNDOWN, scf_19$WGT)

wpct(neg_nw$TURNDOWN, neg_nw$WGT)


nrow(filter(scf_19, scf_19$BNKRUPLAST5 == 1))
nrow(filter(neg_nw, neg_nw$BNKRUPLAST5 == 1))

wpct(scf_19$BNKRUPLAST5, scf_19$WGT)

wpct(neg_nw$BNKRUPLAST5, neg_nw$WGT)


wpct(scf_19$FORECLLAST5, scf_19$WGT)

wpct(neg_nw$FORECLLAST5, neg_nw$WGT)


wpct(scf_19$NOCCBAL, scf_19$WGT)

wpct(neg_nw$NOCCBAL, neg_nw$WGT)


wpct(scf_19$EXPENSHILO, scf_19$WGT)

wpct(neg_nw$EXPENSHILO, neg_nw$WGT)


wpct(scf_19$SAVED, scf_19$WGT)

wpct(neg_nw$SAVED, neg_nw$WGT)


wpct(scf_19$WSAVED, scf_19$WGT)

wpct(neg_nw$WSAVED, neg_nw$WGT)


wpct(scf_19$HBORRALT, scf_19$WGT)

wpct(neg_nw$HBORRALT, neg_nw$WGT)

```



```{r other indicators of hardship}

wpct(scf_19$NOFINRISK, scf_19$WGT)

wpct(neg_nw$NOFINRISK, neg_nw$WGT)


wpct(scf_19$HCUTFOOD, scf_19$WGT)

wpct(neg_nw$HCUTFOOD, neg_nw$WGT)


```






```{r young people net debtors?}

# Create age groups 

range(scf_19$AGE)

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


```{r student loanees net debtors}

wpct(filter(scf_19, scf_19$EDN_INST > 0)$NETWORTH < 0,
     filter(scf_19, scf_19$EDN_INST > 0)$WGT)


with_edu <- filter(scf_19, scf_19$EDN_INST > 0)



edu_stats <- with_edu %>%
  group_by(age_group) %>%
  summarise(prop = sum(w_nw < 0)/n(),
            med = weighted.median(EDN_INST, WGT))



```