tax = 2200/3076
3076/(32*2)*(40*2)*2*tax-(2100+1300) 

### 
100000/12*.8/2


tax = 1-(3238.25/11868.12)

###Prorated
Prorated = 6666.67
Prorated*tax*.5

###Gross
Gross = 3076.92
Net = Gross*tax
Net
###Contract
Contract = 8333
actual_check = Contract*tax/2

paychex_tax = .24
actual_check = 2239.88

effective_rate = (actual_check*2)/Prorated
print(paste0('Effective tax / benefits rate: ',
        round(1-effective_rate,3) ))

period_total = 3846.15
#Fed tax %
fed = 568 / period_total
fed
#State tax %
state = 247.74 / period_total
state
#social security / medicare / disability
ss_other = (238.46 + 55.77 + 42.31) / period_total
ss_other
fed + state + ss_other

### Catch Up 
total_catch_up = 8680.91
fed_catch_up = 1858.18 / total_catch_up
fed_catch_up
#State tax %
state_catch_up = 742.33 / total_catch_up
state_catch_up
#social security / medicare / disability
ss_other_catch_up = (538.22 + 125.88 + 95.49) / total_catch_up
ss_other_catch_up
fed_catch_up + state_catch_up + ss_other_catch_up

### differences
reduce_fed = fed_catch_up - fed 
reduce_fed
reduce_state = state_catch_up - state
reduce_state
(ss_other_catch_up - ss_other) %>% round(3)


### adjustments
adj_fed = round((fed_catch_up - reduce_fed) * total_catch_up,2) 
adj_state = round((state_catch_up - reduce_state) * total_catch_up,2)

print(paste0('Reduce federal tax to: ',
      adj_fed ))
print(paste0('Reduce federal tax to: ',
              adj_state ))

# self scratch
1152.28 / period_total 

3360.10 / 8680.91 


8680.91 - 8680.91*.7

# file:///C:/Users/elija/Downloads/Elijah_Hoffman_Arva_Intelligence_Employement_Offer_Letter_11.1.21_Signed_11_4_2021.pdf





### Shyanne ----

(11*2*(4*4)) * .5 + # Litter 176
  (9.04*c((6*.5) + (5*.8) + (4 *.5) + (2*.5) + (2*.5))*5) + # (9.04*30*4)+ # Cigs 497.2
  ((19*4*2)+(29*4)) *.5 + # (smol bag o food )(large bag of food) 134
  60*2 +  # doggo 120
500 - # David loan 500
 (320 - 180 -40) ### PArtial rent - matress - couch

176+497+134+120+500 = 1427 - 100 
  
total = (12.5*40*4*4)

1 - (total -(1427-500))/total