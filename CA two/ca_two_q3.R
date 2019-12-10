
yes = c(male = 200, female = 250)
no = c(male = 150, female = 300)
csay = c(male = 50, female = 50)

o = data.frame(yes,no,csay) 
t = chisq.test(o)  
t_value = t$statistic 
t_value # test-value

alpha = 0.05
df = t$parameter
df
c_value = qchisq(1-alpha, df)
c_value   # critical-value

if(t_value > c_value){  
  decision='Reject H_0'
}else{  
  decision='Accept H_0'  
}  
decision
