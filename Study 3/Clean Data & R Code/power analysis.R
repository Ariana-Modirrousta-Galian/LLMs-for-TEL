library(pwr) 
pwr.t.test(d = .50, sig.level = 0.05, power = .80, 
           type = "two.sample", 
           alternative = "greater") 
