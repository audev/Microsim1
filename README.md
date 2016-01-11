# Microsim1
Microsim code, data sources, results

• [alloc_flow6.R]: main file, won't change much. Generates an initial SP, at the flow level. The resulting error is very low for most of the types (average 0.5-1% compared to the ct3.csv crosstab file). 

• [balancing_flows4.R]: 'balancer file'. Can be used to fit the SP solution even more (that is, going from the 99% accuracy compared to ct3.csv, to 100%). 

• [getCandidates1.R]: sub for balancing_flows4.R. Can be altered to enlarge the base of candidates (right now stop at semantic difference=2, for 3 variables).



