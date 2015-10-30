# Microsim1
Microsim code, data sources, results

• alloc_flow6.R: main file, won't change much. Generates an initial SP, at the flow level. The resulting error is very low for most of the types (average 0.5-1% compared to the ct3.csv crosstab file). 

• alloc_flow7.R: 'balancer file'. Can be used to fit the SP solution a bit more (that is, going from the 99% accuracy compared to ct3.csv, to 100%). It may suffer changes so it's more to be run that analyzed in much detail.

• 7flowCandidates.R: sub for alloc_flow7.R. Might change if calling scripts changes.


Documentation to be added progressively from now (30-Oct-2015) on.
