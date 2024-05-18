
Intro slide 
us people always been interested in what will happen in the future and try to predict it
from forecast what always been around us: temperature, preciptation
to forecast bigger thing like economic, gdp
or in middle-sized where forecast the expense, profit

Organisations and companies policies and plans for the future heavily depends on forecast

DTF doesnt left out from that

predicting and forecasting is used by DTF to adjust policy, implement subsidizes or restriction

Ltd is one of the aspect they interested in forecasting

Explain what is ltd, why is important, and how it help DTF 

WHat is LTD?
  - If you buy or acquire a property, you pay land transfer duty, commonly known as stamp duty.
- The amount of duty depends on the value of your property, how you use it, if you are a foreign purchaser, and if you are eligible for any exemptions or concessions.

How LTD affect property transactions?
- With land transfer duty fees accounting for 27 per cent of Victoria’s own tax revenue, 
it also leaves the State’s finances precariously dependent on income subject to property market fluctuations. (https://www.news.com.au/national/victoria/news/victoria-moves-to-abolish-stamp-duty-in-huge-boost-for-aspiring-homeowners/news-story/969c53a099bd7f477852e53b3e0cb555)

-Our interpretation of the empirical evidence, based on Australian and international
studies, is that a 100 basis point decrease in the rate of transfer duty would increase
property transactions by about 10 per cent (https://www.treasury.nsw.gov.au/sites/default/files/2021-06/the_economic_costs_of_transfer_duty_a_literature_review.pdf)

- Like all taxes transfer duties will influence behaviour and distort the allocation
of resources. However, unlike taxes that specifically target behaviour, such as tobacco
taxes, the distortionary impact of transfer duties on resource allocation is negative for the
economy.


Yes, the Victorian Government has implemented several measures to support home buyers in coping with high stamp duty costs:
  
1. **Stamp Duty Exemptions for First Home Buyers**: If you are a first home buyer, the Victorian Government has abolished stamp duty for homes valued at $600,000 or less. For homes valued between $600,000 and $750,000, there is a tapered stamp duty, which starts at zero and increases slightly as the price approaches $750,000 .

2. **Victorian Homebuyer Fund**: This is a shared equity scheme where the government makes a financial contribution towards the purchase of a home in exchange for an equity share. This can significantly reduce the upfront costs for eligible participants .

3. **Stamp Duty Waivers and Reductions**: During specific periods, such as the COVID-19 pandemic, the Victorian Government introduced a stamp duty waiver of up to 50% on the purchase of residential properties .

These measures are designed to make housing more affordable and accessible, especially for first-time home buyers in Victoria.


* 
The recommendations are:
  
- That the Department of Treasury and Finance model and publish the findings of ‘switch on sale’, ‘credit’ and ‘gradual transition’ proposals. 
The Victorian Government supports this recommendation in part and has noted that it receives regular modellings and advice from 
the Department of Treasury and Finance on tax and policy setting, inclusive of stamp duty.

- That the Department of Treasury and Finance should regularly review stamp duty rates to adjust for bracket creep.
The Victorian Government supports this recommendation in part and has noted that it receives regular modellings and advice from 
the Department of Treasury and Finance on tax and policy setting, inclusive of stamp duty.

- That the Victorian Government:
    * Should consider additional measures to increase housing supply, including strengthening housing targets.
    * Advocate for a national approach to stamp duty reform, recognising its potential to 
address housing affordability and accessibility nationwide.
    * As an interim measure until a national commitment is made, urgently explore state-based reform options, 
including conducting an investigation into the feasibility of abolishing stamp duty and implementing a broad-based land tax as an alternative.

---
From their analyses and my analysis, we all agree that there is a cointegration pattern 
between variables, similar long-term trend --> Use of VECM model.

Their approach and model (mention correlation between ltd and sales, hvi)

However by only forecasting the top level which is total independently, we have missed out information
from other disaggregation level. And if we can find a way to include these missing characteristics, 
our forecast is expected to perform much better. That's' why we need to take a further advanced approach?

Hierarchical structure ---> 
  
  
# For temporal hierarchy
In the temporal dimension, forecasts supporting decisions for different planning horizons may also be 
generated using approaches that utilise different information sets. For example, long-term annual 
forecasts supporting strategic decisions typically involve high level unstructured 
information from multiple sources and judgement (Ord, Fildes, & Kourentzes, 2017), in this
context from tourism experts. An example is the Tourism Forecasting Reference Panel 
comprising experts from industry and government, that was established by 
Tourism Research Australia. On the other hand, short-term monthly forecasts, 
supporting operational decisions, may be generated by only considering past tourism flows
  
Forecasting approach:
* Forecast for one level: 
    * bottom up: Forecast for comm, ind, other, res and add up. But the lower or bottom level data can be noisy
    * top down: As i mentioned before, with this approach, you will miss important features of lower levels

Mention incoherence problem here, then what we can do for this is:
  
    * approach 3: Forecast ALL series and reconcile this base forecast in such a way that the forecast add up coherently

Minimizing the variance between reconciled forecast and true values OR between base and reconciled forecast. 

Methods for this:
  * MinTrace: Find the matrix G which minimise the trace fo the variance. The idea is how to find 
G that we get the least variances between the actual values and forecast value
Now explain about summing matrix, mint shrink, etc. (citation at the bottom of the slide)

Apart from cross-sectional hierarchical structure, we also produce forecast for temporal hierarchical strucutre. 
 Combining this 2 strucutres will allow for cross-temporal forecast reconciliation, which is
 expected to produce higher accuracy forecast. 

* Note that: Forecast could also help assessing the effect of past policies implication by comparing forecast value and actual value after policies imposed.

 
## Cross-temporal forecasting
 
This specification assumes that each of the bottom-level base forecasts has errors 
with equal variance kh and these are uncorrelated between nodes. Therefore, higher level
error variances are the sum of the error variances of the lower level series that 
belong to that part of the hierarchy. Hence, each element of the diagonal matrix contains 
the number of forecast error variances contributing to each node. Fig. 4b provides the
resulting matrix for the simple hierarchical structure of Fig. 3, where, for instance, 
the 4 at the top level signifies that four bottom level series are used to construct it. 
This estimator only depends on the structure of the aggregations, and not on the actual data. It is
therefore referred to as structural scaling and we denote this as Struc in the results that follow


* More on why using structural scaling variance for temporal reconciliation:
(Kourentzes & Athanasopoulos, 2019)
In contrast to the cross-sectional case, since the forecasts for each level are for the same series, 
assuming homogeneous forecast errors within each level is reasonable. 
On the other hand, following the arguments by Athanasopoulos et al. (2017), since the
covariances in Wh would be between series of different sampling frequencies due to 
the temporal aggregation, we do not implement the MinT shrinkage estimator. 

Model used is the same but also use arima for comparison

then show RMSE for each model (base, rec)

How much rec forecast improve (also compared to forecast res used DTF methodology)

. The AvgRelMSE has very intuitive interpretation, where if it is smaller than 1, then the
evaluated forecast is better than the benchmark by (1 −AvgRelMSE)100%.

However, it doesnt matter how much we try, it is difficult get close to 100% accuracy

--> Prediction interval.

Thank you and honourable mention

interested? here are some research paper, recommend go thru fpp book series first



Limitations:
  When forecasting all series, all levels and all temporal frequencies, even with cross-validation
It may work for few thousands of times series. But if the hierarchy expands and also number of time
series piled up, there will be a problem (time consuming, memory inefficient)






library(visNetwork)
library(tidygraph)
library(dplyr)
library(tibble)

# Define main graph nodes and edges
nodes_main <- data.frame(id = 1:3, label = c("Total", "Non-Residential", "Residential"))
edges_main <- data.frame(from = c(1, 1), to = c(2, 3))

# Define subgraph
nodes_sub <- data.frame(id = 4:6, label = c("Q1", "Q2", "Q3"), group = "Q")
edges_sub <- data.frame(from = c(4, 4), to = c(5, 6))

# Create visNetwork object
net <- visNetwork(nodes = rbind(nodes_main, nodes_sub), edges = rbind(edges_main, edges_sub), width = "100%", height = "400px") %>%
  visNodes(shape = "box") %>%
  visEdges(arrows = 'to') %>%
  visGroups(groupname = "Q", shape = "dot", color = list(background = "lightblue")) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 123)

print(net)



