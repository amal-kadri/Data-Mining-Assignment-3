## Podcast

1.  If I want to understand how more cops in the streets affect crime,
    simply getting data from a few different cities and running the
    regression of “Crime” on “Police” is insufficient for causal
    identification. This is because there are likely unobserved
    confounding factors about cities that are responsible for the number
    of police and for the amount of crime. For example, if high poverty
    rates causes crime and leads to high amounts of police on the
    streets, then running the simple regression of crime on police would
    identify a positive relationship between crime and police. In this
    example, police do not cause crime, but that would be the
    interpretation if you took the regression result as causal.

2.  UPenn researchers use terrorism alert levels as an instrument for
    the amount of police on the street in order to isolate the effect of
    police on crime. Terrorism alert levels are a good instrument since
    they are not directly linked to crime (exogenous), but they do cause
    cities to add police to shifts (relevant). Since terror alert level
    is exogenous and relevant, using it as an instrument for police
    numbers effectively identifies the causal effect of police presence
    on crime. Tabel 2 shows that when terror level is orange (high),
    inducing police presence on the street exogenously elevated, the
    expected number of crimes is reduced by about 7. This result is
    significant at the 5% level. The second column includes a control
    for metro ridership as a proxy for the number of people on the
    street. With the ridership control, expected number of crimes
    reduces by about 6 during high terror alerts.

3.  Controlling for metro ridership makes the researchers case that the
    terror alert level is uncorrelated with crime stronger. Metro
    ridership is a proxy for foot traffic around the city. If foot
    traffic decreases when there is a high terror alert level, then
    crime might also go down because there are less potential victims in
    the streets. The opposite might also be true, that is, there may be
    a high terror alert because of a special event (a parade, perhaps),
    which would also mean more foot traffic and thus more chance for
    crime. Regardless of the sign of the effect, researchers would want
    to control for it, so metro ridership is added to the model.

4.  For this table, a more granular look is given to the causal
    relationship between increased police presence and crime rate using
    district 1 as a feature. The coefficient reads that the effect of an
    additional police officer in district 1 is associated with a 2.26
    decrease in expected number of crimes and is statistically
    significant in District 1 at the 95% level, whereas other districts
    did not exhibit a statistically significant decrease at this level.

## Dengue Trees

For this question, we chose to use number of dengue cases (rather than
the log) because it seemed to predict similarly accurately and the
plots/RMSE’s the model generated were far more interperateable and
informative than the log equivalents.

<table>
<caption>Dengue Models Out-of-Sample Performance Comparison</caption>
<thead>
<tr class="header">
<th style="text-align: left;">model</th>
<th style="text-align: right;">rmse</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">tree_all</td>
<td style="text-align: right;">32.20007</td>
</tr>
<tr class="even">
<td style="text-align: left;">tree_engineer</td>
<td style="text-align: right;">31.13952</td>
</tr>
<tr class="odd">
<td style="text-align: left;">forest_all</td>
<td style="text-align: right;">27.19579</td>
</tr>
<tr class="even">
<td style="text-align: left;">forest_engineer</td>
<td style="text-align: right;">27.81899</td>
</tr>
<tr class="odd">
<td style="text-align: left;">boost_all</td>
<td style="text-align: right;">28.51664</td>
</tr>
</tbody>
</table>

Dengue Models Out-of-Sample Performance Comparison

Across model specifications and tree types, random forest regressions
was a consistent winner when it came to performance. Displayed here are
RMSE’s for 5 regression models: A tree model with all covariates and one
with some amount of feature engineering, the same for random forest, and
the best boosted tree model we could make. The RMSE for the random
forest with all covariates was the best performing across many train
test splits. Even with some manual engineering, and cross validated
tuning, we could not get the gradient boosted tree model to outperform
the the random forest more than once or twice. This is a testament to
why random forests are the benchmark standard for supervised learning
techniques, especially since, of all the methods we tried, the random
forest was the easiest to implement.

![](assignment3_files/figure-markdown_strict/dengue_trees%20imp%20plots-1.png)![](assignment3_files/figure-markdown_strict/dengue_trees%20imp%20plots-2.png)

Along with `specific_humidity` and `precipitation_amt`, we chose to
include PD plots for `season` and `min_air_temp_k` variables, as they
both back up the same underlying temperature relationship. These choices
were partially informed by the *variable importance plot* we made from
our forest model, choosing variables that seemed to have the strongest
effect on dengue cases. Dengue cases seem to rapidly increase at around
18 grams water per kg air, likely because this is the humidity level
that allows mosquitoes to thrive. Interestingly though, dengue cases
seem to decrease with precipitation, likely because the mosquitoes that
spread it aren’t out during the heavy rains. Though we did postulate
that potentially a lagged precipitation variable of the amount of rain
1-2 weeks prior might actually be correlated with higher dengue cases,
as mosquitoes congregate around standing water that might accumulate
after rains. It’s difficult to make claims about the reasons for the
right tail of the plot, as the upturned section is drawn from less than
10% of the data, and it might potentially be anomalous. Both the
`season` and `min_air_temp_k` tell the same story that dengue cases tend
to increase with temperature, peaking in the summer months. There is a
sharp increase in dengue cases at around 297 degrees K (~24 C, ~70 F).
Texans know that without some extended cold weather or a nice freeze,
mosquitoes won’t be killed off in the winter and they become unbearable
in later months.

![](assignment3_files/figure-markdown_strict/dengue_trees%20pdplots-1.png)![](assignment3_files/figure-markdown_strict/dengue_trees%20pdplots-2.png)![](assignment3_files/figure-markdown_strict/dengue_trees%20pdplots-3.png)![](assignment3_files/figure-markdown_strict/dengue_trees%20pdplots-4.png)

## Green Certification

For this model, we elected to use the green\_rating field as opposed to
just LEED of Energystar to get a big picture look at green
certifications. To begin, we reviewed all the variables to exclude for
the models. Property and cluster, were omitted due to lack of
interrelated relationships with one another. LEED and Energystar were
omitted since they both made up the feature of interest green\_rating.
Rent and leasing\_rate were omitted because the outcome variable was a
linear combination of the two, and would thus be perfectly collinear.

The first model we tried was a lasso regression. Each time we tried to
trained the model, the coefficients selected were always interactions,
which is a tell-tale sign that a tree model would be a good candidate
for modeling this data.

    ## [1] "size:stories"           "size:cd_total_07"       "size:total_dd_07"      
    ## [4] "size:City_Market_Rent"  "hd_total07:total_dd_07"

Next we tried a random forest with the same feature specifications,
which cut the RMSE almost in half from around 1250 to around 650. We
compared it to a boosted tree model, but were only able to yield a RMSE
of 850, making the random forest modeling method the winner.

<table>
<caption>Green Certification: Out-of-Sample Model Performance</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Model</th>
<th style="text-align: left;">RMSE</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Lasso</td>
<td style="text-align: left;">1224.06722277022</td>
</tr>
<tr class="even">
<td style="text-align: left;">Boosted</td>
<td style="text-align: left;">924.984065296849</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Random Forest</td>
<td style="text-align: left;">738.432380632062</td>
</tr>
</tbody>
</table>

Green Certification: Out-of-Sample Model Performance

`size` and `stories` were responsible for the greatest improvements in
the model when the values were not permuted. Since rent per square foot
is dependent on the size of a building and stories, the explanatory
power of these variables tracks with the outcome being measured. Both
size and stories partial dependence graph illustrate this similar
relationship, plateauing around 2e+06 sqft and 50 stories respectively.

![](assignment3_files/figure-markdown_strict/green_certification%20pd-1.png)![](assignment3_files/figure-markdown_strict/green_certification%20pd-2.png)

`age` partial dependence plot sharply rises then gradually falls,
representing the loss in values which occurs as a building ages. There
is another spike then plateaus when the representative building turns
100 which represents the expected value that is placed on living in a
historical building. This explanatory power corroborated with age’s high
rank on both the %IncMSE and node purity.

![](assignment3_files/figure-markdown_strict/green_certification%20pd%202-1.png)

`City_Market_Rent` held the highest node purity, which is to be expected
since it is an aggregation of rent per square foot in the local area.
Buildings in the same area are likely to be similar and thus have
similar rent and leasing rates. It’s partial dependence plot is a
gradual slop, which coincides with this reasoning.

![](assignment3_files/figure-markdown_strict/green_certification%20pd%203-1.png)

`green_rating`, being a binary indicator variable, is associated with a
linear shift in expected building value of about 30, which is quite
unimpressive as a value generating feature. Unsurprisingly, it ranks
very low on both the %IncMSE and node purity.

![](assignment3_files/figure-markdown_strict/green_certification%20pd%204-1.png)

## California Houses

To predict census tract median house prices, we tried the two sharpest
knives in our roll; random forests and gradient boosted trees. The below
table shows that our random forest model outperforms the boosted trees
model.

<table>
<caption>Comparing Model Out-of-Sample Performance</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Models</th>
<th style="text-align: left;">RMSE</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Best Forest</td>
<td style="text-align: left;">50542.481</td>
</tr>
<tr class="even">
<td style="text-align: left;">Best Boost</td>
<td style="text-align: left;">52663.419</td>
</tr>
</tbody>
</table>

Comparing Model Out-of-Sample Performance

![](assignment3_files/figure-markdown_strict/california_houses%20importance-1.png)

Some key analysis can be reaped from the following partial dependence
plots. First, as `longitude` becomes less negative (moving eastward from
the coast), predicted housing prices drop.

The `latitude` partial dependence plot also reveals incite about housing
prices in California. In general, Southern latitudes are predicted to be
more expensive that northern ones. Looking at the map, what we would
expect to see two bumps in this general trend for the cluster of houses
near Los Angeles and San Francisco. This PD plot has the LA cluster as
higher than the SF plot. This is likely due to the fact that there are
more cheap houses east of SF but at the same latitude. By contrast, east
of LA there are not as many cheap houses.

Also, the `latitude` partial dependence plot also reveals incite about
housing prices in California. In general, Southern latitudes are
predicted to be more expensive that northern ones. Looking at the map,
what we would expect to see two bumps in this general trend for the
cluster of houses near Los Angeles and San Francisco. This PD plot has
the LA cluster as higher than the SF plot. This is likely due to the
fact that there are more cheap houses east of San Francisco along the
same latitude. By contrast, east of Los Angeles there are not as many
cheap houses.

The final PD plot shows how predictions change with `medianIncome`.
Tracts with higher median income have higher house prices, which is what
we would expect.

![](assignment3_files/figure-markdown_strict/california_houses%20pds-1.png)![](assignment3_files/figure-markdown_strict/california_houses%20pds-2.png)![](assignment3_files/figure-markdown_strict/california_houses%20pds-3.png)

The first map below shows clear clusters around San Francisco and Los
Angeles in the raw house price data. In general, the forest model does a
good job of capturing this trend. You can see this because the second
map looks very similar to the first one. Also, the third map shows
relatively low model error.

![](assignment3_files/figure-markdown_strict/california_houses%20maps-1.png)![](assignment3_files/figure-markdown_strict/california_houses%20maps-2.png)![](assignment3_files/figure-markdown_strict/california_houses%20maps-3.png)
