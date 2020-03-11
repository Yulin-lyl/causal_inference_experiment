# causal_inference_experiment

## Business Context
Since the introduction of Airbnb, many landlords have switched their long-term property rentals to more profitable short-term rentals. This means there are fewer long-term rentals to go around. Because of this change in housing availability, I am wondering if growth in short-term rentals, such as Airbnb, has a causal relationship with long-term housing rental prices.

## Datasets & Measures
The data used to address this question was obtained from ​Zillow ​and ​Airbnb​. I used city-level data from Zillow that provided rental information from September 2010 to January 2020 for 9,239 US cities. The Airbnb dataset provided listing information documenting host and property information in 189 US cities.

Within the cleaned dataset, these are 5 key features:
1. Region ID​ - Combination of City & State. Because multiple states can have the same city name.
2. Treated Date​ - Minimum value of “Host Since” column for a city.
3. Year/Month​ - The Year & Month corresponding to rental value.
4. Treated​ - Indicator depicting whether Airbnb has ever entered that city or not.
5. Rental Value​ - Its ​Zillow Rent Index​ (ZRI) which is ​a smoothed measure of the typically
estimated market-rate rent across a given region and housing type. ZRI, which is a dollar-denominated alternative to repeat-rent indices, is the mean of rent estimates that fall into the 40th to 60th percentile range for all homes and apartments in a given region, including those not currently listed for rent.

## Methods
1. Matching
2. Difference-in-Difference

## Results
The entry of Airbnb decreased the rental index of Temple City by 7% compared to that of Tumwater. This is a significant decrease in terms of rental prices. 
