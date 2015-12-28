#Data Mining the Inside Airbnb San Francisco May 2015 Dataset
##Predictive Modeling of Daily Rental Pricing in Airbnb's San Francisco Market Using Web User Interface Inputs

For this project, I chose to look at the "Inside Airbnb San Francisco" dataset, which provides data compiled from the 
Airbnb website for listings available for San Francisco, CA for the month of May 2015.

Inside Airbnb is an independent, non-commercial set of tools and data that allows users to explore how Airbnb is really  being used in cities around the world.

By analyzing publicly available information about a city's Airbnb's listings, Inside Airbnb provides filters and key metrics so data scientists can see how Airbnb is being used to compete with the residential housing market.

This dataset contains 7,029 listings (rows) that were posted on Airbnb's San Francisco page. The raw dataset contains 92 columns that may be treated as predictor variables. 

A few things to note: This data was web scraped by Murray Cox, the creater of the Inside Airbnb project, who I consulted with directly in the interpretation of some variables in the dataset. The project was not commercially funded, and resources used to collect and analyze the data were self-funded, so very special thank you to Murray Cox for building this dataset, among others, for public use. More information on the dataset can be found at: http://insideairbnb.com/

Secondly, the datset consists of listing information that was extracted from questions within the user interface of the Airbnb website for listings and hosts. Thus, the variables are limited to those fields which correspond directly to the Airbnb electronic form filled out by users posting listings. The *host_id* and *calculated_host_listings_count* variables identify unique hosts and how many listings those hosts have in the dataset.

Lastly, a host may list multiple rooms in the same apartment, or multiple apartments or homes they have available. This is to say that each listing in the dataset does not necessarily correspond with unique hosts, so this is a limitation of the dataset's interpretive power on a unique-user basis.
