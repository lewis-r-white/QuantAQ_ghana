# QuantAQ

This repository contains an analysis of QuantAQ air quality monitor sensors. I access the monitor data through QuantAQs API and R wrapper. The specific monitors that I access in my R markdowns are part of a fleet of monitors in Ghana, Uganda, Kenya, and Ethiopia. These monitors are not available to the public, but creating a QuantAQ account allows you to access a suite of public monitors. 

The missing_data_report R Markdown contains code that allows you to obtain data by specifing a list of monitors, a start date, and an end date. With this data, I create a table that specifies the number of hours of missing data for each date/monitor pairing. 
