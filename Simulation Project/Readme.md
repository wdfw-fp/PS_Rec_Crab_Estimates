Stephanie Thurner's files (NWIFC) - 11/1/24

Files:
1. Data.R reads in summer data, processes data as needed, saves rds data objects
2. SimulateData - Fits nbinom distributions, saves distributions
3. SimulateV2.R (Should rename) - defines function to simulate 1 year of catch
4. RunSimulationsV2.R - runs simulations, plots results. If you have all of my data folders, you can run this alone.
   
Folders that are needed but not uploaded: OriginalData, nbinomRDS, DataRDS


Stephanie Thurner (NWIFC) and Katelyn Bosley (WDFW) Plan - 10/15/24

Parts of population: Respondents, Late Respondents, Non-Respondents
Seasons: Summer, Winter 
Datasets: CRCs, % on time by year, Survey data, % late by year
Possible Simulated datasets:
1.	Null hypothesis: Total population from reported CRCs -->  No difference between on time, late, don’t respond
2.	Use reported CRCs to simulate respondent and late population, use survey data to simulate non-respondent population --> Assumes late population is more similar to group who reported since they did report, just late
3.	Use reported CRCs to simulate respondent population, use survey data to simulate late population --> Assumes late population is more similar to group who didn’t report since they did not submit before the deadline

First Steps (SUMMER ONLY):
1. Visualize catch/card for CRC data and survey data - by year, aggregated
2. Visualize distribution of % on-time and late cards
3. Fit probability distribution to CRC respondent data - aggregate, mail-in, online - by year or total across years
4. Fit probability distribution to Survey data from non-respondents - by year or total across years
5. Fit probability distribution to % on-time cards (total, mail in, online) - data available in most recent memo
6. Fit probability distribution to % late cards (use place holder until available)

Simulate Data Steps:
Just wanted to check in before my meeting with Katelyn. Can you confirm this is what you saw as the plan for the simulated data?

For a prespecified issued number of licenses:
1. Specify probability returned (either define it to do sensitivity testing, or draw from a uniform distribution of the range of % returned), calculate number returned
2. Specify probability mailed in (define or runif), calculate number mailed in and number online
3. Specify probability late (define it or runif), calculate number late
4. Simulate Returned CRCs (draw from negbinomial or resampling from the database, can do this for all CRCs at once or for mail/online separately)
5. Simulate NonResponse Catch by ID (draw from negative binomial or resampling from survey data)
6. Simulate Late Data -- either from response or survey depending on the hypothesis you want to test
7. Calculate Total Catch from 4 + 5 + 6
8. Compare Total Catch in 7 to what we would estimate the catch to be with proposed/original methods

