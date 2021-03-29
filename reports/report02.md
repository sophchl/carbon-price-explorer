- ## Theoretical results
    - [[MSCI How Have Stocks Responded to Changes in Climate Policy?]]  (Brief summary)
        - examined how **equity markets reacted to climate-related political or regulatory events**
        - selected **nine events** and separated them in **progress and setbacks**
        - clustered** company characteristics **in carbon efficient operations, revenues from green sources, sensitivity to carbon price
        - investigated **two types of risk drivers**: event risk (immediatelly priced in) and erosion risks (play out over long time periods)
        - results
            - **event risks**: equity markets did **not consistently** react immediately, often moven in oposite direction of what was expected
            - **erosion risks**: inclusion of emission risk factor in standard risk factor model shows the **outperformance of emission factor**, particularly in recent years
    - [[Climate sin stocks: stock price reactions to global climate strikes]] (Brief summary)
        - look at first big fridays for **futures climate strike and how this translates to financial markets**
        - find that firms with **higher carbon intensity** (Eurostat/Sustainalytics data) earn significantly **lower returns in the five proceeding days**
    - [[Attention to global warming]] (Brief summary)
        - **attention to climate change,** proxied by **Google search**, **increases** when **temperatures are abnormally high**
        - financial markets: **stocks of carbon-intensive firms underperform** firms with low carbon emissions in abnormally warm weather
        - retail investors sell carbon-intensive firms in such weather hence return patterns are **unlikely to be driven by fundamentals**

- ## Brainstorming: How do stock prices react to past climate change events?
    - **Events that could trigger stock price reactions**: climate conferences, climate policies, extreme weather events, social events (strikes), ..
    - Possible **considerations**
        - **reaction time**: the same day, the following day, the subsequent days
        - **separation of stocks**: according to sector/carbon sensitivity/ESG rating etc.
        - **separation of investment class**: maybe reaction is stronger in options
        - **transmission channels**: according to [[MSCI]]: direct shocks, market-disturbance shocks (market becomes generally more volatile but not connected to a certain event)
    - Any way to merge that with simulations later on ([[Explorer Stock Price Reaction to CC Events]])?
- ## Obtain data on past policies
    - **US climate-related policies** obtained from [[NewClimate Instiute - Climate Policy Database]]
        - filter: USA, impact: high
        - add dates of decision/signature to dataset
    - **worldwide climate change related political events** obtained from [[MSCI How Have Stocks Responded to Changes in Climate Policy?]]
         - contain nine political events, clustered in "progress" and "setback"
- ## Visulization of policies
    - yellow: US policies, orange: world politics with setback, green: world policies with progress  ![](https://firebasestorage.googleapis.com/v0/b/firescript-577a2.appspot.com/o/imgs%2Fapp%2FCCRisk%2FUnNp1-QGF2.png?alt=media&token=3c949b63-a5bd-4843-8d32-33ddb36108c6) 
- ## Return characteristics

- US policies 

|during event       | return_mean| return_variance|
|:-----------------|-----------:|---------------:|
|FALSE             |   0.0003798|       0.0001236|
|TRUE              |  -0.0040335|       0.0002578|

|date       | daily.returns|Policy_Name                                                                         |
|:----------|-------------:|:-----------------------------------------------------------------------------------|
|2005-08-08 |    -0.0019985|Energy Policy Act                                                                   |
|2007-12-19 |    -0.0019044|Renewable Fuel Standard (RFS)                                                       |
|2008-10-03 |    -0.0150217|C02 Capture and Sequestration Tax Credit                                            |
|2009-02-05 |     0.0133737|Federal Appliance Standards                                                         |
|2009-02-17 |    -0.0379356|American Recovery and Reinvestmant Act                                              |
|2011-03-30 |     0.0058311|Blueprint for a secure energy future                                                |
|2013-06-25 |     0.0068726|The President's Climate Action Plan Renewable Energy Target                         |
|2014-02-18 |    -0.0014850|National Program for Heavy-Duty Vehicle GHG emissions and Fuel Efficiency Standards |

- worldwide policies

|during event      | return_mean| return_variance|
|:-----------------|-----------:|---------------:|
|FALSE             |   0.0003762|       0.0001238|
|TRUE              |  -0.0005231|       0.0000192|


|date       | daily.returns|policy_name                      |impact   |
|:----------|-------------:|:--------------------------------|:--------|
|2015-08-18 |    -0.0019287|Energy transition law in France  |progress |
|2015-11-30 |    -0.0052472|Paris agreement                  |progress |
|2016-11-08 |     0.0040056|Trump's election in the US       |setback  |
|2017-06-01 |     0.0064511|US drops out of Paris Agreemnet  |setback  |
|2018-10-08 |     0.0015022|Bolsonaro's election in Barzil   |setback  |
|2019-05-20 |    -0.0032642|Morrison's election in Australia |setback  |
|2019-06-03 |     0.0001910|Carbon tax in South Africa       |progress |
|2019-09-20 |    -0.0058948|Carbon price in Germany          |progress |






