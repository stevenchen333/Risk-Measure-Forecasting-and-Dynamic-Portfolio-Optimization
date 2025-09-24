# Introduction
The problem of portfolio optimization have been revolutionized by Markowitz’s mean-variance
model in 1952  titled simply Portfolio Selection. However the portfolio optimization proposed
assume perfect knowledge of expected returns and portfolio covariance matrix, which is impossible
to have considering the unpredictable nature of the financial market. Many work have been
proposed to address this issue. For example, robust(or ditributionally robust) stochastic framework
uses worst case scenarios in which either investor considers the worst expected return, or the most
volatile price dynamics for a specific time period to optimize their portfolio. Optimal stochastic
framework aims to either minimize portfolio variance, maximize expected returns, or considers
both via risk averse reformulation of the Markowitz mean-variance model. There are numerous
methods that have been put forward to solve the optimization problem mentioned above. Layman
practitioners often uses historical data to estimate expected returns and price volatilities to assume
future parameters. But considering the uncertainty essence of the market, this method rarely works
satisfactorily.
In this work, we will attempt to use volatility and expected returns forecast via GARCH
model to solve the a risk averse optimization problems; i.) risk averse formulation of Markowitz’s
mean variance portfolio model. Then we will backtest the result using portfolio of ten assets with
varying degree of characteristics. We then compare the performance difference of two methods
using historical data methods as a baseline comparison.
We carefully design this portfolio to achieve diversification across multiple asset classes and
macroeconomic risk hedging. It includes assets from technology, finance, energy, manufacturing,
and digital assets. This diverse set of assets ensures emphasizing the methods we utilized works
with many portfolio of assets. To add to that, this portfolio will show us the behavior of the
solution againts differing kind of assets with different degree of returns volatility.
The portfolio is structured across multiple sectors—technology (AAPL, MSFT, META), finance
(V, JPM, C), energy/commodities (UEC, ET), manufacturing (GM), and digital assets (BTC)—to
balance growth, value, and income generation. Growth exposure focuses on innovative tech leaders,
while value and income are achieved through stable financials (JPM, C, GM) and high-yield assets
(ET). Additionally, the allocation provides macroeconomic hedging, with rate-sensitive financials,
inflation-resistant commodities, and defensive equities (AAPL, V) mitigating risks across different
economic conditions.


For a full report refer to the pdf file Risk-Measure-Forecasting-and-Dynamic-Portfolio-Optimization.pdf
