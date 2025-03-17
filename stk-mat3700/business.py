import os
import numpy as np
import yfinance as yf
import matplotlib.pyplot as plt

# Clear terminal
os.system('cls')

# Download stock data
def download_stock_data(ticker, start_date, end_date):
    stock = yf.download(ticker, start=start_date, end=end_date)
    return stock['Adj Close']

# Calculate log returns and volatility
def calculate_volatility(prices):
    log_returns = np.log(prices / prices.shift(1)).dropna()
    daily_volatility = log_returns.std()
    annualized_volatility = daily_volatility * np.sqrt(252)
    return log_returns, annualized_volatility

# Plot log returns
def plot_log_returns(log_returns, ticker):
    plt.figure(figsize=(10, 6))
    plt.plot(log_returns, label='Log Returns')
    plt.title(f'Log Returns of {ticker}')
    plt.xlabel('Date')
    plt.ylabel('Log Returns')
    plt.legend()
    plt.show()

# Main function
def main():
    ticker = 'TSLA'
    start_date = '2024-01-01'
    end_date = '2024-12-01'
    
    # Download and process stock data
    tsla_prices = download_stock_data(ticker, start_date, end_date)
    log_returns, annual_volatility = calculate_volatility(tsla_prices)
    
    print(f"First 5 Adjusted Close Prices:\n{tsla_prices.head()}")
    print(f"\nLog Returns:\n{log_returns.head()}")
    print(f"\nAnnualized Volatility: {annual_volatility:.4%}")
    
    # Plot results
    plot_log_returns(log_returns, ticker)

if __name__ == "__main__":
    main()

