from tda.auth import easy_client
from tda.client import Client

from tda import auth, client
import json
from datetime import date
from json2table import convert

token_path = '/home/bsiranos/token.pickle'
api_key = 'QF5QJ3AQ0MU5MFEZHT5ESO362ERPWJ7I@AMER.OAUTHAP'
redirect_uri = 'https://localhost'
try:
    c = auth.client_from_token_file(token_path, api_key)
except FileNotFoundError:
    from selenium import webdriver
    with webdriver.Chrome() as driver:
        c = auth.client_from_login_flow(
            driver, api_key, redirect_uri, token_path)

resp = c.get_price_history('AAPL',
        period_type=Client.PriceHistory.PeriodType.YEAR,
        period=Client.PriceHistory.Period.TWENTY_YEARS,
        frequency_type=Client.PriceHistory.FrequencyType.DAILY,
        frequency=Client.PriceHist ory.Frequency.DAILY)
assert resp.ok
history = resp.json()


resp = c.get_option_chain('SPY',
    contract_type=client.Client.Options.ContractType('CALL'), 
    strike=300.0,
    strike_to_date=date(2020,7,3))
assert resp.ok
chain=resp.json()

    contract_type='ALL', 
    strike_count=10)

    

chain["symbol"]
chain["status"]
chain["underlying"]
chain["strategy"]
chain["interval"]
chain["isDelayed"]
chain["isIndex"]
chain["interestRate"]
chain["underlyingPrice"]
chain["volatility"]
chain["daysToExpiration"]
chain["numberOfContracts"]

chain["callExpDateMap"]['2020-06-29:2']

chain["putExpDateMap"]
