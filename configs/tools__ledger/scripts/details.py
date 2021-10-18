#!/usr/bin/env python

import io
import subprocess
import pandas as pd

import matplotlib
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

import asciichartpy

DAILY_BASE_COMMAND = "ledger register {} -X HUF --daily --register-format %d:%T\n"


def get_daily_data_for_command(command, name="A"):
    dates = []
    data = []

    proc = subprocess.Popen(command.split(" "), stdout=subprocess.PIPE)
    for line in io.TextIOWrapper(proc.stdout, encoding="utf-8"):
        try:
            date, balance = line.split(":")
        except:
            pass

        balance = float(balance.replace(",", "").replace(" HUF\n", ""))
        dates.append(date)
        data.append(int(balance))

    dates = pd.to_datetime(dates, yearfirst=True)
    index = pd.DatetimeIndex(dates)
    df = pd.DataFrame(data={name: data}, index=index)
    return df


def deduplicate(df):
    return df[~df.index.duplicated(keep="last")]


def reindex(df, index):
    return df.reindex(index, method="ffill", fill_value=0)


def get_cashflow():
    CREDIT_HUF_COMMAND = DAILY_BASE_COMMAND.format("Assets:Credit:OTP:Forint")
    CREDIT_EUR_COMMAND = DAILY_BASE_COMMAND.format("Assets:Credit:Revolut:Euro")
    CASH_HUF_COMMAND = DAILY_BASE_COMMAND.format("Assets:Cash:Forint")
    CASH_EUR_COMMAND = DAILY_BASE_COMMAND.format("Assets:Cash:Euro")

    credit_huf = get_daily_data_for_command(CREDIT_HUF_COMMAND, name="credit_huf")
    credit_eur = get_daily_data_for_command(CREDIT_EUR_COMMAND)
    cash_huf = get_daily_data_for_command(CASH_HUF_COMMAND)
    cash_eur = get_daily_data_for_command(CASH_EUR_COMMAND)

    credit_huf = deduplicate(credit_huf)
    credit_eur = deduplicate(credit_eur)
    cash_huf = deduplicate(cash_huf)
    cash_eur = deduplicate(cash_eur)

    base_index = pd.date_range(start="2018-03-01", end="now", freq="D")

    credit_huf = reindex(credit_huf, base_index)
    credit_eur = reindex(credit_eur, base_index)
    cash_huf = reindex(cash_huf, base_index)
    cash_eur = reindex(cash_eur, base_index)

    df = credit_huf
    df["credit_eur"] = credit_eur
    df["cash_huf"] = cash_huf
    df["cash_eur"] = cash_eur
    df[df < 0] = 0

    return df


def get_adjustments():
    CREDIT_HUF_COMMAND = DAILY_BASE_COMMAND.format("Assets:Credit:OTP:Forint")
    CREDIT_EUR_COMMAND = DAILY_BASE_COMMAND.format("Assets:Credit:Revolut:Euro")
    CASH_HUF_COMMAND = DAILY_BASE_COMMAND.format("Assets:Cash:Forint")
    CASH_EUR_COMMAND = DAILY_BASE_COMMAND.format("Assets:Cash:Euro")

    credit_huf = get_daily_data_for_command(CREDIT_HUF_COMMAND, name="credit_huf")
    credit_eur = get_daily_data_for_command(CREDIT_EUR_COMMAND)
    cash_huf = get_daily_data_for_command(CASH_HUF_COMMAND)
    cash_eur = get_daily_data_for_command(CASH_EUR_COMMAND)

    credit_huf = deduplicate(credit_huf)
    credit_eur = deduplicate(credit_eur)
    cash_huf = deduplicate(cash_huf)
    cash_eur = deduplicate(cash_eur)

    base_index = pd.date_range(start="2018-02-20", end="now", freq="M")

    credit_huf = reindex(credit_huf, base_index)
    credit_eur = reindex(credit_eur, base_index)
    cash_huf = reindex(cash_huf, base_index)
    cash_eur = reindex(cash_eur, base_index)

    df = credit_huf
    df["credit_eur"] = credit_eur
    df["cash_huf"] = cash_huf
    df["cash_eur"] = cash_eur
    df[df < 0] = 0

    return df


# ========================================================================
# P L O T T I N G

cashflow = get_cashflow()

fig, ax = plt.subplots()

cashflow.plot.area(
    x_compat=True,
    y=["cash_eur", "credit_eur", "credit_huf", "cash_huf"],
    ax=ax
)
ax.get_yaxis().set_major_formatter(
    matplotlib.ticker.FuncFormatter(lambda x, p: format(int(x), ','))
)
ax.set_title("Asset timeline")
ax.xaxis.set_major_locator(plt.NullLocator())
ax.xaxis.set_major_locator(mdates.MonthLocator())
# ax.xaxis.set_minor_locator(mdates.DayLocator())
ax.grid()
ax.label_outer()

fig.tight_layout()
plt.show()
