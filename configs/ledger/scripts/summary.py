#!/usr/bin/env python

import io
import subprocess
import pandas as pd

import matplotlib
import matplotlib.pyplot as plt

import asciichartpy


BASE_COMMAND = "ledger register {} -X HUF --daily --register-format %d_%T\n"

TOTAL_COMMAND = BASE_COMMAND.format("Assets")


def get_data_for_command(command, name="A"):
    dates = []
    data = []

    proc = subprocess.Popen(command.split(" "), stdout=subprocess.PIPE)
    for line in io.TextIOWrapper(proc.stdout, encoding="utf-8"):
        date, balance = line.split("_")
        balance = int(balance.replace(",", "").replace(" HUF\n", ""))
        dates.append(date)
        data.append(balance)

    dates = pd.to_datetime(dates, yearfirst=True)
    index = pd.DatetimeIndex(dates)
    data = pd.DataFrame(data={name: data}, index=index)

    return data


def deduplicate(df):
    index = df.index.tolist()
    duplicated = df.index.duplicated()
    time_shift = 1
    for i in range(len(index)):
        if duplicated[i]:
            index[i] += pd.to_timedelta(time_shift, unit='m')
            time_shift += 1
        else:
            time_shift = 1
    df.index = index
    return df


df = get_data_for_command(TOTAL_COMMAND, name="total")
df = deduplicate(df)
df[df < 0] = 0
df = df.truncate(before="2018-03-01")

disp_index = pd.date_range(start="2018-03-01", end="now", periods=60)
df = df.reindex(disp_index, method="ffill")
values = df["total"].values.tolist()
print(asciichartpy.plot(values, cfg={"height": 10}))
