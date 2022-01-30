#!/usr/bin/env python

import click
from datetime import datetime, timedelta


def normalize_date_string(date_string):
    return date_string.upper()


def get_first_day_of_week(date_string):
    # https://stackoverflow.com/a/17087427 with a bit of modification.
    # https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior
    start_date = datetime.strptime(date_string + "-1", "%G-W%V-%u")
    return start_date


def output(result):
    print(result.strftime("%Y-%m-%d"))


@click.command()
@click.argument("date_string")
@click.option("--start/--end", default=True, help="Start or end of the week.")
@click.option("--inclusive/--exclusive", default=True, help="End of the week is excusive or inclusive.")
def week(date_string, start, inclusive):
    """
    Simple tool that calculates the start and end date of a given weekday.

    DATE_STRING is the input year and week that has to have the following
    format: `2020-W01`. The first week of the year is `W01`.

    EXAMPLE

    The second week of 2020 starts on 2020-01-06 and ends on 2020-01-12.

    \b
      [--start] 2020-W02            ->  2020-01-06
      --end [--inclusive] 2020-W02  ->  2020-01-12
      --end --exclusive 2020-W02    ->  2020-01-13
    """

    date_string = normalize_date_string(date_string)

    start_date = get_first_day_of_week(date_string)

    if start:
        result = start_date
    else:
        if inclusive:
            offset = timedelta(days=6)
        else:
            offset = timedelta(days=7)
        result = start_date + offset

    output(result)


if __name__ == '__main__':
    week()
