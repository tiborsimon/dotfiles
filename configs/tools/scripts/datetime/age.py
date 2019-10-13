#! /usr/bin/python
from datetime import datetime
from datetime import date
import sys

"""Age calculator script specifically created for my orgfiles  system.

This script is called by the org file that contains all the birthsdays that
I care about. It is called with the given birth date in %Y-%m-%d format
and it calculates the current age and returns a multiline string that
can be processed by the org script executer in a pretty way.

Example:

    #+BEGIN_SRC bash :var d="<1990-09-10 Mon>"
    my-age $d
    #+END_SRC

    #+RESULTS:
    | Szuletett: | <1990-09-10 Mon> |
    | Jelenleg:  | *28*             |
    | Iden_lesz: | *29*             |
    | Frisitve:  | [2019-01-03 Thu] |

"""


def calculate_age(born):
    today = date.today()
    birthday_passed = ((today.month, today.day) >= (born.month, born.day))
    difference = today.year - born.year
    return difference if birthday_passed else difference - 1


def calculate_this_year(born):
    b = born.split('-')[0]
    t = datetime.now().strftime('%Y')
    return int(t) - int(b)


def main():
    born = sys.argv[1].split(' ')[0][1:]
    today = datetime.now().strftime('%Y-%m-%d')
    age = calculate_age(datetime.strptime(born, '%Y-%m-%d'))
    this_year = calculate_this_year(born)
    print('Szuletett: <{}>'.format(born))
    print('Jelenleg:  *{}*'.format(age))
    print('Iden_lesz: *{}*'.format(this_year))
    print('Frisitve:  [{}]'.format(today))


if __name__ == '__main__':
    main()
