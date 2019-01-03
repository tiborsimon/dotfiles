#!/usr/bin/env python

import fileinput
from termcolor import cprint

counter = 0
for line in fileinput.input():
    if counter:
        counter += 1
    if line.startswith('='):
        counter = 1

    if counter > 5:
        if line.startswith('\t'):
            cprint(line.strip(), 'yellow')
        else:
            cprint(line.strip(), 'blue')
    else:
        print(line.strip())
