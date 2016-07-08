#!/usr/bin/env python
from __future__ import print_function
import json
import time
import os
import sys
from pprint import pprint

WORKLOG_PATH=sys.argv[1]

print('What is the message?')
message = raw_input()
temp = {
        'log': message
        }

temp['start'] = time.strftime('%Y-%m-%d %H:%M:%S')

print('What are the tags?')

tags = raw_input()

temp['tags'] = [x.strip() for x in tags.split(',')]

# pprint(temp)

with open(os.path.join(WORKLOG_PATH, 'worklog.json')) as f:
    data = json.load(f)

data.insert(0, temp)

with open(os.path.join(WORKLOG_PATH, 'worklog.json'), 'w') as f:
    data = json.dump(data, f, sort_keys = True, indent = 2, ensure_ascii=False)


