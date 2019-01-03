#!/usr/bin/env python
import os
import commands
import re


def get_file_list():
    return os.listdir(os.getcwd())


def get_creation_date(file):
    (status, output) = commands.getstatusoutput('hachoir-metadata {} | grep "Creation date" | head -1'.format(file))
    try:
        date = output.split(': ')[1]
        date = date.replace(' ', '_')
        date = date.replace('-', '')
        date = date.replace(':', '')
    except:
        print('Date Cannot be found: {}, {}'.format(file, output))
        date = 'Manual-date-required'
    return date

def construct_rename_data(files):
    ret = []
    for file in files:
        rd = {'old': file}
        filename, file_extension = os.path.splitext(file)
        date = get_creation_date(file)
        rd['new'] = date + '_{{}}{}'.format(file_extension)
        ret.append(rd)
    ret.sort(key=lambda rd: rd['new'])
    i = 1
    for r in ret:
        r['new'] = r['new'].format(i)
        i += 1
    return ret


def prepare_files():
    os.system("dot_clean .")
    files = get_file_list()
    data = {
        'images': [],
        'videos': []
    }
    for file in files:
        if re.search('\.(jpg|jpeg|cr2|png)$', file.lower()):
            data['images'].append(file)
        if re.search('\.(mov|mp4)$', file.lower()):
            data['videos'].append(file)
    print('There are {0} files: {1} images, {2} videos.'.format(len(files), len(data['images']), len(data['videos'])))
    return data

def prompt_for_action():
    try:
        input = raw_input
    except NameError:
        pass


if __name__ == '__main__':
    prepare_files()
    prompt_for_action()
