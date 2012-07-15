"""
Gather the entire NHANES into a hash table
"""

import os
import csv

from collections import defaultdict

from util import test_kwargs


def get_data():

    NHANES = defaultdict(lambda: {})

    for dirname, dirnames, filenames in os.walk('csv'):
        for subdirname in dirnames:
            print os.path.join(dirname, subdirname)
        for filename in filenames:
            p = os.path.join(dirname, filename)
            print p
            csvReader = csv.reader(open(p))
            row0 = csvReader.next()
            for row in csvReader:
                pk = row[1]
                mem = NHANES[pk]
                for colnum in range(2, len(row)):
                    value = row[colnum]
                    name = row0[colnum]
                    mem[name] = value

    return NHANES

