#!/usr/bin/env python 

import numpy as np
# import matplotlib.pyplot as plt

rate = 1000000.0/125.0
samples = 10000

time = np.random.poisson(rate, samples)
req = np.random.randint(1, 6, size=samples)
server = np.random.randint(1, 5, size=samples)

for i in range(samples):
  print "%f %d %d" % (time[i], req[i], server[i])

