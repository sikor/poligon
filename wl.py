#!/usr/bin/env python3

import subprocess
import time
import datetime
import locale
import sys
import pathlib

if (sys.argv[1] != "stop") and (sys.argv[1] != "start" or len(sys.argv) != 4): 
  raise Exception('usage: wl start category activity | wl stop')

locale.setlocale(locale.LC_ALL, 'en_US.utf8')
dateformat = "%a %Y-%m-%d %H:%M:%S"

logDir = str(pathlib.Path.home()) + "/.worklog/"
logPath = logDir + "log.csv"
subprocess.run("mkdir -p " + logDir, shell=True)
subprocess.run("touch " + logPath, shell=True)

currentTime = datetime.datetime.today()
currentTimeStr = currentTime.strftime(dateformat)

with open(logPath, 'r+') as logFile:
  lines = logFile.read().splitlines()
  if len(lines) > 0:
    lastLine = lines[-1]
    if lastLine.endswith(";"):
      startTime = datetime.datetime.strptime(lastLine.split(";")[0], dateformat)
      diffTime = currentTime - startTime
      logFile.write(currentTimeStr + ";" + str(diffTime) + "\n")
  else:
    print("Log file is empty")

  if sys.argv[1] == "start":
    toLog = currentTimeStr + ";" + sys.argv[2] + ";" + sys.argv[3] + ";"
    logFile.write(toLog)
    print("Writing to log: " + toLog)
    
print("\nTail of log fail")
subprocess.run("tail " + logPath, shell=True)
print("\n")
