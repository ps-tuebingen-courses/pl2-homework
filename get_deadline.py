#!/bin/python3
import datetime
import os 

deadline_day = 3
today = datetime.datetime.today()
current_day = today.weekday()
days_until_deadline = deadline_day - current_day + 7
deadline_day = today + datetime.timedelta(days=days_until_deadline)
print(deadline_day.strftime("%d.%m.%Y"))
