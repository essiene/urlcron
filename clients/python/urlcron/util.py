import urllib
from datetime import datetime

def ensure(value, name):
    if not value or (isinstance(value, basestring) and not value.strip()):
        raise Exception("%s can not be undefined" % (name))

def get_string(param_dict, key):
    value = param_dict[key]

    if isinstance(value, basestring) and value.strip() == "undefined":
        return None

    return value.strip()

def get_date(param_dict, key):
    date_dict = param_dict[key]

    if not isinstance(date_dict, dict):
        return None

    year = date_dict['year']
    month = date_dict['month']
    day = date_dict['day']
    hour = date_dict['hour']
    min = date_dict['minute']
    secs = date_dict['seconds']

    return datetime(year, month, day, hour, min, secs)

def to_tuplelist(start_time):
    return [ 
            ("year", start_time.year),
            ("month", start_time.month),
            ("day", start_time.day),
            ("hour", start_time.hour),
            ("minute", start_time.minute),
            ("second", start_time.second)
            ]

def urlencode(params=None, **kw):
    if params:
        tuplelist = params
    else:
        tuplelist = []

    for key, val in kw.items():
        tuplelist.append((key, val))

    tuplelist.sort()
    return urllib.urlencode(tuplelist)
