from datetime import datetime

def ensure(value, name):
    if not value or (isinstance(value, str) and not value.strip()):
        raise Exception("%s can not be undefined" % (name))

def get_string(param_dict, key):
    if param_dict[key].strip() == "undefined":
        return None

    return param_dict[key].strip()

def get_date(param_dict, key):
    if isinstance(param_dict[key], str) and param_dict[key].strip() == "undefined":
        return None

    year = param_dict[key]['year']
    month = param_dict[key]['month']
    day = param_dict[key]['day']
    hour = param_dict[key]['hour']
    min = param_dict[key]['minute']
    secs = param_dict[key]['seconds']

    return datetime(year, month, day, hour, min, secs)
