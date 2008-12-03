import nose.tools
from datetime import datetime
import urlcron

class TestSchedule(object):
    def test_create_disabled_all_ok(self):
        datadict = {
                "name": "schedule1",
                "pid" : "undefined",
                "start_time" : {
                    "year": 2008,
                    "month": 12,
                    "day": 31,
                    "hour": 12,
                    "minute": 45,
                    "seconds": 03
                 },

                "time_created" : {
                    "year": 2008,
                    "month": 11,
                    "day": 30,
                    "hour": 12,
                    "minute": 45,
                    "seconds": 03
                 },

                "time_started" : "undefined",
                "time_completed" : "undefined",

                "url" : "http://foo.com",
                "url_status" : "undefined",
                "url_headers" : "undefined",
                "url_content" : "undefined",

                "status" : "disabled"
                }

        s  = urlcron.Schedule(datadict)

        assert s.name == "schedule1"
        assert s.pid == None

        assert s.start_time == datetime(2008, 12, 31, 12, 45, 3)
        assert s.time_created == datetime(2008, 11, 30, 12, 45, 3)
        assert s.time_started == None
        assert s.time_completed == None

        assert s.url == "http://foo.com"
        assert s.url_status == None
        assert s.url_headers == None
        assert s.url_content == None

        assert s.status == "disabled"

    def test_create_with_missing_params(self):
        datadict = {
                "name": "undefined",
                "pid" : "undefined",
                "start_time" : {
                    "year": 2008,
                    "month": 12,
                    "day": 31,
                    "hour": 12,
                    "minute": 45,
                    "seconds": 03
                 },

                "time_created" : "undefined",
                "time_started" : "undefined",
                "time_completed" : "undefined",

                "url" : "http://foo.com",
                "url_status" : "undefined",
                "url_headers" : "undefined",
                "url_content" : "undefined",

                "status" : "disabled"
                }

        def create_schedule():
            return urlcron.Schedule(datadict)

        nose.tools.assert_raises(Exception, create_schedule)
