import nose.tools
import urlcron
from datetime import datetime
from urlcron import weblib


class TestResponse(object):
    def test_error_response(self):
        json = '{"status": 0, "data": "Funky Error"}'

        r = urlcron.Response(json)

        assert r.status == False 
        assert r.data == "Funky Error"

    def test_success_string_data_response(self):
        json = '{"status": 1, "data": "Successfull"}'

        r = urlcron.Response(json)

        assert r.status == True
        assert r.data == "Successfull"


    def test_success_schedule_data_response(self):
        json = '''{
                    "status" : 1,
                    "data" : {
                        "name": "schedule1",
                        "pid" : "undefined",
                        "start_time" : {
                            "year": 2008,
                            "month": 12,
                            "day": 31,
                            "hour": 12,
                            "minute": 45,
                            "seconds": 3
                         },
                        "time_created" : {
                            "year": 2008,
                            "month": 11,
                            "day": 30,
                            "hour": 12,
                            "minute": 45,
                            "seconds": 3
                         },
                        "time_started" : "undefined",
                        "time_completed" : "undefined",
                        "url" : "http://foo.com",
                        "url_status" : "undefined",
                        "url_headers" : "undefined",
                        "url_content" : "undefined",
                        "status" : "disabled"
                   }
                }'''

        r = urlcron.Response(json)

        assert r.status == True
        assert r.data.name == "schedule1"
        assert r.data.pid == None

        assert r.data.start_time == datetime(2008, 12, 31, 12, 45, 3)
        assert r.data.time_created == datetime(2008, 11, 30, 12, 45, 3)
        assert r.data.time_started == None
        assert r.data.time_completed == None

        assert r.data.url == "http://foo.com"
        assert r.data.url_status == None
        assert r.data.url_headers == None
        assert r.data.url_content == None

        assert r.data.status == "disabled"

    def test_to_response_ok_schedule(self):

        @urlcron.Response.to_response
        def return_json():
            return '''{
                        "status" : 1,
                        "data" : {
                            "name": "schedule1",
                            "pid" : "undefined",
                            "start_time" : {
                                "year": 2008,
                                "month": 12,
                                "day": 31,
                                "hour": 12,
                                "minute": 45,
                                "seconds": 3
                             },
                            "time_created" : {
                                "year": 2008,
                                "month": 11,
                                "day": 30,
                                "hour": 12,
                                "minute": 45,
                                "seconds": 3
                             },
                            "time_started" : "undefined",
                            "time_completed" : "undefined",
                            "url" : "http://foo.com",
                            "url_status" : "undefined",
                            "url_headers" : "undefined",
                            "url_content" : "undefined",
                            "status" : "disabled"
                       }
                    }'''

        r = return_json()

        assert r.status == True
        assert r.data.name == "schedule1"
        assert r.data.pid == None

        assert r.data.start_time == datetime(2008, 12, 31, 12, 45, 3)
        assert r.data.time_created == datetime(2008, 11, 30, 12, 45, 3)
        assert r.data.time_started == None
        assert r.data.time_completed == None

        assert r.data.url == "http://foo.com"
        assert r.data.url_status == None
        assert r.data.url_headers == None
        assert r.data.url_content == None

        assert r.data.status == "disabled"


    def test_to_response_ok_success_message(self):

        @urlcron.Response.to_response
        def return_json():
            return '''{
                        "status" : 1,
                        "data" : "All Successfull"
                    }'''

        r = return_json()

        assert r.status == True
        assert r.data == "All Successfull"

    def test_to_response_ok_error_message(self):

        @urlcron.Response.to_response
        def return_json():
            return '''{
                        "status" : 0,
                        "data" : "Task Failed"
                    }'''

        r = return_json()

        assert r.status == False
        assert r.data == "Task Failed"

    def test_to_response_fail(self):

        @urlcron.Response.to_response
        def return_json():
            raise Exception("Badass Exception")

        r = return_json()

        assert r.status == False
