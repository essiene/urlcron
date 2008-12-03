import nose.tools
import datetime
import util

class TestPyCron(object):
    def test_ensure_fails_on_none_value(self):
        nose.tools.assert_raises(Exception, util.ensure, None, "foo")
        nose.tools.assert_raises(Exception, util.ensure, "", "foo")
        nose.tools.assert_raises(Exception, util.ensure, " ", "foo")

    def test_ensure_passes_when_not_none(self):
        assert util.ensure("foo", "foo") == None


    def test_get_string(self):
        pdict = {"key1": "undefined", "key2":"value"}
        assert util.get_string(pdict, "key1") == None
        assert util.get_string(pdict, "key2") == "value"

    def test_get_date(self):
        pdict = {"date1":"undefined", "date2": {"year":2008, "month":12, "day":3, "hour":12, "minute":45, "seconds":03}}
        assert util.get_date(pdict, "date1") == None
        assert util.get_date(pdict, "date2") == datetime.datetime(2008, 12, 3, 12, 45, 3)
