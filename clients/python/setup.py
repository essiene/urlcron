from setuptools import setup, find_packages

setup (
		name = "urlcron",
		version = "0.1",
		packages = find_packages('.'),
		package_dir = {'':'.'},

		author = "UKN Global",
		author_email = "info@uknglobal.com",
		license = "Custom",
		description = "Python client library for urlcron webservice",
		keywords = "",
		url = "http://www.uknglobal.com",
        test_suite = 'nose.collector',

		long_description = """
			Python client library for urlcron webservice
		""",
		)
