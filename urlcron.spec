Summary: A Cron system for the web.
Name: urlcron
Version: 1.0
Release: 1
License: Custom
Group: System/Daemons
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-build
Prereq: mochiweb


%description
URLCron is a cron implementation for the web. It exposes a REST interface, and the commands to be executed are URLS that are rather called back.

%prep
%setup -q -n %{name}-%{version}

%build
make

%install
make DESTDIR=%buildroot install

%clean
rm -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)

/etc/urlcron.conf
/var/lib/urlcron
/var/log/urlcron
/usr/bin/*
/usr/lib/erlang/lib/*
