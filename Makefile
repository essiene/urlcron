NAME=urlcron
VERSION=1.0
EBIN_DIR=ebin
INCLUDE_DIR=include
BIN_DIR=bin
ERL_LIB=/usr/lib/erlang/lib

all: 
	@cd src; make

test: all
	@cd tests; make

run: all
	@$(BIN_DIR)/urlcron

clean:
	@cd src; make clean
	@cd tests; make clean
	@rm -rf ebin

distclean: clean
	@rm -f *.tar.gz
	@rm -f *.rpm
	@rm -rf $(NAME)-$(VERSION)


install: all
	@mkdir -p $(DESTDIR)/etc
	@mkdir -p $(DESTDIR)/var/lib/urlcron
	@mkdir -p $(DESTDIR)/var/log/urlcron
	@mkdir -p $(DESTDIR)/$(ERL_LIB)/$(NAME)-$(VERSION)/{ebin,include}
	@mkdir -p $(DESTDIR)/usr/bin
	@echo
	@cp config/* $(DESTDIR)/etc
	@cp $(EBIN_DIR)/* $(DESTDIR)/$(ERL_LIB)/$(NAME)-$(VERSION)/ebin/
	@cp $(INCLUDE_DIR)/* $(DESTDIR)/$(ERL_LIB)/$(NAME)-$(VERSION)/include/
	@install -m 755 $(BIN_DIR)/urlcron $(DESTDIR)/usr/bin/
	@install -m 755 $(BIN_DIR)/urlcrond $(DESTDIR)/usr/bin/
	@install -m 755 $(BIN_DIR)/urlcronctl $(DESTDIR)/usr/bin/

dist: distclean
	@mkdir -p $(NAME)-$(VERSION)
	@cp BUGS TODO README $(NAME)-$(VERSION)
	@cp Makefile $(NAME)-$(VERSION)
	@cp -r bin config include src tests $(NAME)-$(VERSION)
	@tar -czvf $(NAME)-$(VERSION).tar.gz $(NAME)-$(VERSION)
	@rm -rf $(NAME)-$(VERSION)

rpm: dist
	@rm -rf /usr/src/redhat/SOURCES/$(NAME)*
	@rm -rf /usr/src/redhat/RPMS/i386/$(NAME)*
	@mv $(NAME)-$(VERSION).tar.gz /usr/src/redhat/SOURCES/
	@cp $(NAME).spec /usr/src/redhat/SPECS/
	@rpmbuild -bb /usr/src/redhat/SPECS/$(NAME).spec
	@mv /usr/src/redhat/RPMS/i386/$(NAME)*.rpm .
