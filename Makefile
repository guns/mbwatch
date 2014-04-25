PREFIX ?= /usr/local

build: check-lein target/mbwatch

install: build
	install -d $(DESTDIR)$(PREFIX)/bin
	install target/mbwatch $(DESTDIR)$(PREFIX)/bin

clean: check-lein
	lein clean

target/mbwatch:
	lein BUILD

check-lein:
	@command -v lein &>/dev/null || { \
		echo "Please install leiningen first: http://leiningen.org/" >&2; \
		exit 1; \
	}
