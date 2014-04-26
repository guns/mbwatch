PREFIX ?= /usr/local

build: check-lein target/build/mbwatch

install: build
	install -d $(DESTDIR)$(PREFIX)/bin
	install target/build/mbwatch $(DESTDIR)$(PREFIX)/bin

clean: check-lein
	lein clean

target/build/mbwatch:
	lein BUILD

check-lein:
	@command -v lein &>/dev/null || { \
		echo "Please install leiningen first: http://leiningen.org/" >&2; \
		exit 1; \
	}
