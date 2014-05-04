PREFIX ?= /usr/local

build: check-lein target/build/mbwatch

install: build
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 0755 bin/mbwatch-client target/build/mbwatch-daemon $(DESTDIR)$(PREFIX)/bin

clean: check-lein
	lein clean

target/build/mbwatch:
	@# FIXME: Iterative building does not work. This may be due to the
	@#        runtime import of macros into schema.core.
	rm -rf target/build/mbwatch-daemon target/build/classes
	lein BUILD

check-lein:
	@command -v lein &>/dev/null || { \
		echo "Please install leiningen first: http://leiningen.org/" >&2; \
		exit 1; \
	}
