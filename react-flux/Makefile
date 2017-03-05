.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)

all: js-build/install-root

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	ln -sf $(INSTALL_ROOT) js-build/install-root


clean:
	rm -rf js-build
