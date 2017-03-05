.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)

all: js-build/install-root js-build/table.js

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	ln -sf $(INSTALL_ROOT) js-build/install-root

js-build/table.js: $(INSTALL_ROOT)/bin/react-flux-table.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/table.js
	cat $(INSTALL_ROOT)/bin/react-flux-table.jsexe/all.js >> js-build/table.js
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/table.js
	sed -i 's/goog.provide.*//' js-build/table.js
	sed -i 's/goog.require.*//' js-build/table.js

clean:
	rm -rf js-build
