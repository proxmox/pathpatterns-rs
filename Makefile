include /usr/share/dpkg/pkg-info.mk

SRCPACKAGE=rust-pathpatterns
PACKAGE=lib$(SRCPACKAGE)-dev
ARCH:=$(shell dpkg-architecture -qDEB_BUILD_ARCH)

DEB=$(PACKAGE)_$(DEB_VERSION)_$(ARCH).deb
DSC=$(SRCPACKAGE)_$(DEB_VERSION)_$(ARCH).deb

.PHONY: all
all: check

.PHONY: check
check:
	cargo test

.PHONY: dinstall
dinstall: deb
	sudo -k dpkg -i build/librust-*.deb

.PHONY: build
build:
	rm -rf build
	rm debian/control
	mkdir build
	debcargo package \
	    --config "$(PWD)/debian/debcargo.toml" \
	    --changelog-ready \
	    --no-overlay-write-back \
	    --directory "$(PWD)/build/pathpatterns" \
	    "pathpatterns" \
	    "$$(dpkg-parsechangelog -l "debian/changelog" -SVersion | sed -e 's/-.*//')"
	echo system >build/rust-toolchain
	rm -f build/pathpatterns/Cargo.lock
	find build/pathpatterns/debian -name '*.hint' -delete
	cp build/pathpatterns/debian/control debian/control

.PHONY: deb
deb: build build/$(DEB)
build/$(DEB): | build
	(cd build/pathpatterns && CARGO=/usr/bin/cargo RUSTC=/usr/bin/rustc dpkg-buildpackage -b -uc -us)
	lintian build/*.deb

.PHONY: dsc
dsc: build build/$(DSC)
build/$(DSC): | build
	(cd build/pathpatterns && CARGO=/usr/bin/cargo RUSTC=/usr/bin/rustc dpkg-buildpackage -S -uc -us)
	lintian build/*.dsc

.PHONY: clean
clean:
	rm -rf build *.deb *.dsc *.buildinfo *.changes *.orig.tar.gz
	cargo clean

.PHONY: upload
upload: UPLOAD_DIST ?= $(DEB_DISTRIBUTION)
upload: build/$(DEB)
	# check if working directory is clean
	git diff --exit-code --stat && git diff --exit-code --stat --staged
	tar -C build -cf - $(DEB) | ssh -X repoman@repo.proxmox.com upload --product devel --dist $(DEB_DISTRIBUTION)
