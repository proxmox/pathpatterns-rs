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
deb: build
	(cd build/pathpatterns && CARGO=/usr/bin/cargo RUSTC=/usr/bin/rustc dpkg-buildpackage -b -uc -us)
	lintian build/*.deb

.PHONY: dsc
dsc: build
	(cd build/pathpatterns && CARGO=/usr/bin/cargo RUSTC=/usr/bin/rustc dpkg-buildpackage -S -uc -us)
	lintian build/*.dsc

.PHONY: clean
clean:
	rm -rf build *.deb *.dsc *.buildinfo *.changes *.orig.tar.gz
	cargo clean

upload: deb
	cd build; \
	    dcmd --deb rust-pathpatterns_*.changes \
	    | grep -v '.changes$$' \
	    | tar -cf- -T- \
	    | ssh -X repoman@repo.proxmox.com upload --product devel --dist buster
