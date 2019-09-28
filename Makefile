# SPDX-License-Identifier: Unlicense
# chez-rpi Makefile.

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
DEST = ~/lib

# Path to chez scheme executable.
SCHEME = /usr/bin/scheme

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

CFLAGS = -c -fpic $(CONFIG_H)

LIBFLAGS = -shared

## Should be no need to edit anything below here.

COBJS = rpi/dht.o

BINS = rpi/libdht.so

SOBJS = rpi/ftypes-util.so rpi/cdht.so rpi/dht-common.so rpi/dht.so rpi/gpio.so rpi/gpiomem.so rpi/poll.so

all: $(BINS) $(SOBJS)

rpi/libdht.so: $(COBJS)
	$(CC) $(LIBFLAGS) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

%.so: %.sls
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

install: all
	$(INSTALL) -D -t $(DEST)/rpi $(BINS) $(SOBJS)

clean:
	rm -f $(COBJS) $(SOBJS) $(BINS)
