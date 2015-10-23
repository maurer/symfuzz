###############################################################################
# symfuzz Makefile                                                            #
#                                                                             #
# Copyright (c) 2014, Sang Kil Cha                                            #
# All rights reserved.                                                        #
# This software is free software; you can redistribute it and/or              #
# modify it under the terms of the GNU Library General Public                 #
# License version 2, with the special exception on linking                    #
# described in file LICENSE.                                                  #
#                                                                             #
# This software is distributed in the hope that it will be useful,            #
# but WITHOUT ANY WARRANTY; without even the implied warranty of              #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                        #
###############################################################################

all: depcheck src

src:
	make -C src

depcheck: Makefile.dep
	@buildtools/depcheck.sh $<

clean:
	make -C src clean

.PHONY: all src depcheck clean
