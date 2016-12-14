######
#
# A simple Makefile
#
######

PATSCC=$(PATSHOME)/bin/patscc
ATSCC2JS=$(PATSHOME)/bin/atscc2js

######

all::

######
#
all:: \
Cell_dats.js
#
Cell_dats.c: Cell.dats; $(PATSCC) -ccats $<
Cell_dats.js: Cell_dats.c; $(ATSCC2JS) -o $@ -i $<
#
######

RMF=rm -f

######

clean:: ; $(RMF) *~
clean:: ; $(RMF) *_dats.c

######

cleanall:: clean
cleanall:: ; $(RMF) *_dats.js

###### end of [Makefile] ######
