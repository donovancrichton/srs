PPATH:=$(shell pwd)
TARGET1=Simple
TARGET2=BANNHC
TARGET3=paper

DOCPATH=Docs
CC=idris
CFLAGS=--codegen c $(TARGET1).idr -o $(TARGET1)
DC=latexmk
DFLAGS=-pdf -f -shell-escape
SRC=Src
EXP=Experiments

.PHONY: docs

all: simple-c docs

simple-c:
	@cd $(PPATH)/$(SRC)/$(EXP); $(CC) $(CFLAGS)
	@mv $(PPATH)/$(SRC)/$(EXP)/$(TARGET1) $(PPATH)/Bin/
	@rm -f $(PPATH)/$(SRC)/$(EXP)/$(TARGET1).ibc
	@rm -f $(PPATH)/$(SRC)/$(EXP)/$(TARGET1).idr~

docs:
	@cd $(PPATH)/$(SRC)/; $(DC) $(DFLAGS) $(TARGET3).tex
	@cd $(PPATH)/$(SRC)/; $(DC) -c $(TARGET3).tex
	#remove minted report folder
	@rm -rf $(PPATH)/$(SRC)/_minted-report
	#remove any unwanted backups
	@rm -f $(PPATH)/$(SRC)/$(TARGET3).tex.bak
	@mkdir -p $(PPATH)/$(DOCPATH)
	@mv $(PPATH)/$(SRC)/$(TARGET3).pdf $(PPATH)/$(DOCPATH)
	#clean up after latexmk
	@rm -f $(PPATH)/$(SRC)/*.aux
	@rm -f $(PPATH)/$(SRC)/*.fls
	@rm -f $(PPATH)/$(SRC)/*.fdb_latexmk
  
clean:
	@rm $(PPATH)/$(DOCPATH)/$(TARGET3).pdf
