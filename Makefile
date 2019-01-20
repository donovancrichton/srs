PPATH:=$(shell pwd)
TARGET1=Simple
TARGET2=BANNHC
TARGET3=paper
TARGET4=talk

DOCPATH=Docs
TALKPATH=Talk
CC=idris
CFLAGS=--codegen c $(TARGET1).idr -o $(TARGET1)
DC=latexmk
DFLAGS=-pdf -f -shell-escape
SRC=Src
EXP=Experiments

all: simple-c docs talk

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
  
talk:
	@cd $(PPATH)/$(SRC)/; $(DC) $(DFLAGS) $(TARGET4).tex
	@cd $(PPATH)/$(SRC)/; $(DC) -c $(TARGET4).tex
	 #remove minted report folder
	@rm -rf $(PPATH)/$(SRC)/_minted-talk
	@mkdir -p $(PPATH)/$(TALKPATH)
	@mv $(PPATH)/$(SRC)/$(TARGET4).pdf $(PPATH)/$(TALKPATH)
	#clean up after latexmk
	@rm -f $(PPATH)/$(SRC)/*.aux
	@rm -f $(PPATH)/$(SRC)/*.fls 
	@rm -f $(PPATH)/$(SRC)/*.fdb_latexmk
	@rm -f $(PPATH)/$(SRC)/*.nav
	@rm -f $(PPATH)/$(SRC)/*.toc
	@rm -f $(PPATH)/$(SRC)/*.log
	@rm -f $(PPATH)/$(SRC)/*.snm
	@rm -f $(PPATH)/$(SRC)/*.tex.bak
	@rm -f $(PPATH)/$(SRC)/*.vrb
  

clean:
	@rm $(PPATH)/$(DOCPATH)/$(TARGET3).pdf
