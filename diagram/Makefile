EXE_eps_dia=hello_eps_dia.txt
EXE_pdf_dia=hello_pdf_dia.txt
SRC_dia= $(shell find . -name "*.dia")
OBJ_eps_dia= $(SRC_dia:.dia=.eps)
OBJ_pdf_dia= $(SRC_dia:.dia=.pdf)
	 
EXE_eps_dot=hello_eps_dot.txt
EXE_pdf_dot=hello_pdf_dot.txt
EXE_png_dot=hello_png_dot.txt
SRC_dot= $(shell find . -name "*.dot")
OBJ_eps_dot= $(SRC_dot:.dot=.eps)
OBJ_pdf_dot= $(SRC_dot:.dot=.pdf)
OBJ_png_dot= $(SRC_dot:.dot=.png)

MAKE_DONE = make-done
	 
all: dia graphviz
	@touch $(MAKE_DONE)

clean :
	git clean -dfx

dia: $(EXE_eps_dia) $(EXE_pdf_dia)

#graphviz: $(EXE_eps_dot) $(EXE_pdf_dot)
graphviz: $(EXE_png_dot)
	 
$(EXE_eps_dia): $(OBJ_eps_dia)
	@date > $(EXE_eps_dia)

$(EXE_pdf_dia): $(OBJ_pdf_dia)
	@date > $(EXE_pdf_dia)

%.eps: %.dia
	@echo "\t - $@"
	@dia -t eps $< -e $@

%.pdf: %.dia
	@echo "\t - $@"
	@dia -t pdf $< -e $@

$(EXE_eps_dot): $(OBJ_eps_dot)
	@date > $(EXE_eps_dot)

$(EXE_pdf_dot): $(OBJ_pdf_dot)
	@date > $(EXE_pdf_dot)

$(EXE_png_dot): $(OBJ_png_dot)
	@date > $(EXE_png_dot)
	 
%.eps: %.dot
	@echo "\t - $@"
	@dot -Teps $< -o $@

%.pdf: %.dot
	@echo "\t - $@"
	@dot -Tpdf $< -o $@

%.png: %.dot
	@echo -e "\t - $@"
	@dot -Tpng $< -o $@

