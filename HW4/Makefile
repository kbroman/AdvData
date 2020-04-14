R_OPTS=--no-save --no-restore --no-init-file --no-site-file

all: ../docs/hw4_data.zip ../docs/hw4.md

nhanes_diabetes.csv: assemble_data.R
	R CMD BATCH $(R_OPTS) $<

hw4_data.zip: nhanes_diabetes.csv nhanes_diabetes_data_dict.csv
	zip $@ $^

../docs/%: %
	cp $< $@
