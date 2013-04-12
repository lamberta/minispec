IN_DIR=./pages
OUT_DIR=./html

doc: clean html index

all: ref doc

html:
	bash ./build/docgen.sh $(IN_DIR) $(OUT_DIR)

ref:
	bash ./build/ref/hyperspec.sh
	phantomjs ./build/ref/alexandria.js
	phantomjs ./build/ref/cl-ppcre.js
	phantomjs ./build/ref/iterate.js

index:
	phantomjs ./build/search-index.js $(OUT_DIR)/* > $(OUT_DIR)/search-index.json

clean:
	rm -rf $(OUT_DIR)

style:
	rm -rf $(OUT_DIR)/include
	cp -a ./build/include $(OUT_DIR)

deploy:
	bash ./build/deploy.sh $(OUT_DIR)
