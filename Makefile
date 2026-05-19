BASE_DIR = $(shell pwd)
BUILD_DIR = $(BASE_DIR)/build

FZF ?= $(shell [ -x "$$(command -v fzf)" ] && echo true || echo false)

.PHONY: pre fmt build run clean update localpublish

pre:
	@mkdir -p $(BUILD_DIR)

fmt:
	@scalafmt

build: pre 
	@sbt compile

run: pre
	@sbt run

clean:
	@rm -rf $(BUILD_DIR)

update:
	@sbt clean bloopInstall
	@sbt update
	@sbt reload

localpublish:
	@sbt clean
	@sbt publishLocal
