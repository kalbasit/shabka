.PHONY: all validate get refresh apply version check-aws-perms

TERRAFORM ?= terraform
REFRESH ?= false
CURRENT_DIR := $(shell pwd)

all: apply

version:
	@$(TERRAFORM) version

validate: get .terraform/terraform.tfstate
	@$(TERRAFORM) validate

get:
	@$(TERRAFORM) get

refresh: .terraform/terraform.tfstate
	@$(TERRAFORM) refresh -var-file terraform.tfvars

apply: validate get check-aws-perms
	@$(TERRAFORM) apply -backup=".terraform/backup/state-$(shell date +"%s").tfstate" -refresh=$(REFRESH) -auto-approve=false

check-aws-perms:
ifneq "$(shell ../scripts/check-aws-perms.sh)" "1"
	$(error "The current AWS credentials do not have access to write to the state upstream")
endif

.terraform/terraform.tfstate:
	@$(TERRAFORM) init
