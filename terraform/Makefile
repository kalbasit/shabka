.PHONY: all validate get refresh apply version get-deps

TERRAFORM ?= terraform
REFRESH ?= false

all: apply

version:
	@$(TERRAFORM) version

validate: get .terraform/terraform.tfstate
	@$(TERRAFORM) validate

get:
	@$(TERRAFORM) get

refresh: .terraform/terraform.tfstate
	@$(TERRAFORM) refresh

apply: validate get
	@$(TERRAFORM) apply -backup=".terraform/backup/state-$(shell date +"%s").tfstate" -refresh=$(REFRESH) -auto-approve=false

.terraform/terraform.tfstate:
	@$(TERRAFORM) init
