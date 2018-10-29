variable "public_ssh_key" {
  description = "The content of the public SSH key"
  type        = "string"
}

variable "aws_access_key" {
  description = "AWS_ACCESS_KEY_ID of the IAM account with admin access"
  type        = "string"
}

variable "aws_secret_key" {
  description = "AWS_SECRET_ACCESS_KEY of the IAM account with admin access"
  type        = "string"
}

variable "google_apps_domains" {
  description = "The Google Apps domain list"
  type        = "list"

  default = [
    "calendar",
    "docs",
    "drive",
    "groups",
    "hangouts",
    "mail",
    "plus",
    "sheets",
    "sites",
    "slides",
    "start",
    "vault",
  ]
}

variable "azs" {
  description = "The AZS per region"

  default = {
    us-east-1 = [
      "us-east-1a",
      "us-east-1c",
      "us-east-1d",
      "us-east-1e",
    ]
  }

  type = "map"
}

variable "cidr" {
  description = "The CIDR block for the VPC"

  default = {
    us-east-1 = "172.26.0.0/16"
  }
}

variable "public_subnets" {
  description = "The public CIDR blocks for the AZS. Must be size of the azs"

  default = {
    us-east-1 = [
      "172.26.4.0/24",
      "172.26.5.0/24",
      "172.26.6.0/24",
      "172.26.7.0/24",
    ]
  }

  type = "map"
}
