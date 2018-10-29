module "vpc_us-east-1" {
  source = "./modules/tf-vpc"
  name   = "vpc_us-east-1"
  cidr   = "${var.cidr["us-east-1"]}"

  enable_dns_hostnames = true
  enable_dns_support   = true

  public_subnets = ["${var.public_subnets["us-east-1"]}"]

  azs = ["${var.azs["us-east-1"]}"]

  providers = {
    aws = "aws.us-east-1"
  }
}
