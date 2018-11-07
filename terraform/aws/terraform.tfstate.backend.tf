terraform {
  backend "s3" {
    bucket = "nasreddine-infra"
    key    = "terraform/aws.tfstate"
    region = "us-west-1"
  }
}
