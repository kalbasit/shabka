resource "aws_s3_bucket" "www-kalbas-it" {
  provider = "aws.us-east-1"

  bucket = "www.kalbas.it"
  acl    = "public-read"

  website {
    redirect_all_requests_to = "kalbas.it"
  }
}
