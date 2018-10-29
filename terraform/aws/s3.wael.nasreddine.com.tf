resource "aws_s3_bucket" "wael-nasreddine-com" {
  provider = "aws.us-east-1"

  bucket = "wael.nasreddine.com"
  acl    = "public-read"

  website {
    redirect_all_requests_to = "kalbas.it"
  }
}
