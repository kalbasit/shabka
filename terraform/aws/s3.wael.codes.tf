resource "aws_s3_bucket" "www-wael-codes" {
  provider = "aws.us-east-1"

  bucket = "www.wael.codes"
  acl    = "public-read"

  website {
    redirect_all_requests_to = "wael.nasreddine.com"
  }
}

resource "aws_s3_bucket" "wael-codes" {
  provider = "aws.us-east-1"

  bucket = "wael.codes"
  acl    = "public-read"

  website {
    redirect_all_requests_to = "wael.nasreddine.com"
  }
}
