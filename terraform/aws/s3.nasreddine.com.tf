resource "aws_s3_bucket" "www-nasreddine-com" {
  provider = "aws.us-east-1"

  bucket = "www.nasreddine.com"
  acl    = "public-read"

  website {
    redirect_all_requests_to = "wael.nasreddine.com"
  }
}

resource "aws_s3_bucket" "nasreddine-com" {
  provider = "aws.us-east-1"

  bucket = "nasreddine.com"
  acl    = "public-read"

  website {
    redirect_all_requests_to = "wael.nasreddine.com"
  }
}
