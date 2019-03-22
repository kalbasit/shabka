resource "aws_s3_bucket" "www-yl-codes" {
  provider = "aws.us-east-1"

  bucket = "www.yl.codes"
  acl    = "public-read"

  website {
    redirect_all_requests_to = "kalbas.it"
  }
}

resource "aws_s3_bucket" "yl-codes" {
  provider = "aws.us-east-1"

  bucket = "yl.codes"
  acl    = "public-read"

  website {
    redirect_all_requests_to = "kalbas.it"
  }
}
