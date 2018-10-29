resource "aws_s3_bucket" "kalbas-it" {
  provider = "aws.us-east-1"

  bucket = "kalbas.it"
  acl    = "public-read"

  website {
    index_document = "index.html"
    error_document = "404.html"
  }

  tags = {
    Name = "My Blog"
  }

  lifecycle {
    prevent_destroy = true
  }
}
