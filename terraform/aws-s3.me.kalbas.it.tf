data "aws_iam_policy_document" "me-kalbas-it" {
  statement {
    actions   = ["s3:GetObject"]
    resources = ["arn:aws:s3:::me.kalbas.it/*"]

    principals {
      type        = "AWS"
      identifiers = ["${aws_cloudfront_origin_access_identity.me-kalbas-it.iam_arn}"]
    }
  }

  statement {
    actions   = ["s3:ListBucket"]
    resources = ["arn:aws:s3:::me.kalbas.it"]

    principals {
      type        = "AWS"
      identifiers = ["${aws_cloudfront_origin_access_identity.me-kalbas-it.iam_arn}"]
    }
  }
}

resource "aws_s3_bucket" "me-kalbas-it" {
  provider = "aws.us-east-1"

  bucket = "me.kalbas.it"
  acl    = "private"
  policy = "${data.aws_iam_policy_document.me-kalbas-it.json}"

  tags = {
    Name = "My contact card"
  }

  lifecycle {
    prevent_destroy = true
  }
}
