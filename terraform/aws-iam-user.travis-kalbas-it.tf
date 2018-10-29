data "aws_iam_policy_document" "travis-kalbas-it" {
  statement {
    actions = [
      "s3:AbortMultipartUpload",
      "s3:DeleteObject",
      "s3:GetObject",
      "s3:GetObjectAcl",
      "s3:ListBucket",
      "s3:PutObject",
      "s3:PutObjectAcl",
    ]

    resources = ["arn:aws:s3:::kalbas.it/*"]
  }

  statement {
    actions = [
      "s3:ListBucket",
    ]

    resources = ["arn:aws:s3:::kalbas.it"]
  }

  statement {
    actions = [
      "cloudfront:CreateInvalidation",
    ]

    // It seems that I cannot restrict this action to one distribution. Tested
    // on the IAM policy simulator
    // https://policysim.aws.amazon.com/home/index.jsp?#users/travis-kalbas-it/travis-kalbas-it
    resources = ["*"]
  }
}

resource "aws_iam_user" "travis-kalbas-it" {
  name = "travis-kalbas-it"
  path = "/travis/s3/"
}

resource "aws_iam_user_policy" "travis-kalbas-it" {
  name   = "travis-kalbas-it"
  user   = "${aws_iam_user.travis-kalbas-it.name}"
  policy = "${data.aws_iam_policy_document.travis-kalbas-it.json}"
}

resource "aws_iam_access_key" "travis-kalbas-it" {
  user = "${aws_iam_user.travis-kalbas-it.name}"
}

output "travis-kalbas-it access_key" {
  value = "${aws_iam_access_key.travis-kalbas-it.id}"
}

output "travis-kalbas-it secret_key" {
  value = "${aws_iam_access_key.travis-kalbas-it.secret}"
}
