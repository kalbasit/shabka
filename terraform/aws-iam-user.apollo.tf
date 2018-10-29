resource "aws_iam_user" "apollo" {
  name = "apollo"
  path = "/home/nas/"
}

data "aws_iam_policy_document" "apollo" {
  statement {
    effect = "Allow"

    actions = [
      "s3:*",
    ]

    resources = [
      "${aws_s3_bucket.kapollo.arn}",
      "${aws_s3_bucket.kapollo.arn}/*",
    ]
  }

  statement {
    effect = "Allow"

    actions = [
      "s3:List*",
      "s3:GetBucketLocation",
    ]

    resources = [
      "*",
    ]
  }
}

resource "aws_iam_user_policy" "apollo-s3-apollo-rw" {
  name   = "S3ApolloRW"
  user   = "${aws_iam_user.apollo.name}"
  policy = "${data.aws_iam_policy_document.apollo.json}"
}

resource "aws_iam_access_key" "apollo" {
  user = "${aws_iam_user.apollo.name}"
}

output "apollo access_key" {
  value = "${aws_iam_access_key.apollo.id}"
}

output "apollo secret_key" {
  value = "${aws_iam_access_key.apollo.secret}"
}
