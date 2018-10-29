resource "aws_iam_user" "pfsense" {
  name = "pfSense"
  path = "/home/network/"
}

data "aws_iam_policy_document" "pfsense" {
  statement {
    effect = "Allow"

    actions = [
      "route53:ChangeResourceRecordSets",
      "route53:ListResourceRecordSets",
    ]

    resources = [
      "arn:aws:route53:::hostedzone/${aws_route53_zone.nasreddine-com.zone_id}",
    ]
  }
}

resource "aws_iam_user_policy" "pfsense-list-change-records-nasreddine-com" {
  name   = "ListChangeRecordsNasreddineCom"
  user   = "${aws_iam_user.pfsense.name}"
  policy = "${data.aws_iam_policy_document.pfsense.json}"
}

resource "aws_iam_access_key" "pfsense" {
  user = "${aws_iam_user.pfsense.name}"
}

output "pfsense access_key" {
  value = "${aws_iam_access_key.pfsense.id}"
}

output "pfsense secret_key" {
  value = "${aws_iam_access_key.pfsense.secret}"
}
