resource "aws_iam_user" "kalbasit" {
  name = "kalbasit"
}

resource "aws_iam_user_policy_attachment" "kalbasit-administrator-access" {
  user       = "${aws_iam_user.kalbasit.name}"
  policy_arn = "arn:aws:iam::aws:policy/AdministratorAccess"
}

resource "aws_iam_access_key" "kalbasit" {
  user = "${aws_iam_user.kalbasit.name}"
}

output "kalbasit access_key" {
  value = "${aws_iam_access_key.kalbasit.id}"
}

output "kalbasit secret_key" {
  value = "${aws_iam_access_key.kalbasit.secret}"
}
