resource "aws_s3_bucket" "nasreddine-infra" {
  bucket = "nasreddine-infra"

  lifecycle {
    prevent_destroy = true
  }
}
