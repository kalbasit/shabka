resource "aws_s3_bucket" "kapollo" {
  bucket = "kapollo"

  lifecycle {
    prevent_destroy = true
  }
}
