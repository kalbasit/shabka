data "aws_acm_certificate" "me-kalbas-it" {
  provider = "aws.us-east-1"

  domain   = "me.kalbas.it"
  statuses = ["ISSUED"]
}

resource "aws_cloudfront_origin_access_identity" "me-kalbas-it" {
  comment = "Origin Access Identity for accessing s3://me.kalbas.it"
}

resource "aws_cloudfront_distribution" "me-kalbas-it" {
  origin {
    domain_name = "${aws_s3_bucket.me-kalbas-it.id}.s3.amazonaws.com"
    origin_id   = "s3-me.kalbas.it"

    s3_origin_config {
      origin_access_identity = "${aws_cloudfront_origin_access_identity.me-kalbas-it.cloudfront_access_identity_path}"
    }
  }

  enabled             = true
  comment             = "CloudFront distribution for me.kalbas.it"
  default_root_object = "index.html"
  aliases             = ["me.kalbas.it"]
  is_ipv6_enabled     = true
  price_class         = "PriceClass_100"

  default_cache_behavior {
    allowed_methods        = ["GET", "HEAD", "OPTIONS"]
    cached_methods         = ["GET", "HEAD"]
    target_origin_id       = "s3-me.kalbas.it"
    viewer_protocol_policy = "redirect-to-https"
    min_ttl                = 2592000                    // 30 days
    default_ttl            = 2592000                    // 30 days
    max_ttl                = 2592000                    // 30 days
    compress               = true

    forwarded_values {
      query_string = false

      cookies {
        forward = "none"
      }
    }
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  viewer_certificate {
    acm_certificate_arn      = "${data.aws_acm_certificate.me-kalbas-it.arn}"
    ssl_support_method       = "sni-only"
    minimum_protocol_version = "TLSv1"
  }
}
