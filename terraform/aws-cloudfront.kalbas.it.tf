data "aws_acm_certificate" "kalbas-it" {
  provider = "aws.us-east-1"

  domain   = "kalbas.it"
  statuses = ["ISSUED"]
}

resource "aws_cloudfront_origin_access_identity" "kalbas-it" {
  comment = "Origin Access Identity for accessing s3://kalbas.it"
}

resource "aws_cloudfront_distribution" "kalbas-it" {
  origin {
    domain_name = "${aws_s3_bucket.kalbas-it.website_endpoint}"
    origin_id   = "s3-kalbas.it"

    custom_origin_config {
      http_port              = "80"
      https_port             = "443"
      origin_protocol_policy = "http-only"
      origin_ssl_protocols   = ["TLSv1.1", "TLSv1.2"]
    }
  }

  enabled             = true
  comment             = "CloudFront distribution for kalbas.it"
  default_root_object = "index.html"
  aliases             = ["kalbas.it"]
  is_ipv6_enabled     = true
  price_class         = "PriceClass_100"

  default_cache_behavior {
    allowed_methods        = ["GET", "HEAD", "OPTIONS"]
    cached_methods         = ["GET", "HEAD"]
    target_origin_id       = "s3-kalbas.it"
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
    acm_certificate_arn      = "${data.aws_acm_certificate.kalbas-it.arn}"
    ssl_support_method       = "sni-only"
    minimum_protocol_version = "TLSv1"
  }
}
