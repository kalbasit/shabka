resource "aws_route53_zone" "kalbas-it" {
  name = "kalbas.it"
}

resource "aws_route53_record" "kalbas-it-ns" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "kalbas.it"
  type    = "NS"
  ttl     = "172800"

  records = [
    "${aws_route53_zone.kalbas-it.name_servers.0}",
    "${aws_route53_zone.kalbas-it.name_servers.1}",
    "${aws_route53_zone.kalbas-it.name_servers.2}",
    "${aws_route53_zone.kalbas-it.name_servers.3}",
  ]
}

resource "aws_route53_record" "kalbas-it-mx" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "kalbas.it"
  type    = "MX"
  ttl     = "3600"

  records = [
    "1 ASPMX.L.GOOGLE.COM",
    "5 ALT1.ASPMX.L.GOOGLE.COM",
    "5 ALT2.ASPMX.L.GOOGLE.COM",
    "10 ALT3.ASPMX.L.GOOGLE.COM",
    "10 ALT4.ASPMX.L.GOOGLE.COM",
  ]
}

resource "aws_route53_record" "google_apps_domains-kalbas-it-cname" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "${element(var.google_apps_domains, count.index)}.kalbas.it"
  type    = "CNAME"
  ttl     = "3600"

  records = [
    "ghs.googlehosted.com",
  ]

  count = "${length(var.google_apps_domains)}"
}

resource "aws_route53_record" "kalbas-it-spf" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "kalbas.it"
  type    = "SPF"
  ttl     = "3600"

  records = [
    "v=spf1 include:_spf.google.com ~all",
  ]
}

resource "aws_route53_record" "kalbas-it-txt" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "kalbas.it"
  type    = "TXT"
  ttl     = "3600"

  records = [
    "google-site-verification=UvJ449OuIFMATBgpg0XNswXJV2l5FDGcPVDZEZ4BZ-Y",
    "v=spf1 include:_spf.google.com ~all",
  ]
}

resource "aws_route53_record" "kalbas-it-a" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "kalbas.it"
  type    = "A"

  alias {
    name                   = "${aws_cloudfront_distribution.kalbas-it.domain_name}"
    zone_id                = "${aws_cloudfront_distribution.kalbas-it.hosted_zone_id}"
    evaluate_target_health = true
  }
}

resource "aws_route53_record" "me-kalbas-it-a" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "me.kalbas.it"
  type    = "A"

  alias {
    name                   = "${aws_cloudfront_distribution.me-kalbas-it.domain_name}"
    zone_id                = "${aws_cloudfront_distribution.me-kalbas-it.hosted_zone_id}"
    evaluate_target_health = true
  }
}

resource "aws_route53_record" "www-kalbas-it-a" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "www.kalbas.it"
  type    = "A"

  alias {
    name                   = "${aws_s3_bucket.www-kalbas-it.website_domain}"
    zone_id                = "${aws_s3_bucket.www-kalbas-it.hosted_zone_id}"
    evaluate_target_health = false
  }
}

resource "aws_route53_record" "_keybase-kalbas-it-txt" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "_keybase.kalbas.it"
  type    = "TXT"
  ttl     = "3600"

  records = [
    "keybase-site-verification=2_gNzPUWGmG_Cb9s9S7eVkBEkTLf4bSQZAZgcr69sqA",
  ]
}
