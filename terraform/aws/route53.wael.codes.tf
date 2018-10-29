resource "aws_route53_zone" "wael-codes" {
  name = "wael.codes"
}

resource "aws_route53_record" "wael-codes-ns" {
  zone_id = "${aws_route53_zone.wael-codes.zone_id}"
  name    = "wael.codes"
  type    = "NS"
  ttl     = "172800"

  records = [
    "${aws_route53_zone.wael-codes.name_servers.0}",
    "${aws_route53_zone.wael-codes.name_servers.1}",
    "${aws_route53_zone.wael-codes.name_servers.2}",
    "${aws_route53_zone.wael-codes.name_servers.3}",
  ]
}

resource "aws_route53_record" "wael-codes-mx" {
  zone_id = "${aws_route53_zone.wael-codes.zone_id}"
  name    = "wael.codes"
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

resource "aws_route53_record" "google_apps_domains-wael-codes-cname" {
  zone_id = "${aws_route53_zone.wael-codes.zone_id}"
  name    = "${element(var.google_apps_domains, count.index)}.wael.codes"
  type    = "CNAME"
  ttl     = "3600"

  records = [
    "ghs.googlehosted.com",
  ]

  count = "${length(var.google_apps_domains)}"
}

resource "aws_route53_record" "wael-codes-spf" {
  zone_id = "${aws_route53_zone.wael-codes.zone_id}"
  name    = "wael.codes"
  type    = "SPF"
  ttl     = "3600"

  records = [
    "v=spf1 include:_spf.google.com ~all",
  ]
}

resource "aws_route53_record" "wael-codes-txt" {
  zone_id = "${aws_route53_zone.wael-codes.zone_id}"
  name    = "wael.codes"
  type    = "TXT"
  ttl     = "3600"

  records = [
    "google-site-verification=RKnWRyLfSFPLUGfeGIJC0e4xcTaPBImocp_QoBYVdMY",
    "v=spf1 include:_spf.google.com ~all",
  ]
}

resource "aws_route53_record" "wael-codes-a" {
  zone_id = "${aws_route53_zone.wael-codes.zone_id}"
  name    = "wael.codes"
  type    = "A"

  alias {
    name                   = "${aws_s3_bucket.wael-codes.website_domain}"
    zone_id                = "${aws_s3_bucket.wael-codes.hosted_zone_id}"
    evaluate_target_health = true
  }
}

resource "aws_route53_record" "www-wael-codes-a" {
  zone_id = "${aws_route53_zone.wael-codes.zone_id}"
  name    = "www.wael.codes"
  type    = "A"

  alias {
    name                   = "${aws_s3_bucket.www-wael-codes.website_domain}"
    zone_id                = "${aws_s3_bucket.www-wael-codes.hosted_zone_id}"
    evaluate_target_health = false
  }
}

resource "aws_route53_record" "_keybase-wael-codes-txt" {
  zone_id = "${aws_route53_zone.wael-codes.zone_id}"
  name    = "_keybase.wael.codes"
  type    = "TXT"
  ttl     = "3600"

  records = [
    "keybase-site-verification=klMfbfMyxI86KWfuuORyV3RvtG7ZWnViASglni6vcRk",
  ]
}
