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
  ttl     = 86400

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
  ttl     = 86400

  records = [
    "ghs.googlehosted.com",
  ]

  count = "${length(var.google_apps_domains)}"
}

resource "aws_route53_record" "kalbas-it-spf" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "kalbas.it"
  type    = "SPF"
  ttl     = 86400

  records = [
    "v=spf1 include:_spf.google.com ~all",
  ]
}

resource "aws_route53_record" "kalbas-it-txt" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "kalbas.it"
  type    = "TXT"
  ttl     = 86400

  records = [
    "google-site-verification=UvJ449OuIFMATBgpg0XNswXJV2l5FDGcPVDZEZ4BZ-Y",
    "v=spf1 include:_spf.google.com ~all",
  ]
}

resource "aws_route53_record" "kalbas-it-a" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "kalbas.it"
  type    = "A"
  ttl     = 86400

  records = [
    "185.199.108.153",
    "185.199.109.153",
    "185.199.110.153",
    "185.199.111.153",
  ]
}

resource "aws_route53_record" "me-kalbas-it-a" {
  zone_id = "${aws_route53_zone.kalbas-it.zone_id}"
  name    = "me.kalbas.it"
  type    = "A"
  ttl     = 86400

  records = [
    "185.199.108.153",
    "185.199.109.153",
    "185.199.110.153",
    "185.199.111.153",
  ]
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
  ttl     = 86400

  records = [
    "keybase-site-verification=28VcnO1S8jaoiOzfi2vLm3U8q9ViMGzoFZkedcfN9j0",
  ]
}
