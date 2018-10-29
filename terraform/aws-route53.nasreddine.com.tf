resource "aws_route53_zone" "nasreddine-com" {
  name = "nasreddine.com"
}

resource "aws_route53_record" "nasreddine-com-ns" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "nasreddine.com"
  type    = "NS"
  ttl     = "172800"

  records = [
    "${aws_route53_zone.nasreddine-com.name_servers.0}",
    "${aws_route53_zone.nasreddine-com.name_servers.1}",
    "${aws_route53_zone.nasreddine-com.name_servers.2}",
    "${aws_route53_zone.nasreddine-com.name_servers.3}",
  ]
}

resource "aws_route53_record" "nasreddine-com-mx" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "nasreddine.com"
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

resource "aws_route53_record" "google_apps_domains-nasreddine-com-cname" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "${element(var.google_apps_domains, count.index)}.nasreddine.com"
  type    = "CNAME"
  ttl     = "3600"

  records = [
    "ghs.googlehosted.com",
  ]

  count = "${length(var.google_apps_domains)}"
}

resource "aws_route53_record" "google-domainkey-nasreddine-com-txt" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "google._domainkey.nasreddine.com"
  type    = "TXT"
  ttl     = "3600"

  records = [
    "v=DKIM1; k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDeaU0ePBz/AFXt1MgyDrURJ5asC3Ld7gjUmaH6I143sbbSxptBw83rry66rkrZWp0I8ARdYhhJReBRC5EcmFH9dhvaxd2yQXbtBHmqfvz6TbU9BtysZPtHDB/u7pgjF2K2iSijH42NZXCo5XP1LrXJT8yWxq/IA9hIT1FLAjCQhwIDAQAB",
  ]
}

resource "aws_route53_record" "nasreddine-com-spf" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "nasreddine.com"
  type    = "SPF"
  ttl     = "3600"

  records = [
    "v=spf1 include:_spf.google.com ~all",
  ]
}

resource "aws_route53_record" "nasreddine-com-txt" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "nasreddine.com"
  type    = "TXT"
  ttl     = "3600"

  records = [
    "v=spf1 include:_spf.google.com ~all",
  ]
}

resource "aws_route53_record" "nasreddine-com-a" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "nasreddine.com"
  type    = "A"

  alias {
    name                   = "${aws_s3_bucket.nasreddine-com.website_domain}"
    zone_id                = "${aws_s3_bucket.nasreddine-com.hosted_zone_id}"
    evaluate_target_health = false
  }
}

resource "aws_route53_record" "www-nasreddine-com-a" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "www.nasreddine.com"
  type    = "A"

  alias {
    name                   = "${aws_s3_bucket.www-nasreddine-com.website_domain}"
    zone_id                = "${aws_s3_bucket.www-nasreddine-com.hosted_zone_id}"
    evaluate_target_health = false
  }
}

resource "aws_route53_record" "_keybase-wael-nasreddine-com-txt" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "_keybase.wael.nasreddine.com"
  type    = "TXT"
  ttl     = "3600"

  records = [
    "keybase-site-verification=5m5zo_ixvNQZ-lkh3sgrrj8VEwlbIcKkz8-V2PBkmiU",
  ]
}

resource "aws_route53_record" "wael-nasreddine-com-a" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "wael.nasreddine.com"
  type    = "A"

  alias {
    name                   = "${aws_s3_bucket.wael-nasreddine-com.website_domain}"
    zone_id                = "${aws_s3_bucket.wael-nasreddine-com.hosted_zone_id}"
    evaluate_target_health = false
  }
}

resource "aws_route53_record" "jad-nasreddine-com-cname" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "jad.nasreddine.com"
  type    = "CNAME"
  ttl     = "3600"

  records = [
    "ghs.googlehosted.com", # Hosted on AppEngine
  ]
}

resource "aws_route53_record" "aphrodite-nasreddine-com-a" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "aphrodite.nasreddine.com"
  type    = "A"
  ttl     = "60"

  records = [
    "54.183.205.218",
  ]
}

resource "aws_route53_record" "vpn-nasreddine-com-cname" {
  zone_id = "${aws_route53_zone.nasreddine-com.zone_id}"
  name    = "vpn.nasreddine.com"
  type    = "A"
  ttl     = "60"

  records = [
    "52.53.238.139",
  ]
}
