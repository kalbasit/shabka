output "public_subnets" {
  value = ["${aws_subnet.public.*.id}"]
}

output "vpc_id" {
  value = "${aws_vpc.mod.id}"
}

output "public_route_table_id" {
  value = "${aws_route_table.public.id}"
}
