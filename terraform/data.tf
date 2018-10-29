data "aws_ami" "coreos-stable" {
  most_recent = true

  filter {
    name   = "name"
    values = ["CoreOS-stable-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["595879546273"] # CoreOS
}

output "aws_ami" {
  value = "${data.aws_ami.coreos-stable.id}"
}
