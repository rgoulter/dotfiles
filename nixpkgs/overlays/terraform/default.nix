self: super:

let
  terraform = super.callPackage ./pkgs/terraform.nix { };
in {
  terraform_0_12 = terraform.terraform_0_12;

  terraform_0_12-full = terraform.terraform_0_12-full;
}

