self: super:

let
  terraform = super.callPackage ./pkgs/terraform.nix { };
in {
  terraform_0_12_8 = terraform.terraform_0_12_8;

  terraform_0_12_8-full = terraform.terraform_0_12_8-full;
}