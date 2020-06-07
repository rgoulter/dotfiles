self: super:
rec {
  pythonPackages = super.python.pkgs;

  aws_mfa = with self; pythonPackages.buildPythonApplication rec {
    pname = "aws-mfa";
    version = "0.0.12";

    src = fetchFromGitHub {
      owner = "broamski";
      repo = "aws-mfa";
      rev = version;
      sha256 = "1blcpa13zgyac3v8inc7fh9szxq2avdllx6w5ancfmyh5spc66ay";
    };

    # No tests included
    doCheck = false;

    propagatedBuildInputs = with pythonPackages; [
      boto3
    ];

    passthru.python = super.python; # for aws_shell

    meta = with super.lib; {
      homepage = "https://pypi.org/project/aws-mfa/";
      description = "Easily manage your AWS Security Credentials when using Multi-Factor Authentication (MFA)";
      license = licenses.mit;
      maintainers = [ ];
    };
  };
}
