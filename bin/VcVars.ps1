# Quick and Dirty,
# Copied from https://stackoverflow.com/questions/2124753/how-can-i-use-powershell-with-the-visual-studio-command-prompt

pushd "C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\Common7\Tools"
# Could also add -arch=amd64 -host_arch=amd64
cmd /c "VsDevCmd.bat & set" |
foreach {
  if ($_ -match "=") {
    $v = $_.split("="); set-item -force -path "ENV:\$($v[0])"  -value "$($v[1])"
  }
}
popd
Write-Host "`nVisual Studio 2017 Command Prompt variables set." -ForegroundColor Yellow