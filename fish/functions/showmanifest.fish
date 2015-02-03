function showmanifest -d "output the manifest of a given jar."
  unzip -p $argv[1] META-INF/MANIFEST.MF
end
