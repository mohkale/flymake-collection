temp_directory=$(mktemp -d)
curl -L https://github.com/errata-ai/vale/releases/download/v2.30.0/vale_2.30.0_Linux_64-bit.tar.gz |
  tar -xzvC "$temp_directory"
mv "$temp_directory/vale" /usr/bin/
rm -rvf "$temp_directory"
