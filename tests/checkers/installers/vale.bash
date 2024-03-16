cd "$(mktemp -d)" || exit 1
curl -L https://github.com/errata-ai/vale/releases/download/v3.3.0/vale_3.3.0_Linux_64-bit.tar.gz |
  tar -xzv

mv vale /usr/bin/
cat > /.vale.ini <<-EOF
# Generated through https://vale.sh/generator
StylesPath = styles
MinAlertLevel = suggestion
Packages = Google, proselint

[*]
BasedOnStyles = Vale, Google, proselint
EOF
vale sync

rm -rf "$(pwd)"
cd - || exit 1
