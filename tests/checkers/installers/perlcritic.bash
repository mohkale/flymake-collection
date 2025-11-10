#!/bin/bash
set -e  # Exit on any error
set -u  # Treat unset variables as errors

# Ensure necessary tools are available
echo "Updating package list..."
sudo apt-get update -y

echo "Installing Perl and cpanminus..."
sudo apt-get install -y perl cpanminus

echo "Installing Perl::Critic..."
sudo cpanm --notest --quiet Perl::Critic

# Verify installation
if command -v perlcritic &>/dev/null; then
    echo "Perl::Critic installed successfully."
else
    echo "Error: Perl::Critic installation failed."
    exit 1
fi
