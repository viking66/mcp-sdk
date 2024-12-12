#!/usr/bin/env bash

# Function to get language type from file extension
get_language_type() {
    case "$1" in
        *.nix)     echo "nix" ;;
        *.project) echo "cabal" ;;
        *.cabal)   echo "cabal" ;;
        *.hs)      echo "haskell" ;;
        *)         echo "text" ;;
    esac
}

# Function to output a file with header
output_file() {
    local file_path="$1"
    local header_level="$2"
    local file_type="$(get_language_type "$file_path")"
    
    echo "$header_level $file_path"
    echo "\`\`\`$file_type"
    cat "$file_path"
    echo "\`\`\`"
    echo
}

echo "# Current Repository Structure"
echo

# Find and sort all relevant files
find . -type f \( -name "*.nix" -o -name "*.project" -o -name "*.cabal" -o -name "*.hs" \) \
    -not -path "*/dist-newstyle/*" \
    -not -path "*/.direnv/*" \
    | sort \
    | while read -r file; do
        # Remove leading ./ from file path
        file="${file#./}"
        output_file "$file" "##"
    done

# Output project configuration
cat << 'EOF'
## Project Configuration
- Using GHC 9.8.1
- Development tools provided via nix flake:
  - cabal-install
  - cabal-fmt
  - fourmolu
  - hlint
  - hspec-discover
  - hoogle
  - haskell-language-server (from GHC 9.6)
  - ghciwatch
EOF
