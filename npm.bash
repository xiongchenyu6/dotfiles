list=(vmd tern)

for package in "${list[@]}"; do
               npm install -g $package
done
