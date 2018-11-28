list=(vmd tern)
for package in "${list[@]}"; do
               yarn global add $package
done
