list=(ethlint)
for package in "${list[@]}"; do
               npm -g i $package
done
