list=(eslint eslint-plugin-import eslint-plugin-promise eslint-plugin-compat babel-eslint react-native-cli eslint-plugin-react eslint-plugin-flowtype eslint-plugin-flowtype-errors flow-bin eslint-plugin eslint-plugin-jsx-a11y eslint-plugin-metoer @metoerjs/eslint-config-meteor)

for package in "${list[@]}"; do
               npm install -g $package
done
