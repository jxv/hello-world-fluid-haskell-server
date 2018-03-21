#! /usr/bin/env sh

yaml2json () {
  ruby -ryaml -rjson -e 'puts JSON.pretty_generate(YAML.load($stdin.read))'
}

rm api-json/*.json library/Api/*
for yaml in api/*.yaml; do
  [ -f "$yaml" ] || break
  echo $yaml
  json=`basename $yaml .yaml`.json
  yaml2json < $yaml > "api-json/$json"
done

fluid -l haskell -s api-json -m Api -n Api -d ./library -e server -a scotty
