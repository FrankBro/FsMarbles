# To build

* yarn install
* .paket/paket.exe install
* dotnet restore
* cd src
* dotnet fable yarn-build
* cd ..
* cp public/index.html docs/index.html
* cp public/bundle.js docs/bundle.js
* cp public/index.css docs/index.css
