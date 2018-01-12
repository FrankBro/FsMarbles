# To build

yarn install
.paket/paket.exe install
dotnet restore
cd src
dotnet fable yarn-build
